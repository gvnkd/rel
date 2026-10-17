class_name IndicatorWidget
extends Control

class Indicator:
	var kind := "SMA"
	var enabled := true
	var period := 20
	var start := 0
	var end := 0
	var predict := 50
	var waves := 0
	var edge_w := 1.0
	var m := 5
	# MKV: rolling window (bars) for BOTH the BSBP histogram and the
	# classification window-sum; rolling surprise window (bars); the
	# bull/bear threshold in percent applies to the SUM of the last
	# `period` returns ending at each bar (partial while the window
	# fills). auto_thresh derives the threshold as auto_mult x the median
	# |window sum| of the loaded slice
	var surp := 20
	var thresh := 5.0
	var auto_thresh := false
	# auto-threshold multiplier on the median |window sum| (3 = the
	# original fit for per-bar returns; window sums are near-Gaussian and
	# want 4-9x — tune live)
	var auto_mult := 3.0
	var locked := false
	var opt_score := -1.0
	# OBVOL: price bucket size in display units; 0 = exact price levels.
	# Volumes resting at prices closer than this merge into one line
	var bucket := 0.0
	# OBVOL source mode: 0 = lifetimes sidecar (true placement→dissolution),
	# 1 = per-candle book snapshots (top book-depth levels only — a level
	# leaving the ladder window reads as dissolved)
	var obvol_mode := 0
	# per-series result caches (keyed by series instance id in `caches`) —
	# indicator math is computed per series, so one shared vals/fit field
	# would let the last-drawn series overwrite the others
	var caches := {}

class IndCache:
	var s: Object = null
	var vals := PackedFloat64Array()
	var vals_off := 0
	var vals2 := PackedFloat64Array()
	var fit := PackedFloat64Array()
	var forecast := PackedFloat64Array()
	# OBVOL: flat segment list, 5 doubles each (side, price, i0, i1, alpha)
	var segs := PackedFloat64Array()
	var cache_key := ""
	# MKV: per-bar classification states (0 Bu / 1 SW / 2 Be) of the whole
	# loaded slice — exact window sums, never slice-truncated
	var states := PackedInt32Array()
	var states_key := ""

const FFT_MIN_WINDOW := 16
const HANDLE_TOL := 5.0

# order-book volume levels (sim tick-mode series only): alpha quantized
# into buckets, capped so candles underneath stay readable
const OBVOL_PRICE_SCALE := 100000.0
const OBVOL_ALPHA_Q := 16
const OBVOL_MAX_ALPHA := 0.6
const OBVOL_BID_COL := Color(0.30, 0.80, 0.45)
const OBVOL_ASK_COL := Color(0.90, 0.40, 0.35)
# group-key side bit: key = side * OBVOL_SIDE_BIT + price key (bucket
# index or 5-decimal price); price keys stay well under 2^40
const OBVOL_SIDE_BIT := 1099511627776
# cache range padding: the cached segment set covers the visible range
# plus a margin rounded to whole blocks, so pans/zooms inside the margin
# reuse it instead of rebuilding per frame
const OBVOL_PAD := 512

# MKV fixed model parameters (mirror Sim.Markov.defaultMarkovParams) —
# adaptive-memory decay ceiling/floor, surprise reference (log-loss units)
# and shrink rate; the threshold IS UI-exposed (Indicator.thresh) or
# auto-calibrated as `auto_mult` x the sampled median |close-to-close
# return| in percent — 3x reproduced the grid-searched optima on sim1
# (1H: 0.99 vs 1.0, Daily: 4.95 vs 5.0); drift sums want more (see the
# Indicator.auto_mult field)
const MKV_LAM_HI := 0.99
const MKV_LAM_LO := 0.90
const MKV_SREF := 0.55
const MKV_K := 4.0

const PERIODS: Array[int] = [5, 10, 20, 50, 100, 200]
const IND_COLORS: Array[Color] = [
	Color(0.95, 0.9, 0.4),
	Color(0.5, 0.85, 1.0),
	Color(1.0, 0.55, 0.75),
	Color(0.65, 1.0, 0.65),
	Color(0.85, 0.7, 1.0),
	Color(1.0, 0.75, 0.45),
]

var view: ChartView
var data: PlotData
var enabled := true
var indicators: Array[Indicator] = []
var vol_pane_h := 0.0
var _sim: Object = null

func _ensure_sim() -> bool:
	if _sim != null:
		return true
	if not ClassDB.class_exists("SimNative"):
		return false
	_sim = ClassDB.instantiate("SimNative")
	return _sim != null

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE
	if indicators.is_empty():
		add_indicator("SMA", 20)

func add_indicator(kind: String, period: int) -> void:
	var ind := Indicator.new()
	ind.kind = kind
	ind.period = period
	indicators.append(ind)

# per-series cache entry; the instance-id key is validated against the
# stored reference because ids can be reused after a series is freed
func _cache_for(ind: Indicator, s: PlotData.Series) -> IndCache:
	var key := s.get_instance_id()
	var c: IndCache = ind.caches.get(key)
	if c == null or c.s != s:
		c = IndCache.new()
		c.s = s
		ind.caches[key] = c
	return c

func remove_indicator(idx: int) -> void:
	indicators.remove_at(idx)
	if data == null:
		return
	for s in data.series:
		var remapped := {}
		for key in s.ind_colors:
			var ki: int = key
			if ki != idx:
				remapped[ki - (1 if ki > idx else 0)] = s.ind_colors[key]
		s.ind_colors = remapped

func color_for(s: PlotData.Series, idx: int) -> Color:
	var override: Variant = s.ind_colors.get(idx)
	if override != null:
		return override
	var si := data.series.find(s)
	return IND_COLORS[(idx + maxi(si, 0)) % IND_COLORS.size()]

# plot rect excluding the volume pane at the bottom — the 0..1 overlays
# (KS/TEMP/CECP) map into this so the volume widget (drawn on top) can't
# cover them
func overlay_rect() -> Rect2:
	var r := view.plot_rect
	return Rect2(r.position, Vector2(r.size.x, maxf(1.0, r.size.y - vol_pane_h)))

func _draw() -> void:
	if not enabled or view == null or data == null:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	for k in indicators.size():
		var ind: Indicator = indicators[k]
		if not ind.enabled:
			continue
		if ind.kind == "FFT":
			_draw_fft_handles(ind)
		for s in data.series:
			if not s.visible:
				continue
			if ind.kind == "FFT":
				_draw_fft(ind, s, k)
				continue
			if ind.kind == "KS":
				_draw_ks(ind, s, k)
				continue
			if ind.kind == "TEMP":
				_draw_ks(ind, s, k, true)
				continue
			if ind.kind == "CECP":
				_draw_cecp(ind, s, k)
				continue
			if ind.kind == "MKV":
				_draw_mkv(ind, s, k)
				continue
			if ind.kind == "OBVOL":
				_draw_obvol(ind, s, k)
				continue
			if s.size() < ind.period:
				continue
			var col := color_for(s, k)
			var last := mini(i1, s.size() - 1)
			if last < i0:
				continue
			var pts := PackedVector2Array()
			var win_lo := maxi(0, i0 - ind.period + 1)
			var sum := 0.0
			for j in range(win_lo, i0):
				sum += s.candle_at(j).close
			for i in range(i0, last + 1):
				sum += s.candle_at(i).close
				if i - ind.period >= win_lo:
					sum -= s.candle_at(i - ind.period).close
				if i >= win_lo + ind.period - 1:
					pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0, view.price_to_y(sum / ind.period)))
			if pts.size() >= 2:
				draw_polyline(pts, col, 1.5)

func _draw_fft(ind: Indicator, s: PlotData.Series, k: int) -> void:
	var n: int = s.size()
	if n < FFT_MIN_WINDOW:
		return
	var w := fft_window(ind, n)
	var e_start := w.x
	var e_end := w.y
	if e_end - e_start < FFT_MIN_WINDOW or ind.predict < 1:
		return
	if not _fft_ensure(ind, s):
		return
	var c := _cache_for(ind, s)
	var col := color_for(s, k)
	if c.fit.size() >= 2:
		var fpts := PackedVector2Array()
		for i in c.fit.size():
			fpts.append(Vector2(view.index_to_x(e_start + i) + view.px_per_index / 2.0, view.price_to_y(c.fit[i])))
		draw_polyline(fpts, col.lightened(0.55), 1.5)
	if c.forecast.is_empty():
		return
	var pts := PackedVector2Array()
	# anchor the prediction at the fit's last point so the two curves meet
	var anchor := c.forecast[0]
	if c.fit.size() >= 1:
		anchor = c.fit[c.fit.size() - 1]
	pts.append(Vector2(view.index_to_x(e_end - 1) + view.px_per_index / 2.0, view.price_to_y(anchor)))
	for j in c.forecast.size():
		pts.append(Vector2(view.index_to_x(e_end + j) + view.px_per_index / 2.0, view.price_to_y(c.forecast[j])))
	for j in range(pts.size() - 1):
		draw_dashed_line(pts[j], pts[j + 1], col, 1.5, 4.0)

# rolling KS entropy (permutation entropy, normalized to 0..1) drawn as a
# line over the full plot height: 1 = noise-like, near 0 = trending.
# TEMP draws the same series inverted (1 - PE = the market "temperature":
# structural order, low when the system crystallizes/herds).
# Computed over the visible range plus a warm-up margin ahead of it (the
# FFI gets a closes slice, not the whole series); the per-series cache
# covers [vals_off, vals_off + vals.size()) and its key carries the store
# window offset so a refill recomputes.
func _draw_ks(ind: Indicator, s: PlotData.Series, k: int, invert := false) -> void:
	var n: int = s.size()
	if n < 2:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := mini(int(ceil(ir.y)), n - 1)
	if not _ks_ensure(ind, s, i0, i1):
		return
	var c := _cache_for(ind, s)
	var r := overlay_rect()
	var col := color_for(s, k)
	var pts := PackedVector2Array()
	for i in range(i0, i1 + 1):
		if i < c.vals_off or i - c.vals_off >= c.vals.size():
			continue
		var v: float = c.vals[i - c.vals_off]
		if is_nan(v):
			continue
		if invert:
			v = 1.0 - v
		pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0, r.end.y - v * r.size.y))
	if pts.size() >= 2:
		draw_polyline(pts, col, 1.5)

# Complexity-Entropy Causality Plane: the rolling (H, C) pairs mapped into
# the plot rect as a phase plane (x = entropy, y = complexity, both
# 0..1), not a time series. Draws the max-complexity boundary curve for
# k = m! ordinal patterns plus the window's trajectory, older points more
# transparent: periodic lives bottom-left, noise bottom-right, structured
# data rides the ridge.
func _draw_cecp(ind: Indicator, s: PlotData.Series, k: int) -> void:
	var n: int = s.size()
	if n < 2:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := mini(int(ceil(ir.y)), n - 1)
	if not _cecp_ensure(ind, s, i0, i1):
		return
	var c := _cache_for(ind, s)
	var r := overlay_rect()
	var col := color_for(s, k)
	_draw_cecp_curve(ind.m, r)
	var span := maxi(1, i1 - i0)
	var prev := Vector2(-1, -1)
	for i in range(i0, i1 + 1):
		if i < c.vals_off or i - c.vals_off >= c.vals.size():
			continue
		var h: float = c.vals[i - c.vals_off]
		var cc: float = c.vals2[i - c.vals_off]
		if is_nan(h) or is_nan(cc):
			continue
		var age := float(i - i0) / span
		var p := Vector2(r.position.x + clampf(h, 0.0, 1.0) * r.size.x, r.end.y - clampf(cc, 0.0, 1.0) * r.size.y)
		if prev.x >= 0.0:
			draw_line(prev, p, Color(col, 0.08 + 0.2 * age), 1.0)
		prev = p
		var dot := Color(col, 0.15 + 0.85 * age)
		draw_circle(p, 3.5 if i == i1 else 2.0, dot)
	var font := ThemeDB.fallback_font
	var dim := Color(1, 1, 1, 0.35)
	draw_string(font, r.position + Vector2(4, 13), "periodic", HORIZONTAL_ALIGNMENT_LEFT, 80.0, 10, dim)
	draw_string(font, Vector2(r.end.x - 44, r.position.y + 13), "random", HORIZONTAL_ALIGNMENT_LEFT, 80.0, 10, dim)
	draw_string(font, Vector2(r.position.x + r.size.x * 0.5 - 20, r.position.y + 13), "chaotic", HORIZONTAL_ALIGNMENT_LEFT, 80.0, 10, dim)

# Bull-Sideways-Bear Markov indicator: the transition matrix itself is
# rendered as a 3x3 panel under the cursor tableau (cursor_widget), here
# only the rolling surprise (mean log-loss of the previous bar's estimate
# vs reality, normalized by the uniform-model loss ln 3) is drawn — a
# translucent bottom band; a high surprise means the Markov assumption
# just broke (regime change), it also drives the adaptive decay (lambda)
func _draw_mkv(ind: Indicator, s: PlotData.Series, k: int) -> void:
	var n: int = s.size()
	if n < 2:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := mini(int(ceil(ir.y)), n - 1)
	if not _mkv_ensure(ind, s, i0, i1):
		return
	var c := _cache_for(ind, s)
	var r := overlay_rect()
	var ln3 := log(3.0)
	for i in range(i0, i1 + 1):
		if i < c.vals_off or i - c.vals_off >= c.vals.size() / 15:
			continue
		var sv: float = c.vals[(i - c.vals_off) * 15 + 13]
		if is_nan(sv):
			continue
		var t := clampf(sv / ln3, 0.0, 1.0)
		if t <= 0.0:
			continue
		var h := t * r.size.y * 0.25
		draw_rect(Rect2(view.index_to_x(i), r.end.y - h, maxf(view.px_per_index, 1.0), h), Color(1.0, 0.45, 0.6, 0.10 + 0.22 * t))

# max-complexity boundary of the CECP: the parametric curve for
# distributions {p, (1-p)/(k-1), ...} (the ridge's upper envelope)
func _draw_cecp_curve(m: int, r: Rect2) -> void:
	var k := 1
	for j in range(2, m + 1):
		k *= j
	if k < 2:
		return
	var lk := log(k)
	var jmax := log(2.0) + lk / 2.0 - (k + 1.0) / (2.0 * k) * log(k + 1.0)
	var pts := PackedVector2Array()
	for j in range(129):
		var p := lerpf(1.0 / k, 1.0, float(j) / 128.0)
		var q := (1.0 - p) / (k - 1)
		var hm := (p + 1.0 / k) / 2.0
		var hq := (q + 1.0 / k) / 2.0
		var h := (-p * log(p) - (1.0 - p) * log(q)) / lk
		var jsd := -hm * log(hm) - (k - 1.0) * hq * log(hq) - (-p * log(p) - (1.0 - p) * log(q) + lk) / 2.0
		pts.append(Vector2(r.position.x + clampf(h, 0.0, 1.0) * r.size.x, r.end.y - clampf(h * jsd / jmax, 0.0, 1.0) * r.size.y))
	draw_polyline(pts, Color(1, 1, 1, 0.25), 1.0, true)

# order-book volume levels: one horizontal segment per continuous (side,
# price) level lifetime from the levels sidecar — placement candle to
# full dissolution, no matter how far price wanders from the top of the
# ladder; opacity = peak volume relative to the range max. Levels use
# fixed bid/ask hues — the per-series indicator color would collapse the
# two sides into one.
func _draw_obvol(ind: Indicator, s: PlotData.Series, k: int) -> void:
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := mini(int(ceil(ir.y)), s.size() - 1)
	if i1 < i0 or not _obvol_ensure(ind, s, i0, i1):
		return
	var c := _cache_for(ind, s)
	var r := view.plot_rect
	var n := c.segs.size() / 5
	for t in n:
		if c.segs[t * 5 + 3] < i0 or c.segs[t * 5 + 2] > i1:
			continue
		var y := view.price_to_y(c.segs[t * 5 + 1])
		if y < r.position.y or y > r.end.y:
			continue
		# runs span candle center to candle center (the convention other
		# indicator lines use); a single-candle run draws its full cell so
		# one-candle pulses stay visible
		var hw := view.px_per_index / 2.0
		var x0 := view.index_to_x(int(c.segs[t * 5 + 2])) + hw
		var x1 := view.index_to_x(int(c.segs[t * 5 + 3])) + hw
		if x1 <= x0:
			x0 -= hw
			x1 += hw
		x0 = maxf(x0, r.position.x)
		x1 = minf(x1, r.end.x)
		if x1 <= x0:
			continue
		var col: Color = OBVOL_BID_COL if int(c.segs[t * 5]) == 0 else OBVOL_ASK_COL
		col.a = c.segs[t * 5 + 4] * OBVOL_MAX_ALPHA
		draw_line(Vector2(x0, y), Vector2(x1, y), col, 1.0 + 2.5 * c.segs[t * 5 + 4])

# fills the per-series segment cache for [i0, i1] when its key is stale.
# The cache covers a block-rounded padded range so minor view changes
# reuse it; false when the mode's data is unavailable (non-tick series,
# real quotes)
func _obvol_ensure(ind: Indicator, s: PlotData.Series, i0: int, i1: int) -> bool:
	var n: int = s.size()
	if n < 1 or i1 < i0:
		return false
	var blk := OBVOL_PAD
	var lo := maxi(0, i0 / blk * blk - blk)
	var hi := mini(n - 1, i1 / blk * blk + blk)
	var c := _cache_for(ind, s)
	var key := "%d:%d:%d:%s:%d:%d:%f:%d" % [data.series.find(s), n, s.win_off, s.tf, lo, hi, ind.bucket, ind.obvol_mode]
	if key == c.cache_key:
		return not c.segs.is_empty()
	if ind.obvol_mode == 1:
		return _obvol_ensure_snap(ind, s, lo, hi, c, key)
	return _obvol_ensure_life(ind, s, lo, hi, c, key)

# lifetime mode: source is the levels sidecar (Sim.Lvl) — one row per
# continuous level lifetime, so a resting level stays drawn until fully
# lifetime mode: source is the levels sidecar (Sim.Lvl) — one row per
# continuous level lifetime, so a resting level stays drawn until fully
# consumed no matter how far price wanders from the top of the ladder.
# Each lifetime's volume-change events (rests bump, partial fills reduce)
# are read from the .vev binary sidecar and the line is split into runs
# per volume level: opacity AND width track the resting volume over
# time. Old exports without events fall back to one constant-alpha piece
# per merged lifetime. Single pass over the flat rows with hash grouping
# (no global sort)
func _obvol_ensure_life(ind: Indicator, s: PlotData.Series, lo: int, hi: int, c: IndCache, key: String) -> bool:
	if s.levels.is_empty():
		c.segs = PackedFloat64Array()
		c.cache_key = key
		return false
	# level rows are stored in base (1m) candle indices; map them to the
	# current timeframe like PlotData.Series.book_at does
	var g := 1
	if s.tf != "1m":
		g = QuoteStore.TF_MINUTES[s.tf]
	# group rows by (side, bucket): integer hash key, no sort; members
	# are [c0, c1, ev0, evn, px, peak] in the grouped timeframe
	var groups := {}
	for j in range(0, s.levels.size(), 7):
		var c0g: int = int(s.levels[j]) / g
		var c1g: int = int(s.levels[j + 1]) / g
		if c1g < lo or c0g > hi:
			continue
		var side := int(s.levels[j + 2])
		var px := s.levels[j + 3]
		var pk: int
		if ind.bucket > 0.0:
			# epsilon guards float representation at bucket boundaries
			# (1.2 / 0.0001 = 11999.999... would floor one bucket low)
			pk = int(floor(px / ind.bucket + 1e-6))
			px = pk * ind.bucket
		else:
			pk = int(round(px * OBVOL_PRICE_SCALE))
		var gk := OBVOL_SIDE_BIT * side + pk
		var arr: Variant = groups.get(gk)
		if arr == null:
			arr = []
			groups[gk] = arr
		arr.append([c0g, c1g, int(s.levels[j + 5]), int(s.levels[j + 6]), px, s.levels[j + 4]])
	if groups.is_empty():
		c.segs = PackedFloat64Array()
		c.cache_key = key
		return false
	var vf: FileAccess = null
	if s.levels_path != "" and FileAccess.file_exists(s.levels_path):
		vf = FileAccess.open(s.levels_path, FileAccess.READ)
	# render pieces: [side, px, a, b, vol]
	var pieces: Array = []
	for gk in groups:
		var arr: Array = groups[gk]
		var side: float = 1.0 if gk >= OBVOL_SIDE_BIT else 0.0
		var events: Array = []
		var has_ev := vf != null
		if has_ev:
			for mi in arr.size():
				var evs := _vev_read(vf, int(arr[mi][2]), int(arr[mi][3]))
				if evs.is_empty() and int(arr[mi][3]) > 0:
					has_ev = false
					break
				for e in evs:
					events.append([int(e[0]) / g, e[1], mi])
		if has_ev and not events.is_empty():
			events.sort()
			# combined volume of the bucket over its span: walk the
			# merged event stream; a new run starts whenever the summed
			# volume changes (quantized into alpha buckets at emit)
			var g0: int = arr[0][0]
			var g1: int = arr[0][1]
			for m in arr:
				g0 = mini(g0, int(m[0]))
				g1 = maxi(g1, int(m[1]))
			var last: Array = []
			last.resize(arr.size())
			for k in last.size():
				last[k] = 0.0
			var prev_comb := -1.0
			var run_a := g0
			for e in events:
				last[e[2]] = e[1]
				var comb := 0.0
				for v in last:
					comb += v
				if prev_comb < 0.0:
					prev_comb = comb
					run_a = int(e[0])
				elif comb != prev_comb:
					if prev_comb > 0.0 and int(e[0]) - 1 >= run_a:
						pieces.append([side, float(arr[0][4]), run_a, int(e[0]) - 1, prev_comb])
					prev_comb = comb
					run_a = int(e[0])
			if prev_comb > 0.0 and g1 >= run_a:
				pieces.append([side, float(arr[0][4]), run_a, g1, prev_comb])
		else:
			# no events (old export or missing sidecar): merge spans with
			# summed peaks into one constant-density piece each
			if arr.size() > 1:
				arr.sort()
			var merged: Array = []
			var cur: Array = []
			for e in arr:
				if not cur.is_empty() and int(e[0]) <= int(cur[1]) + 1:
					cur[1] = maxi(int(cur[1]), int(e[1]))
					cur[5] = float(cur[5]) + float(e[5])
				else:
					cur = e
					merged.append(cur)
			for m in merged:
				pieces.append([side, float(m[4]), int(m[0]), int(m[1]), float(m[5])])
	if vf != null:
		vf.close()
	if pieces.is_empty():
		c.segs = PackedFloat64Array()
		c.cache_key = key
		return false
	var vmax := 0.0
	for p in pieces:
		vmax = maxf(vmax, float(p[4]))
	if vmax <= 0.0:
		c.segs = PackedFloat64Array()
		c.cache_key = key
		return false
	var segs := PackedFloat64Array()
	for p in pieces:
		var aq := clampi(int(ceili(float(p[4]) / vmax * OBVOL_ALPHA_Q)), 1, OBVOL_ALPHA_Q)
		segs.append_array([float(p[0]), float(p[1]), float(p[2]), float(p[3]), float(aq) / OBVOL_ALPHA_Q])
	c.segs = segs
	c.cache_key = key
	return not c.segs.is_empty()

# read evn 20-byte records (candle u32, side u32, price u32, volume u64)
# starting at record ev0 of the .vev sidecar; returns [candle, vol] pairs
func _vev_read(vf: FileAccess, ev0: int, evn: int) -> Array:
	var out: Array = []
	if vf == null or evn <= 0:
		return out
	vf.seek(16 + ev0 * 20)
	var buf := vf.get_buffer(evn * 20)
	if buf.size() < evn * 20:
		return out
	var sp := StreamPeerBuffer.new()
	sp.big_endian = false
	sp.data_array = buf
	for i in evn:
		var ec := sp.get_u32()
		sp.get_u32()
		sp.get_u32()
		var ev := sp.get_u64()
		out.append([ec, ev])
	return out

# snapshot mode (the original OBVOL): level presence derived from the
# per-candle book snapshots — a level draws from the first snapshot it
# appears in until the first it is missing from. Only the top book-depth
# levels exist in the sidecar, so a level that price walks away from
# drops out while still in the book
func _obvol_ensure_snap(ind: Indicator, s: PlotData.Series, lo: int, hi: int, c: IndCache, key: String) -> bool:
	if s.books.is_empty():
		c.segs = PackedFloat64Array()
		c.cache_key = key
		return false
	var vmax := 0.0
	for i in range(lo, hi + 1):
		var book: Variant = s.book_at(i)
		if book == null:
			continue
		for lv: Vector2 in book["bids"]:
			vmax = maxf(vmax, lv.y)
		for lv: Vector2 in book["asks"]:
			vmax = maxf(vmax, lv.y)
	if vmax <= 0.0:
		c.segs = PackedFloat64Array()
		c.cache_key = key
		return false
	var pieces: Array = []
	var open := {}
	var seen := {}
	for i in range(lo, hi + 1):
		var book: Variant = s.book_at(i)
		seen.clear()
		if book != null:
			for side in 2:
				var lvls: Array = book["bids"] if side == 0 else book["asks"]
				for lv: Vector2 in lvls:
					var px := lv.x
					var pk: int
					if ind.bucket > 0.0:
						pk = int(floor(px / ind.bucket + 1e-6))
						px = pk * ind.bucket
					else:
						pk = int(round(px * OBVOL_PRICE_SCALE))
					var gk := OBVOL_SIDE_BIT * side + pk
					seen[gk] = true
					if open.has(gk):
						var o: Array = open[gk]
						if int(o[1]) == i - 1:
							# contiguous presence: extend, the run keeps the
							# largest volume seen (alpha at emit)
							o[1] = i
							o[2] = maxf(float(o[2]), lv.y)
							continue
						_obvol_close(pieces, gk, o)
					open[gk] = [i, i, lv.y, px]
		for gk: int in open.keys():
			if not seen.has(gk):
				_obvol_close(pieces, gk, open[gk])
				open.erase(gk)
	for gk: int in open.keys():
		_obvol_close(pieces, gk, open[gk])
	var segs := PackedFloat64Array()
	for p in pieces:
		var aq := clampi(int(ceili(float(p[4]) / vmax * OBVOL_ALPHA_Q)), 1, OBVOL_ALPHA_Q)
		segs.append_array([float(p[0]), float(p[1]), float(p[2]), float(p[3]), float(aq) / OBVOL_ALPHA_Q])
	c.segs = segs
	c.cache_key = key
	return not c.segs.is_empty()

# append a snapshot-mode run: [side, px, a, b, maxvol]
func _obvol_close(pieces: Array, gk: int, o: Array) -> void:
	pieces.append([1.0 if gk >= OBVOL_SIDE_BIT else 0.0, float(o[3]), int(o[0]), int(o[1]), float(o[2])])

# fills the per-series cache for [lo, i1] when its key is stale; false
# when the value line cannot be computed (missing FFI, short series)
func _ks_ensure(ind: Indicator, s: PlotData.Series, i0: int, i1: int) -> bool:
	var n: int = s.size()
	if n < 2:
		return false
	var lo := maxi(0, i0 - ind.period - 2)
	var c := _cache_for(ind, s)
	var key := "%d:%d:%d:%d:%d:%d:%d" % [data.series.find(s), ind.period, ind.m, n, s.win_off, lo, i1]
	if key == c.cache_key:
		return not c.vals.is_empty()
	if not _ensure_sim():
		return false
	var closes := PackedFloat64Array()
	closes.resize(i1 - lo + 1)
	for i in range(lo, i1 + 1):
		closes[i - lo] = s.candle_at(i).close
	var ret: Variant = _sim.call("perm_entropy", closes, ind.period, ind.m)
	var vals: PackedFloat64Array = ret as PackedFloat64Array
	if vals.size() == i1 - lo + 1:
		c.vals = vals
		c.vals_off = lo
	else:
		c.vals = PackedFloat64Array()
	c.cache_key = key
	return not c.vals.is_empty()

# fills the per-series CECP cache for [lo, i1]; the FFI returns (H, C)
# interleaved, split here into vals/vals2
func _cecp_ensure(ind: Indicator, s: PlotData.Series, i0: int, i1: int) -> bool:
	var n: int = s.size()
	if n < 2:
		return false
	var lo := maxi(0, i0 - ind.period - 2)
	var c := _cache_for(ind, s)
	var key := "%d:%d:%d:%d:%d:%d:%d" % [data.series.find(s), ind.period, ind.m, n, s.win_off, lo, i1]
	if key == c.cache_key:
		return not c.vals.is_empty()
	if not _ensure_sim():
		return false
	var closes := PackedFloat64Array()
	closes.resize(i1 - lo + 1)
	for i in range(lo, i1 + 1):
		closes[i - lo] = s.candle_at(i).close
	var ret: Variant = _sim.call("cecp", closes, ind.period, ind.m)
	var all: PackedFloat64Array = ret as PackedFloat64Array
	if all.size() == 2 * (i1 - lo + 1):
		c.vals.resize(i1 - lo + 1)
		c.vals2.resize(i1 - lo + 1)
		for j in range(i1 - lo + 1):
			c.vals[j] = all[2 * j]
			c.vals2[j] = all[2 * j + 1]
		c.vals_off = lo
	else:
		c.vals = PackedFloat64Array()
	c.cache_key = key
	return not c.vals.is_empty()

# fills the per-series MKV cache for [lo, i1]; the FFI returns the flat
# 15-double per-bar record (BSBP x3, matrix row-major x9, surprise,
# rolling surprise, lambda) stored whole in vals, one record per candle
# from vals_off. The matrix/surprise chain is path-dependent: it restarts
# from the uniform prior at the slice start, so the key carries win_off
# fills the per-series STATE cache for the whole loaded slice: one state
# per bar of [win_off, win_off + candles) from the window sums (partial
# at the slice edges only). Computed once per (period, thresh, slice), so
# classification is EXACT for any subslice — it never restarts at a
# cache-fill boundary. The FFI consumes these states; only the
# count/surprise chain restarts at the slice start (path-dependent, the
# cache key carries win_off)
func _mkv_states_ensure(ind: Indicator, s: PlotData.Series, th: float) -> bool:
	var base := s.win_off
	var hi := base + s.candles.size()
	var c := _cache_for(ind, s)
	var key := "%d:%d:%f:%d:%d" % [data.series.find(s), ind.period, th, hi, base]
	if key == c.states_key:
		return c.states.size() == hi - base
	if hi - base < 2:
		c.states = PackedInt32Array()
		c.states_key = key
		return false
	var st := PackedInt32Array()
	st.resize(hi - base)
	st[0] = 1
	var ssum := 0.0
	var k := base + 1
	var j := 1
	while k < hi:
		ssum += (s.candle_at(k).close / s.candle_at(k - 1).close - 1.0) * 100.0
		# keep only the last `period` returns in the window
		if j > ind.period:
			ssum -= (s.candle_at(k - ind.period).close / s.candle_at(k - ind.period - 1).close - 1.0) * 100.0
		st[j] = 0 if ssum >= th else (2 if ssum < -th else 1)
		k += 1
		j += 1
	c.states = st
	c.states_key = key
	return true

# fills the per-series MKV cache for [lo, i1]; the FFI returns the flat
# 15-double per-bar record (BSBP x3, matrix row-major x9, surprise,
# rolling surprise, lambda) stored whole in vals, one record per candle
# from vals_off. lo clamps to win_off (the states cover the whole slice,
# exact classification for any subslice); the BSBP window is
# slice-relative, so the first `period` bars of the slice read NaN — the
# margin in lo keeps them outside [i0, i1]
func _mkv_ensure(ind: Indicator, s: PlotData.Series, i0: int, i1: int) -> bool:
	var n: int = s.size()
	if n < 2:
		return false
	var lo := maxi(s.win_off, i0 - ind.period - 2)
	var th := ind.thresh
	if ind.auto_thresh:
		var ta := _mkv_auto_thresh(ind, s)
		if ta > 0.0:
			th = ta
	# note: %d rejects booleans in Redot's format — cast the flag
	var c := _cache_for(ind, s)
	var key := "%d:%d:%d:%d:%f:%f:%d:%d:%d:%d" % [data.series.find(s), ind.period, ind.surp, int(ind.auto_thresh), th, ind.auto_mult, n, s.win_off, lo, i1]
	if key == c.cache_key:
		return not c.vals.is_empty()
	if not _ensure_sim():
		return false
	if not _mkv_states_ensure(ind, s, th):
		return false
	var off := lo - s.win_off
	var st := PackedInt32Array()
	st.resize(i1 - lo + 1)
	for j in range(i1 - lo + 1):
		st[j] = c.states[off + j]
	var ret: Variant = _sim.call("markov", st, ind.period, ind.surp, MKV_LAM_HI, MKV_LAM_LO, MKV_SREF, MKV_K)
	var vals: PackedFloat64Array = ret as PackedFloat64Array
	if vals.size() == 15 * (i1 - lo + 1):
		c.vals = vals
		c.vals_off = lo
	else:
		c.vals = PackedFloat64Array()
	c.cache_key = key
	return not c.vals.is_empty()

# auto threshold: ind.auto_mult x the median |window sum| in percent over
# up to 4096 evenly spaced samples of the loaded slice (median, not mean:
# spikes inflate the mean; the sample series is the same quantity the
# classifier compares against the threshold). Returns -1 when the slice
# is too short, so the caller keeps the manual threshold
func _mkv_auto_thresh(ind: Indicator, s: PlotData.Series) -> float:
	var lo := maxi(1, s.win_off + 1)
	var hi := s.win_off + s.candles.size()
	if hi - lo < 8:
		return -1.0
	var step := maxi(1, (hi - lo) / 4096)
	var samples: Array[float] = []
	var k := lo
	while k < hi:
		# full window only (window [k-period+1 .. k], all returns
		# defined): partial sums are a different scale
		if k - ind.period >= s.win_off:
			var ssum := 0.0
			for j in range(k - ind.period + 1, k + 1):
				ssum += (s.candle_at(j).close / s.candle_at(j - 1).close - 1.0) * 100.0
			samples.append(absf(ssum))
		k += step
	if samples.size() < 8:
		return -1.0
	samples.sort()
	var med: float = samples[samples.size() / 2]
	return maxf(ind.auto_mult * med, 0.0001)

# fills the per-series cache (fit/forecast) when its key is stale; false
# when the fit cannot be computed
func _fft_ensure(ind: Indicator, s: PlotData.Series) -> bool:
	var n: int = s.size()
	if n < FFT_MIN_WINDOW:
		return false
	var w := fft_window(ind, n)
	var e_start := w.x
	var e_end := w.y
	if e_end - e_start < FFT_MIN_WINDOW or ind.predict < 1:
		return false
	var c := _cache_for(ind, s)
	var key := "%d:%d:%d:%d:%d:%f:%d:%d" % [data.series.find(s), e_start, e_end, ind.predict, ind.waves, ind.edge_w, n, s.win_off]
	if key == c.cache_key:
		return not c.fit.is_empty()
	# the store window must cover the whole FFT range (chart._refresh
	# extends it there; bail instead of reading out of bounds)
	if s.store != null and (e_start < s.win_off or e_end > s.win_off + s.candles.size()):
		return false
	if not _ensure_sim():
		return false
	var closes := PackedFloat64Array()
	closes.resize(e_end - e_start)
	for i in range(e_end - e_start):
		closes[i] = s.candle_at(e_start + i).close
	var ret: Variant = _sim.call("fft_forecast", closes, 0, closes.size(), ind.predict, ind.waves, ind.edge_w)
	var all: PackedFloat64Array = ret as PackedFloat64Array
	if all.size() != closes.size() + ind.predict:
		c.fit = PackedFloat64Array()
		c.forecast = PackedFloat64Array()
	else:
		c.fit = all.slice(0, closes.size())
		c.forecast = all.slice(closes.size())
	c.cache_key = key
	return not c.fit.is_empty()

# indicator value for a series at candle index i, or null when undefined
# there (SMA warm-up, KS NaN, FFT outside its fit/forecast range)
func value_at(ind: Indicator, s: PlotData.Series, i: int) -> Variant:
	if s.size() == 0 or i < 0 or i >= s.size():
		return null
	if ind.kind == "SMA":
		if i < ind.period - 1:
			return null
		if s.store != null and (i - ind.period + 1 < s.win_off or i >= s.win_off + s.candles.size()):
			return null
		var sum := 0.0
		for j in range(i - ind.period + 1, i + 1):
			sum += s.candle_at(j).close
		return sum / ind.period
	if ind.kind == "KS":
		var c := _cache_for(ind, s)
		if i < c.vals_off or i >= c.vals_off + c.vals.size():
			if not _ks_ensure(ind, s, i, i):
				return null
		var v: float = c.vals[i - c.vals_off]
		return null if is_nan(v) else v
	if ind.kind == "TEMP":
		var c := _cache_for(ind, s)
		if i < c.vals_off or i >= c.vals_off + c.vals.size():
			if not _ks_ensure(ind, s, i, i):
				return null
		var v: float = c.vals[i - c.vals_off]
		return null if is_nan(v) else 1.0 - v
	if ind.kind == "CECP":
		var c := _cache_for(ind, s)
		if i < c.vals_off or i >= c.vals_off + c.vals.size():
			if not _cecp_ensure(ind, s, i, i):
				return null
		var h: float = c.vals[i - c.vals_off]
		var cc: float = c.vals2[i - c.vals_off]
		if is_nan(h) or is_nan(cc):
			return null
		return [h, cc]
	if ind.kind == "MKV":
		var c := _cache_for(ind, s)
		if i < 2 or i < c.vals_off or i - c.vals_off >= c.vals.size() / 15:
			if not _mkv_ensure(ind, s, i, i):
				return null
		var o := (i - c.vals_off) * 15
		var rec := []
		rec.resize(13)
		# BSBP block (record 0..2) passes through even while NaN (window
		# warm-up) — the panel skips the line; the matrix below decides
		# null
		for j in 3:
			rec[j] = c.vals[o + j]
		for j in 9:
			var tv: float = c.vals[o + 3 + j]
			if is_nan(tv):
				return null
			rec[3 + j] = tv
		rec[12] = c.vals[o + 13]
		return rec
	if ind.kind == "OBVOL":
		var book: Variant = s.book_at(i)
		if book == null:
			return null
		var b := 0.0
		var a := 0.0
		for lv: Vector2 in book["bids"]:
			b += lv.y
		for lv: Vector2 in book["asks"]:
			a += lv.y
		return [b, a]
	if ind.kind == "FFT":
		var n: int = s.size()
		if n < FFT_MIN_WINDOW:
			return null
		var w := fft_window(ind, n)
		if i < w.x:
			return null
		if not _fft_ensure(ind, s):
			return null
		var fc := _cache_for(ind, s)
		if i < w.y:
			return fc.fit[i - w.x] if i - w.x < fc.fit.size() else null
		return fc.forecast[i - w.y] if i - w.y < fc.forecast.size() else null
	return null

# resolves the FFT window against a series length; end 0 (or past the
# end) follows the series tail, negative start falls back to end - 256;
# index 0 is a valid concrete start
func fft_window(ind: Indicator, n: int) -> Vector2i:
	var e_end := ind.end if ind.end > 0 else n
	e_end = mini(e_end, n)
	var e_start := ind.start if ind.start >= 0 else maxi(0, e_end - 256)
	e_start = clampi(e_start, 0, maxi(e_end - FFT_MIN_WINDOW, 0))
	return Vector2i(e_start, e_end)

func _handle_x(ind: Indicator, field: int, n: int) -> float:
	var w := fft_window(ind, n)
	var i := w.x if field == 0 else w.y - 1
	return view.index_to_x(i) + view.px_per_index / 2.0

func _draw_fft_handles(ind: Indicator) -> void:
	var n := data.max_len()
	if n < 1 or view == null:
		return
	var w := fft_window(ind, n)
	var r := view.plot_rect
	var font := ThemeDB.fallback_font
	for field in 2:
		var x := _handle_x(ind, field, n)
		var i := w.x if field == 0 else w.y
		var label := "s:%d" % i if field == 0 else "e:%d" % i
		draw_dashed_line(Vector2(x, r.position.y), Vector2(x, r.end.y), Color(1, 1, 1, 0.35), 1.0, 4.0)
		draw_string(font, Vector2(x + 4.0, r.position.y + 13.0), label, HORIZONTAL_ALIGNMENT_LEFT, 80.0, 12, Color(1, 1, 1, 0.6))

# hit test for the draggable FFT window handles: returns
# [indicator idx, field] or []
func fft_handle_at(pos: Vector2) -> Array:
	if not enabled or view == null:
		return []
	var r := view.plot_rect
	if pos.y < r.position.y or pos.y > r.end.y:
		return []
	for k in indicators.size():
		var ind: Indicator = indicators[k]
		if not ind.enabled or ind.kind != "FFT":
			continue
		for field in 2:
			if absf(pos.x - _handle_x(ind, field, data.max_len())) <= HANDLE_TOL:
				return [k, field]
	return []
