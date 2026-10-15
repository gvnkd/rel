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
	var locked := false
	var opt_score := -1.0
	# per-series result caches (keyed by series instance id in `caches`) —
	# indicator math is computed per series, so one shared vals/fit field
	# would let the last-drawn series overwrite the others
	var caches := {}

class IndCache:
	var s: Object = null
	var vals := PackedFloat64Array()
	var vals_off := 0
	var fit := PackedFloat64Array()
	var forecast := PackedFloat64Array()
	var cache_key := ""

const FFT_MIN_WINDOW := 16
const HANDLE_TOL := 5.0

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
# Computed over the visible range plus a warm-up margin ahead of it (the
# FFI gets a closes slice, not the whole series); the per-series cache
# covers [vals_off, vals_off + vals.size()) and its key carries the store
# window offset so a refill recomputes.
func _draw_ks(ind: Indicator, s: PlotData.Series, k: int) -> void:
	var n: int = s.size()
	if n < 2:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := mini(int(ceil(ir.y)), n - 1)
	if not _ks_ensure(ind, s, i0, i1):
		return
	var c := _cache_for(ind, s)
	var r := view.plot_rect
	var col := color_for(s, k)
	var pts := PackedVector2Array()
	for i in range(i0, i1 + 1):
		if i < c.vals_off or i - c.vals_off >= c.vals.size():
			continue
		var v: float = c.vals[i - c.vals_off]
		if is_nan(v):
			continue
		pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0, r.end.y - v * r.size.y))
	if pts.size() >= 2:
		draw_polyline(pts, col, 1.5)

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
