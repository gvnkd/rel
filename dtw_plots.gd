class_name DtwPlots
extends Control

# DTW result views for one session's matches csv:
#   scatter — (i, j) alignment plot, colored by dist; diagonal = in-sync
#   dist    — per-step distance per A window (quality over time)
#   drill   — two z-normalized windows overlaid + warping-path connectors
#   pairs   — correspondence wiring: A index range on top, B on the bottom,
#             one connector per matched window (RMB-click twice to select)
# LMB click in scatter/dist emits drill_requested(i, j); RMB sets a range
# endpoint (two RMB clicks switch to the pairs view).

signal drill_requested(i: int, j: int)

const MARGIN_L := 56.0
const MARGIN_R := 12.0
const MARGIN_T := 26.0
const MARGIN_B := 22.0
const MAX_POINTS := 60000

var _matches := {}      # i -> [j, dist]
var _step := 1
var _n_win := 0
var _meta := {}         # session json {tfA, tfB, win, band, best, ...}
var _mode := "scatter"  # scatter | dist | drill
var _drill: Dictionary = {}
var _range := [-1, -1]  # correspondence range endpoints (A indices)
var _keys: Array = []   # sorted matched A indices (for nearest snapping)
var _sel_i := -1        # drilled A index (drawn marker)
var _mode_btns := {}
var _status: Label
var _hint: Label
# drill zoom: visible fractions of the x (index) and y (z-value) ranges;
# RMB drag scales like the chart (right = zoom in x, up = zoom in y,
# anchored at the cursor), RMB click resets
var _zx := [0.0, 1.0]
var _zy := [0.0, 1.0]
var _rmb_down := false
var _rmb_down_pos := Vector2.ZERO

func _ready() -> void:
	var bar := HBoxContainer.new()
	bar.set_anchors_preset(Control.PRESET_TOP_WIDE)
	add_child(bar)
	for m in ["scatter", "dist", "drill", "pairs"]:
		var b := Button.new()
		b.text = m
		b.toggle_mode = true
		b.button_pressed = m == _mode
		b.pressed.connect(_set_mode.bind(m))
		b.disabled = m == "drill" or m == "pairs"
		bar.add_child(b)
		_mode_btns[m] = b
	_status = Label.new()
	_status.size_flags_horizontal = SIZE_EXPAND_FILL
	_status.horizontal_alignment = HORIZONTAL_ALIGNMENT_RIGHT
	bar.add_child(_status)
	_hint = Label.new()
	_hint.text = "LMB: drill into nearest match · RMB twice: select correspondence range"
	_hint.add_theme_font_size_override("font_size", 11)
	_hint.set_anchors_preset(Control.PRESET_BOTTOM_WIDE)
	_hint.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	add_child(_hint)
	custom_minimum_size = Vector2(420, 300)

func _set_mode(m: String) -> void:
	_mode = m
	for k in _mode_btns:
		(_mode_btns[k] as Button).button_pressed = k == m
	match m:
		"drill":
			_hint.text = "drill: RMB drag zooms (right = x, up = y), RMB click resets · mode buttons switch views"
		"pairs":
			_hint.text = "pairs: A index rail on top, B match rail on bottom · RMB twice in scatter/dist selects a new range"
		_:
			_hint.text = "LMB: drill into nearest match · RMB twice: select correspondence range"
	queue_redraw()

# load a session's matches; meta = the session json. Prefers the RELM
# binary (parse-free), falls back to the csv. Returns match count.
func show_run(dir: String, base: String, meta: Dictionary) -> int:
	_matches.clear()
	_drill = {}
	_range = [-1, -1]
	_meta = meta
	_n_win = int(meta.get("windows", 0))
	_step = 1
	var bin_path := "%s/%s.bin" % [dir, base]
	var keys: Array = []
	if FileAccess.file_exists(bin_path):
		var bf := FileAccess.open(bin_path, FileAccess.READ)
		if bf != null:
			keys = _load_bin(bf)
	if keys.is_empty():
		keys = _load_csv("%s/%s.csv" % [dir, base])
	if keys.is_empty():
		_status.text = "no matches data"
		return 0
	keys.sort()
	_keys = keys
	_sel_i = -1
	for k in range(1, keys.size()):
		var d: int = keys[k] - keys[k - 1]
		if d > 0 and (_step == 1 or d < _step):
			_step = d
	_set_mode("scatter")
	_status.text = "%s: %d matches, win %d, best %.6f" % [
		base, _matches.size(), int(meta.get("win", 0)),
		(meta.get("best", {}) as Dictionary).get("dist", 0.0)]
	queue_redraw()
	return _matches.size()

# RELM: u32 magic, u32 ver, u32 win, u32 band, u32 step, u64 count, then
# count x 16B (i u32, j i32, dist f64). FileAccess lacks decode helpers —
# raw little-endian byte math (verified against the Haskell writer).
func _load_bin(bf: FileAccess) -> Array:
	var head := bf.get_buffer(28)
	if head.size() < 28:
		return []
	if head.decode_u32(0) != 0x4D4C4552 or head.decode_u32(4) != 1:
		return []
	var n := head.decode_u64(20)
	var keys: Array = []
	for k in n:
		var rec := bf.get_buffer(16)
		if rec.size() < 16:
			break
		var i := rec.decode_u32(0)
		_matches[i] = [float(rec.decode_s32(4)), rec.decode_double(8)]
		keys.append(i)
	return keys

func _load_csv(matches_path: String) -> Array:
	var keys: Array = []
	var f := FileAccess.open(matches_path, FileAccess.READ)
	if f == null:
		return keys
	f.get_line()
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 3:
			continue
		var i := parts[0].to_int()
		_matches[i] = [parts[1].to_float(), parts[2].to_float()]
		keys.append(i)
	return keys

# meta of the session currently shown (session json); used for drill params
func session_meta() -> Dictionary:
	return _meta

func set_drill(d: Dictionary) -> void:
	if d.has("error"):
		_status.text = "drill-down: " + str(d["error"])
		return
	_drill = d
	_range = [-1, -1]
	_zx = [0.0, 1.0]
	_zy = [0.0, 1.0]
	(_mode_btns["drill"] as Button).disabled = false
	_set_mode("drill")
	_status.text = "drill: dist %.6f, path %d steps — RMB drag zooms, RMB click resets" % [d.get("dist", 0.0), (d.get("path", []) as Array).size()]

func _plot_rect() -> Rect2:
	return Rect2(MARGIN_L, MARGIN_T, size.x - MARGIN_L - MARGIN_R, size.y - MARGIN_T - MARGIN_B)

func _draw() -> void:
	var r := _plot_rect()
	if r.size.x < 20 or r.size.y < 20:
		return
	match _mode:
		"scatter":
			_draw_scatter(r)
		"dist":
			_draw_dist(r)
		"drill":
			_draw_drill(r)
		"pairs":
			_draw_pairs(r)
	_draw_markers(r)

# selection + pending-range markers shared by scatter/dist
func _draw_markers(r: Rect2) -> void:
	if _mode != "scatter" and _mode != "dist":
		return
	var i_max: float = _n_win * _step
	if i_max <= 0:
		return
	var px: float = r.position.x + r.size.x * _sel_i / i_max
	if _sel_i >= 0:
		draw_line(Vector2(px, r.position.y), Vector2(px, r.end.y), Color(0.9, 0.9, 1.0, 0.7), 1.0, true)
	if _range[0] >= 0 and _range[1] < 0:
		var pr: float = r.position.x + r.size.x * _range[0] / i_max
		draw_line(Vector2(pr, r.position.y), Vector2(pr, r.end.y), Color(1.0, 0.7, 0.3, 0.8), 1.0, true)

# --- scatter ---------------------------------------------------------------

func _draw_scatter(r: Rect2) -> void:
	if _matches.is_empty():
		return
	var keys := _matches.keys()
	keys.sort()
	var j0: float = _matches[keys[0]][0]
	var j1 := j0
	for k in keys:
		var jv: float = _matches[k][0]
		j0 = minf(j0, jv)
		j1 = maxf(j1, jv)
	# y range covers both the data and the diagonal reference
	var i_max: float = maxf(_n_win * _step, keys[keys.size() - 1])
	j1 = maxf(j1, i_max)
	var px := func(k: float) -> float: return r.position.x + r.size.x * k / maxf(1.0, i_max)
	var py := func(v: float) -> float: return r.end.y - r.size.y * (v - j0) / maxf(1.0, j1 - j0)
	# diagonal reference j = i (meaningful when tfA == tfB)
	draw_line(Vector2(px.call(0.0), py.call(0.0)), Vector2(px.call(i_max), py.call(i_max)),
		Color(0.4, 0.4, 0.4, 0.6), 1.0, true)
	var stride := maxi(1, keys.size() / MAX_POINTS)
	var w := maxf(1.0, r.size.x / 800.0)
	for n in range(0, keys.size(), stride):
		var k: int = keys[n]
		var jd: Array = _matches[k]
		var t: float = clampf(jd[1] / 1.5, 0.0, 1.0)
		var col := Color(0.1 + 0.9 * t, 0.85 - 0.7 * t, 0.25 - 0.2 * t)
		draw_rect(Rect2(Vector2(px.call(k) - w * 0.5, py.call(jd[0]) - w * 0.5), Vector2(w, w)), col)
	_draw_axes(r, "A window i", "B match j (0 .. %.0f)" % j1)

# --- dist ------------------------------------------------------------------

func _draw_dist(r: Rect2) -> void:
	if _matches.is_empty():
		return
	var keys := _matches.keys()
	keys.sort()
	var dmax := 0.0
	for k in keys:
		dmax = maxf(dmax, _matches[k][1])
	dmax = maxf(dmax, 0.1)
	var i_max: float = maxf(_n_win * _step, keys[keys.size() - 1])
	var col := Color(0.85, 0.7, 0.2)
	# envelope: min/max dist per pixel column
	var cols := maxi(1, int(r.size.x))
	var lo := PackedFloat64Array()
	var hi := PackedFloat64Array()
	lo.resize(cols)
	hi.resize(cols)
	var have := PackedByteArray()
	have.resize(cols)
	for n in keys.size():
		var k: int = keys[n]
		var c := clampi(int(r.size.x * k / maxf(1.0, i_max)), 0, cols - 1)
		var d: float = _matches[k][1]
		if have[c]:
			lo[c] = minf(lo[c], d)
			hi[c] = maxf(hi[c], d)
		else:
			lo[c] = d
			hi[c] = d
			have[c] = 1
	var x0 := r.position.x
	var y_base := r.end.y
	for c in cols:
		if not have[c]:
			continue
		var xa := x0 + c
		draw_line(Vector2(xa, y_base - r.size.y * lo[c] / dmax),
			Vector2(xa, y_base - r.size.y * hi[c] / dmax), col)
	_draw_axes(r, "A window i", "dist (0 .. %.3g)" % dmax)

# --- drill -----------------------------------------------------------------

func _draw_drill(r: Rect2) -> void:
	if not _drill.has("a"):
		return
	var a: Array = _drill["a"]
	var b: Array = _drill["b"]
	var path: Array = _drill.get("path", [])
	var m := a.size()
	if m < 2:
		return
	# b is the win+2band segment; the path's max r tells the drawn extent
	var r_max := 0
	for p in path:
		r_max = maxi(r_max, int(p[1]))
	var n := maxi(m, r_max + 1)
	var lo := 0.0
	var hi := 0.0
	for v in a:
		lo = minf(lo, v)
		hi = maxf(hi, v)
	for k in n:
		var v: float = b[k] if k < b.size() else 0.0
		lo = minf(lo, v)
		hi = maxf(hi, v)
	var pad := maxf(0.5, (hi - lo) * 0.08)
	lo -= pad
	hi += pad
	# zoom windows: fractions of the index range and of the value range
	var x_lo: float = _zx[0] * (n - 1)
	var x_hi := maxf(_zx[1] * (n - 1), x_lo + 1.0)
	var v_span := maxf(1e-9, hi - lo)
	var y_lo: float = lo + _zy[0] * v_span
	var y_hi := maxf(lo + _zy[1] * v_span, y_lo + 1e-6)
	var px := func(x: float) -> float: return r.position.x + r.size.x * (x - x_lo) / (x_hi - x_lo)
	var py := func(v: float) -> float: return r.end.y - r.size.y * (v - y_lo) / (y_hi - y_lo)
	# warping connectors (thin, dim)
	var stride := maxi(1, path.size() / 96)
	for k in range(0, path.size(), stride):
		var p: Array = path[k]
		var ri: int = p[1]
		if ri < b.size():
			draw_line(Vector2(px.call(p[0]), py.call(a[p[0]])),
				Vector2(px.call(ri), py.call(b[ri])), Color(1, 1, 1, 0.10))
	# the two normalized windows
	var pa := PackedVector2Array()
	for k in m:
		pa.append(Vector2(px.call(k), py.call(a[k])))
	draw_polyline(pa, Color(0.4, 0.75, 1.0), 1.6, true)
	var pb := PackedVector2Array()
	for k in n:
		if k < b.size():
			pb.append(Vector2(px.call(k), py.call(b[k])))
	draw_polyline(pb, Color(1.0, 0.8, 0.25), 1.6, true)
	_draw_axes(r, "query (blue) vs match (amber)", "z-score (%.2f .. %.2f)" % [lo, hi])

# --- pairs (correspondence wiring) ------------------------------------------

func _draw_pairs(r: Rect2) -> void:
	var ra: int = _range[0]
	var rb: int = _range[1]
	if ra < 0 or rb <= ra:
		return
	var keys := _matches.keys()
	keys.sort()
	var in_range: Array = []
	var j0 := 0.0
	var j1 := 0.0
	for k in keys:
		if k >= ra and k <= rb:
			in_range.append(k)
			var jv: float = _matches[k][0]
			if in_range.size() == 1:
				j0 = jv
				j1 = jv
			j0 = minf(j0, jv)
			j1 = maxf(j1, jv)
	if in_range.is_empty():
		return
	var y_top := r.position.y + 6.0
	var y_bot := r.end.y - 6.0
	var px_a := func(i: float) -> float: return r.position.x + r.size.x * (i - ra) / maxf(1.0, rb - ra)
	var px_b := func(j: float) -> float: return r.position.x + r.size.x * (j - j0) / maxf(1.0, j1 - j0)
	# rails
	draw_line(Vector2(r.position.x, y_top), Vector2(r.end.x, y_top), Color(0.6, 0.6, 0.7), 2.0)
	draw_line(Vector2(r.position.x, y_bot), Vector2(r.end.x, y_bot), Color(0.6, 0.6, 0.7), 2.0)
	var stride := maxi(1, in_range.size() / 400)
	for n in range(0, in_range.size(), stride):
		var k: int = in_range[n]
		var jd: Array = _matches[k]
		var t: float = clampf(jd[1] / 1.5, 0.0, 1.0)
		var col := Color(0.15 + 0.8 * t, 0.85 - 0.65 * t, 0.3 - 0.22 * t, 0.75)
		draw_line(Vector2(px_a.call(k), y_top), Vector2(px_b.call(jd[0]), y_bot), col, 1.0, true)
	var fnt := get_theme_default_font()
	draw_string(fnt, Vector2(r.position.x, y_top - 4), "A %d..%d" % [ra, rb], HORIZONTAL_ALIGNMENT_LEFT, -1, 11, Color(0.7, 0.7, 0.7))
	draw_string(fnt, Vector2(r.position.x, y_bot + 15), "B %.0f..%.0f (%d matched)" % [j0, j1, in_range.size()], HORIZONTAL_ALIGNMENT_LEFT, -1, 11, Color(0.7, 0.7, 0.7))

# --- axes ------------------------------------------------------------------

func _draw_axes(r: Rect2, xlabel: String, ylabel: String) -> void:
	var fnt := get_theme_default_font()
	var fs := 11
	var c := Color(0.6, 0.6, 0.6)
	draw_rect(r, Color(0.25, 0.25, 0.25), false, 1.0)
	draw_string(fnt, Vector2(r.position.x, r.position.y - 8), ylabel, HORIZONTAL_ALIGNMENT_LEFT, -1, fs, c)
	draw_string(fnt, Vector2(r.end.x, r.end.y + 16), xlabel, HORIZONTAL_ALIGNMENT_RIGHT, -1, fs, c)

# --- input -----------------------------------------------------------------

func _gui_input(ev: InputEvent) -> void:
	if ev is InputEventMouseButton:
		var button := ev as InputEventMouseButton
		if button.pressed:
			if button.button_index == MOUSE_BUTTON_RIGHT and _mode == "drill":
				_rmb_down = true
				_rmb_down_pos = button.position
				return
			if _matches.is_empty() or _mode == "drill" or _mode == "pairs":
				return
			var r := _plot_rect()
			if not r.has_point(button.position):
				return
			var i_max: float = _n_win * _step
			if i_max <= 0:
				return
			var k := int((button.position.x - r.position.x) / r.size.x * i_max)
			var i := clampi(k, 0, int(i_max))
			if button.button_index == MOUSE_BUTTON_LEFT:
				# always drill the NEAREST matched window — an exact-grid
				# lookup made most clicks dead ends
				var ni := _nearest_key(i)
				if ni >= 0:
					_sel_i = ni
					drill_requested.emit(ni, int(_matches[ni][0]))
					_status.text = "drill at i=%d (j=%d)" % [ni, int(_matches[ni][0])]
					queue_redraw()
			elif button.button_index == MOUSE_BUTTON_RIGHT:
				# two RMB clicks select the correspondence range
				if _range[0] < 0 or _range[1] >= 0:
					_range = [i, -1]
					_status.text = "range start i=%d (RMB again to end)" % i
				else:
					_range = [mini(_range[0], i), maxi(_range[0], i)]
					(_mode_btns["pairs"] as Button).disabled = false
					_set_mode("pairs")
		else:
			if button.button_index == MOUSE_BUTTON_RIGHT and _rmb_down:
				# RMB click (no drag) in drill resets the zoom
				if _mode == "drill" and button.position.distance_to(_rmb_down_pos) < 4.0:
					_zx = [0.0, 1.0]
					_zy = [0.0, 1.0]
					queue_redraw()
				_rmb_down = false
	elif ev is InputEventMouseMotion:
		var motion := ev as InputEventMouseMotion
		if _rmb_down and _mode == "drill" \
				and motion.button_mask & MOUSE_BUTTON_MASK_RIGHT != 0:
			_drill_scale_by(motion.relative, motion.position)

# binary search: nearest matched A index to i (-1 when empty)
func _nearest_key(i: int) -> int:
	if _keys.is_empty():
		return -1
	var lo := 0
	var hi := _keys.size() - 1
	while lo < hi:
		var mid := (lo + hi) / 2
		if int(_keys[mid]) < i:
			lo = mid + 1
		else:
			hi = mid
	if lo > 0 and i - int(_keys[lo - 1]) < int(_keys[lo]) - i:
		return int(_keys[lo - 1])
	return int(_keys[lo])

# chart-like zoom: drag right zooms in x, drag up zooms in y, cursor-anchored
func _drill_scale_by(delta: Vector2, pos: Vector2) -> void:
	var r := _plot_rect()
	if not r.has_point(pos):
		return
	var cx := clampf((pos.x - r.position.x) / r.size.x, 0.0, 1.0)
	var cy := clampf((pos.y - r.position.y) / r.size.y, 0.0, 1.0)
	_zoom_axis(_zx, cx, pow(1.01, -delta.x), 1.0 / 4096.0)
	_zoom_axis(_zy, cy, pow(1.01, delta.y), 1.0 / 4096.0)
	queue_redraw()

# shrink/expand the [lo,hi] fraction window by factor around the anchored
# fraction c (c measured within the old window, so the cursor point stays)
func _zoom_axis(win: Array, c: float, factor: float, min_span: float) -> void:
	var lo: float = win[0]
	var hi: float = win[1]
	var span := maxf(hi - lo, min_span)
	var anchor := lo + c * span
	var new_span := clampf(span * factor, min_span, 1.0)
	var new_lo := clampf(anchor - c * new_span, 0.0, 1.0 - new_span)
	win[0] = new_lo
	win[1] = new_lo + new_span
