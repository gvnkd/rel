class_name StratPlots
extends Control

# the strategy optimizer page's own plot widgets — nothing here touches the
# chart:
# - top-left: val-Calmar-per-candidate curves for every session
# - bottom-left: the selected session's test walk — price with position
#   shading (green long / red short / gray flat) and the equity curve
# - right: per-session metric bars (calmar) with pnl/slot labels

const COLORS := [
	Color(0.95, 0.65, 0.25), Color(0.35, 0.75, 0.95), Color(0.55, 0.9, 0.45),
	Color(0.85, 0.5, 0.9), Color(0.95, 0.5, 0.55), Color(0.6, 0.85, 0.85),
]
const BG := Color(0.12, 0.12, 0.14)
const GRID := Color(1, 1, 1, 0.12)
const TXT := Color(0.75, 0.75, 0.78)
const LONG_C := Color(0.3, 0.85, 0.4, 0.13)
const SHORT_C := Color(0.95, 0.35, 0.35, 0.13)
const FLAT_C := Color(0.6, 0.6, 0.65, 0.10)

var _run_dir := ""
var _sessions := {}   # sess_seed -> {cand: PackedFloat64Array, met: Dictionary}
var _order: Array = []  # seeds in insertion order
var _selected := -1
var _close := PackedFloat64Array()
var _equity := PackedFloat64Array()
var _pos := PackedInt32Array()

func _ready() -> void:
	custom_minimum_size = Vector2(420, 320)
	mouse_filter = MOUSE_FILTER_IGNORE

func clear() -> void:
	_run_dir = ""
	_sessions = {}
	_order = []
	_selected = -1
	_close = PackedFloat64Array()
	_equity = PackedFloat64Array()
	_pos = PackedInt32Array()
	queue_redraw()

# called once after a batch finishes: results maps sess_seed -> metrics dict
func show_run(out_dir: String, results: Dictionary) -> void:
	_run_dir = out_dir
	_sessions = {}
	_order = []
	for sess_seed in results:
		_order.append(sess_seed)
		_sessions[sess_seed] = {"cand": _load_cand(sess_seed), "met": results[sess_seed]}
	_selected = _best_seed()
	_load_test(_selected)
	queue_redraw()

func select(sess_seed: int) -> void:
	if not _sessions.has(sess_seed):
		return
	_selected = sess_seed
	_load_test(sess_seed)
	queue_redraw()

func _best_seed() -> int:
	var best := -INF
	var best_seed := -1
	for sess_seed in _sessions:
		var calmar: float = (_sessions[sess_seed]["met"] as Dictionary).get("calmar", 0.0)
		if calmar > best:
			best = calmar
			best_seed = sess_seed
	return best_seed

func _load_cand(sess_seed: int) -> PackedFloat64Array:
	var out := PackedFloat64Array()
	var f := FileAccess.open("%s/cand_%d.csv" % [_run_dir, sess_seed], FileAccess.READ)
	if f == null:
		return out
	f.get_line()  # header
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() == 2:
			out.append(parts[1].to_float())
	f.close()
	return out

func _load_test(sess_seed: int) -> void:
	_close = PackedFloat64Array()
	_equity = PackedFloat64Array()
	_pos = PackedInt32Array()
	var f := FileAccess.open("%s/test_%d.csv" % [_run_dir, sess_seed], FileAccess.READ)
	if f == null:
		return
	f.get_line()  # header
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() == 3:
			_close.append(parts[0].to_float())
			_equity.append(parts[1].to_float())
			_pos.append(parts[2].to_int())
	f.close()

# --- drawing ---------------------------------------------------------------

func _draw() -> void:
	var r := Rect2(Vector2.ZERO, size)
	draw_rect(r, BG, true)
	var font := get_theme_default_font()
	if _sessions.is_empty():
		draw_string(font, Vector2(16, 24), "optimize a batch to see plots", HORIZONTAL_ALIGNMENT_LEFT, -1, 13, TXT)
		return
	var pad := 8.0
	var label_w := 52.0
	# right column: session bars
	var bars_r := Rect2(r.position + Vector2(r.size.x * 0.62, pad),
		Vector2(r.size.x * 0.38 - pad, r.size.y - 2 * pad))
	_draw_session_bars(bars_r, font)
	# left column: candidate history (top) + test walk (bottom)
	var left_w := r.size.x * 0.62 - 2 * pad
	var hist_h := r.size.y * 0.30
	var hist_r := Rect2(r.position + Vector2(pad, pad), Vector2(left_w, hist_h))
	_draw_history(hist_r, font)
	var walk_h := r.size.y - hist_h - 3 * pad
	var walk_r := Rect2(r.position + Vector2(pad, 2 * pad + hist_h), Vector2(left_w, walk_h))
	_draw_test_walk(walk_r, font, label_w)

func _frame(rr: Rect2) -> void:
	draw_rect(rr, GRID, false, 1.0)

func _draw_history(rr: Rect2, font: Font) -> void:
	_frame(rr)
	draw_string(font, rr.position + Vector2(4, 12), "val calmar / candidate", HORIZONTAL_ALIGNMENT_LEFT, -1, 11, TXT)
	var cands := 1
	for sess_seed in _order:
		cands = maxi(cands, (_sessions[sess_seed]["cand"] as PackedFloat64Array).size())
	var lo := INF
	var hi := -INF
	for sess_seed in _order:
		for v in _sessions[sess_seed]["cand"]:
			lo = minf(lo, v)
			hi = maxf(hi, v)
	if not is_finite(lo):
		return
	if hi - lo < 1e-12:
		hi = lo + 1.0
	var plot := rr.grow(-4.0)
	var ci := 0
	for sess_seed in _order:
		var cand: PackedFloat64Array = _sessions[sess_seed]["cand"]
		if cand.size() < 2:
			ci += 1
			continue
		var col: Color = COLORS[ci % COLORS.size()]
		if sess_seed == _selected:
			col.a = 1.0
		else:
			col.a = 0.45
		var pts := PackedVector2Array()
		pts.resize(cand.size())
		for i in cand.size():
			var x := plot.position.x + plot.size.x * float(i) / maxf(1.0, float(cands - 1))
			var y := plot.end.y - plot.size.y * (cand[i] - lo) / (hi - lo)
			pts[i] = Vector2(x, y)
		draw_polyline(pts, col, 1.5 if sess_seed == _selected else 1.0)
		ci += 1
	draw_string(font, plot.position + Vector2(0, plot.size.y + 11), "%d candidates   y %.4f..%.4f" % [cands, lo, hi], HORIZONTAL_ALIGNMENT_LEFT, -1, 10, TXT)

func _draw_test_walk(rr: Rect2, font: Font, label_w: float) -> void:
	if _selected < 0 or _close.is_empty():
		draw_string(font, rr.position + Vector2(4, 14), "no test data for the selected session", HORIZONTAL_ALIGNMENT_LEFT, -1, 11, TXT)
		return
	# two stacked sub-plots: price with position shading, then equity
	var price_r := Rect2(rr.position, Vector2(rr.size.x, rr.size.y * 0.55))
	var eq_r := Rect2(rr.position + Vector2(0, rr.size.y * 0.58), Vector2(rr.size.x, rr.size.y * 0.42))
	_draw_price(price_r, font, label_w)
	_draw_equity(eq_r, font, label_w)

func _x_at(plot: Rect2, i: int, n: int) -> float:
	return plot.position.x + plot.size.x * float(i) / maxf(1.0, float(n))

func _draw_price(rr: Rect2, font: Font, label_w: float) -> void:
	_frame(rr)
	var plot := Rect2(rr.position + Vector2(label_w, 12), rr.size - Vector2(label_w + 4, 16))
	var n := _close.size()
	var lo := INF
	var hi := -INF
	for c in _close:
		lo = minf(lo, c)
		hi = maxf(hi, c)
	if hi - lo < 1e-12:
		hi = lo + 1.0
	var bw := plot.size.x / float(n)
	for i in n:
		var col: Color = LONG_C if _pos[i] == 1 else (SHORT_C if _pos[i] == -1 else FLAT_C)
		draw_rect(Rect2(Vector2(_x_at(plot, i, n), plot.position.y), Vector2(bw + 0.5, plot.size.y)), col, true)
	var pts := PackedVector2Array()
	pts.resize(n)
	for i in n:
		pts[i] = Vector2(_x_at(plot, i, n),
			plot.end.y - plot.size.y * (_close[i] - lo) / (hi - lo))
	draw_polyline(pts, Color(0.92, 0.92, 0.95), 1.2)
	draw_string(font, rr.position + Vector2(4, 12), "price #%d" % _selected, HORIZONTAL_ALIGNMENT_LEFT, -1, 11, TXT)
	draw_string(font, plot.position + Vector2(2, 9), "%.5f" % hi, HORIZONTAL_ALIGNMENT_LEFT, -1, 9, TXT)
	draw_string(font, Vector2(plot.position.x + 2, plot.end.y - 2), "%.5f" % lo, HORIZONTAL_ALIGNMENT_LEFT, -1, 9, TXT)

func _draw_equity(rr: Rect2, font: Font, label_w: float) -> void:
	_frame(rr)
	var plot := Rect2(rr.position + Vector2(label_w, 4), rr.size - Vector2(label_w + 4, 8))
	var n := _equity.size()
	var lo := INF
	var hi := -INF
	for v in _equity:
		lo = minf(lo, v)
		hi = maxf(hi, v)
	if hi - lo < 1e-12:
		hi = lo + 1.0
	var pts := PackedVector2Array()
	pts.resize(n)
	for i in n:
		pts[i] = Vector2(_x_at(plot, i, n),
			plot.end.y - plot.size.y * (_equity[i] - lo) / (hi - lo))
	var col := Color(0.4, 0.9, 0.55) if _equity[n - 1] >= 0 else Color(0.95, 0.45, 0.45)
	draw_polyline(pts, col, 1.2)
	draw_string(font, rr.position + Vector2(4, 12), "equity", HORIZONTAL_ALIGNMENT_LEFT, -1, 11, TXT)
	draw_string(font, plot.position + Vector2(2, 9), "%.5f" % hi, HORIZONTAL_ALIGNMENT_LEFT, -1, 9, TXT)
	draw_string(font, Vector2(plot.position.x + 2, plot.end.y - 2), "%.5f" % lo, HORIZONTAL_ALIGNMENT_LEFT, -1, 9, TXT)

func _draw_session_bars(rr: Rect2, font: Font) -> void:
	_frame(rr)
	draw_string(font, rr.position + Vector2(4, 12), "sessions", HORIZONTAL_ALIGNMENT_LEFT, -1, 11, TXT)
	var rows := _order.size()
	if rows == 0:
		return
	var bar_h := mini(26.0, (rr.size.y - 24.0) / float(rows))
	var max_abs := 0.0
	for sess_seed in _order:
		max_abs = maxf(max_abs, absf((_sessions[sess_seed]["met"] as Dictionary).get("calmar", 0.0)))
	max_abs = maxf(max_abs, 1e-9)
	var mid := rr.position.x + 4 + (rr.size.x - 60) / 2.0
	var y := rr.position.y + 20.0
	for sess_seed in _order:
		var met: Dictionary = _sessions[sess_seed]["met"]
		var calmar: float = met.get("calmar", 0.0)
		var bl := absf(calmar) / max_abs * (rr.size.x - 60) / 2.0
		var col: Color = COLORS[_order.find(sess_seed) % COLORS.size()]
		if sess_seed != _selected:
			col.a = 0.55
		var bc := Color(0.4, 0.9, 0.55) if calmar >= 0 else Color(0.95, 0.45, 0.45)
		if sess_seed == _selected:
			bc = bc.lightened(0.2)
		draw_rect(Rect2(Vector2(mid if calmar >= 0 else mid - bl, y), Vector2(maxi(bl, 1.0), bar_h - 6)), bc, true)
		draw_string(font, Vector2(rr.position.x + 4, y + bar_h - 10), "#%d" % sess_seed, HORIZONTAL_ALIGNMENT_LEFT, -1, 10, col)
		draw_string(font, Vector2(rr.position.x + 40, y + bar_h - 10),
			"%.3f  pnl %.4f  %s" % [calmar, met.get("pnl", 0.0), met.get("slots", "")],
			HORIZONTAL_ALIGNMENT_LEFT, -1, 9, TXT)
		y += bar_h
