class_name TradesWidget
extends Control

# backtest trades overlay: for every trade an entry marker (triangle,
# green up / red down) at the entry candle and price, a dashed line at
# the entry price tracking right until the exit candle, an x marker at
# the exit price, an info label (entry price, bar volume, open cause)
# next to the entry, and the trade result (% of entry, fee-inclusive pnl
# sign) at the exit. Trades come from the shared `_trades_sets` array
# owned by chart.gd (same object identity — chart mutates, we redraw).

var view: ChartView
var data: PlotData
var trades_sets: Array = []

const DASH_ON := 6.0
const DASH_OFF := 4.0
const FONT_SZ := 20
const LABEL_BG := Color(0.08, 0.08, 0.10, 0.96)
const PAD := Vector2(3, 2)

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func _draw() -> void:
	if view == null:
		return
	var font := get_theme_default_font()
	# draw_multiline_string/draw_string place the FIRST LINE'S BASELINE at
	# pos (even for multiline) — the text band starts ascent px above pos,
	# so label backgrounds must be ascent-shifted too
	var ascent := font.get_ascent(FONT_SZ)
	var ir := view.visible_index_range()
	for ts in trades_sets:
		if not ts.get("enabled", true):
			continue
		var col: Color = ts.get("color", Color.WHITE)
		for t in ts["trades"]:
			var i0: int = t["i0"]
			var i1: int = t["i1"]
			if i1 < ir.x or i0 > ir.y:
				continue
			var x0 := view.index_to_x(i0)
			var x1 := view.index_to_x(i1)
			var y0 := view.price_to_y(t["entry"])
			var y1 := view.price_to_y(t["exit"])
			var up: bool = t["side"] > 0
			# dashed track entry -> exit, colored by trade result
			var tcol := Color(0.4, 0.9, 0.55) if t["pnl"] >= 0 else Color(0.95, 0.45, 0.45)
			_dash_line(Vector2(x0, y0), Vector2(minf(x1, size.x), y1), tcol)
			var mcol := col.lightened(0.25)
			var pts := PackedVector2Array()
			if up:
				pts.append(Vector2(x0 - 4, y0 + 6))
				pts.append(Vector2(x0 + 4, y0 + 6))
				pts.append(Vector2(x0, y0 - 2))
			else:
				pts.append(Vector2(x0 - 4, y0 - 6))
				pts.append(Vector2(x0 + 4, y0 - 6))
				pts.append(Vector2(x0, y0 + 2))
			draw_colored_polygon(pts, mcol)
			draw_line(Vector2(x1 - 3, y1 - 3), Vector2(x1 + 3, y1 + 3), mcol, 1.2)
			draw_line(Vector2(x1 - 3, y1 + 3), Vector2(x1 + 3, y1 - 3), mcol, 1.2)
			# entry label: longs above the marker, shorts below — keeps
			# opposite-side labels from stacking on the same level
			var info := "%s x%d\n%s" % [view.fmt_price(t["entry"]), int(t["vol"]), t["cause"]]
			var isize := font.get_multiline_string_size(info, HORIZONTAL_ALIGNMENT_LEFT, -1, FONT_SZ)
			var ipos := Vector2(x0 + 6, y0 - 14 - isize.y) if up else Vector2(x0 + 6, y0 + 12)
			_label_bg(font, ipos, isize, ascent)
			draw_multiline_string(font, ipos, info,
				HORIZONTAL_ALIGNMENT_LEFT, -1, FONT_SZ, -1, col)
			var res := "%+.2f%%" % ((t["exit"] / t["entry"] - 1.0) * 100.0 * signf(t["side"]))
			var rsize := font.get_string_size(res, HORIZONTAL_ALIGNMENT_LEFT, -1, FONT_SZ)
			var rpos := Vector2(x1 + 5, y1 - 6 - rsize.y) if up else Vector2(x1 + 5, y1 + 6)
			_label_bg(font, rpos, rsize, ascent)
			draw_multiline_string(font, rpos, res, HORIZONTAL_ALIGNMENT_LEFT,
				-1, FONT_SZ, -1, tcol)

# dark background behind a label drawn at baseline-origin pos; `lsize` is
# the text's bounding size (multiline size for multi-line labels)
func _label_bg(_font: Font, pos: Vector2, lsize: Vector2, ascent: float) -> void:
	var rect := Rect2(
		Vector2(pos.x - PAD.x, pos.y - ascent - PAD.y),
		lsize + PAD * 2)
	draw_rect(rect, LABEL_BG, true)

func _dash_line(a: Vector2, b: Vector2, col: Color) -> void:
	var d := b - a
	var len := d.length()
	if len < 1.0:
		return
	var dir := d / len
	var pos := 0.0
	while pos < len:
		var e := minf(pos + DASH_ON, len)
		draw_line(a + dir * pos, a + dir * e, col, 1.0)
		pos = e + DASH_OFF
