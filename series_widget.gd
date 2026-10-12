class_name SeriesWidget
extends Control

enum Mode { CANDLES, BARS, LINE, DOTS }

const MODE_NAMES: Array[String] = ["candles", "bars", "line", "dots"]
const MODE_COUNT := 4

var view: ChartView
var data: PlotData
var mode := Mode.CANDLES

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func cycle_mode() -> void:
	mode = (mode + 1) % MODE_COUNT

func _draw() -> void:
	if view == null or data == null:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	for s in data.series:
		if not s.visible or s.candles.is_empty():
			continue
		var total := s.candles.size()
		var end := total if data.head < 0 else mini(data.head + 1, total)
		var last := mini(i1, end - 1)
		if last < i0:
			continue
		match mode:
			Mode.CANDLES:
				_draw_candles(s, i0, last)
			Mode.BARS:
				_draw_bars(s, i0, last)
			Mode.LINE:
				_draw_line(s, i0, last)
			Mode.DOTS:
				_draw_dots(s, i0, last)

func _slot_center(i: int) -> float:
	return view.index_to_x(i) + view.px_per_index / 2.0

func _draw_candles(s, i0: int, i1: int) -> void:
	var body := clampf(view.px_per_index * 0.7, 1.0, 30.0)
	var wick := maxf(1.0, body * 0.14)
	for i in range(i0, i1 + 1):
		var c: Candle = s.candles[i]
		var x := _slot_center(i)
		var col: Color = s.col_pos if c.close >= c.open else s.col_neg
		var y_hi := view.price_to_y(c.high)
		var y_lo := view.price_to_y(c.low)
		draw_rect(Rect2(x - wick / 2.0, y_hi, wick, y_lo - y_hi), col)
		var y_o := view.price_to_y(c.open)
		var y_c := view.price_to_y(c.close)
		draw_rect(Rect2(x - body / 2.0, minf(y_o, y_c), body, maxf(absf(y_c - y_o), 1.0)), col)

func _draw_bars(s, i0: int, i1: int) -> void:
	var tick := maxf(2.0, view.px_per_index * 0.35)
	for i in range(i0, i1 + 1):
		var c: Candle = s.candles[i]
		var x := _slot_center(i)
		var col: Color = s.col_pos if c.close >= c.open else s.col_neg
		draw_line(Vector2(x, view.price_to_y(c.high)), Vector2(x, view.price_to_y(c.low)), col, 1.0)
		draw_line(Vector2(x - tick, view.price_to_y(c.open)), Vector2(x, view.price_to_y(c.open)), col, 1.0)
		draw_line(Vector2(x, view.price_to_y(c.close)), Vector2(x + tick, view.price_to_y(c.close)), col, 1.0)

func _draw_line(s, i0: int, i1: int) -> void:
	var pts := PackedVector2Array()
	pts.resize(i1 - i0 + 1)
	for i in range(i0, i1 + 1):
		pts[i - i0] = Vector2(_slot_center(i), view.price_to_y(s.candles[i].close))
	if pts.size() == 1:
		draw_circle(pts[0], 1.5, s.col_pos)
	else:
		draw_polyline(pts, s.col_pos, 1.5)

func _draw_dots(s, i0: int, i1: int) -> void:
	var rad := maxf(1.2, view.px_per_index * 0.18)
	for i in range(i0, i1 + 1):
		draw_circle(Vector2(_slot_center(i), view.price_to_y(s.candles[i].close)), rad, s.col_pos)
