class_name IndicatorWidget
extends Control

var view: ChartView
var data: PlotData
var enabled := true
var period := 20

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func _draw() -> void:
	if not enabled or view == null or data == null:
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	for s in data.series:
		if not s.visible or s.candles.size() < period:
			continue
		var total := s.candles.size()
		var end := total if data.head < 0 else mini(data.head + 1, total)
		var last := mini(i1, end - 1)
		if last < i0:
			continue
		var pts := PackedVector2Array()
		var sum := 0.0
		for i in range(i0, last + 1):
			sum += s.candles[i].close
			if i - i0 >= period:
				sum -= s.candles[i - period].close
			if i - i0 >= period - 1:
				pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0, view.price_to_y(sum / period)))
		if pts.size() >= 2:
			draw_polyline(pts, s.col_pos.lightened(0.3), 1.5)
