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
		var last := mini(i1, s.candles.size() - 1)
		if last < i0:
			continue
		var pts := PackedVector2Array()
		var win_lo := maxi(0, i0 - period + 1)
		var sum := 0.0
		for j in range(win_lo, i0):
			sum += s.candles[j].close
		for i in range(i0, last + 1):
			sum += s.candles[i].close
			if i - period >= win_lo:
				sum -= s.candles[i - period].close
			if i >= win_lo + period - 1:
				pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0, view.price_to_y(sum / period)))
		if pts.size() >= 2:
			draw_polyline(pts, s.col_pos.lightened(0.3), 1.5)
