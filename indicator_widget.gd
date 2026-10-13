class_name IndicatorWidget
extends Control

class Indicator:
	var kind := "SMA"
	var enabled := true
	var period := 20

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

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE
	if indicators.is_empty():
		add_indicator("SMA", 20)

func add_indicator(kind: String, period: int) -> void:
	var ind := Indicator.new()
	ind.kind = kind
	ind.period = period
	indicators.append(ind)

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
		for s in data.series:
			if not s.visible or s.candles.size() < ind.period:
				continue
			var col := color_for(s, k)
			var last := mini(i1, s.candles.size() - 1)
			if last < i0:
				continue
			var pts := PackedVector2Array()
			var win_lo := maxi(0, i0 - ind.period + 1)
			var sum := 0.0
			for j in range(win_lo, i0):
				sum += s.candles[j].close
			for i in range(i0, last + 1):
				sum += s.candles[i].close
				if i - ind.period >= win_lo:
					sum -= s.candles[i - ind.period].close
				if i >= win_lo + ind.period - 1:
					pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0, view.price_to_y(sum / ind.period)))
			if pts.size() >= 2:
				draw_polyline(pts, col, 1.5)
