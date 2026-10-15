class_name CursorWidget
extends Control

const FONT_SIZE := 12
const LINE_H := 16.0
const BLOCK_W := 560.0
const EXTRA_COL := Color(0.7, 0.72, 0.75)

var view: ChartView
var data: PlotData
var cross: CrossWidget
var indicators: IndicatorWidget
var mode_text := ""
var indicators_text := ""
var load_text := ""

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func _draw() -> void:
	if view == null or data == null:
		return
	var font := ThemeDB.fallback_font
	var lines := []
	for s in data.series:
		if not s.visible or s.size() == 0:
			continue
		var n := s.size()
		var i := n - 1
		if cross != null and view.plot_rect.has_point(cross.mouse):
			i = clampi(int(floor(view.x_to_index(cross.mouse.x))), 0, n - 1)
		elif data.head >= 0:
			i = clampi(data.head, 0, n - 1)
		if s.store != null:
			i = clampi(i, s.win_off, s.win_off + s.candles.size() - 1)
		var c: Candle = s.candle_at(i)
		var txt := "%s  O %s  H %s  L %s  C %s  V %s" % [s.name,
			view.fmt_price(c.open), view.fmt_price(c.high),
			view.fmt_price(c.low), view.fmt_price(c.close),
			VolumeWidget.fmt_vol(c.vol)]
		if indicators != null and indicators.enabled:
			for ind in indicators.indicators:
				if not ind.enabled:
					continue
				var v: Variant = indicators.value_at(ind, s, i)
				if v == null:
					continue
				if ind.kind == "KS":
					txt += "  KS %.3f" % float(v)
				else:
					txt += "  %s %s" % [ind.kind, view.fmt_price(float(v))]
		lines.append([txt, s.col_pos])
	var extra: Array[String] = []
	if not load_text.is_empty():
		extra.append(load_text)
	if not mode_text.is_empty():
		extra.append("mode: " + mode_text)
	if not indicators_text.is_empty():
		extra.append("indicators: " + indicators_text)
	if lines.is_empty() and extra.is_empty():
		return
	var x := view.plot_rect.end.x - BLOCK_W - 10.0
	var y := view.plot_rect.position.y + 8.0
	var h := (lines.size() + extra.size()) * LINE_H + 10.0
	draw_rect(Rect2(x - 6.0, y - 4.0, BLOCK_W, h), Color(0.05, 0.06, 0.08, 0.75))
	for item in lines:
		draw_string(font, Vector2(x, y + 11.0), item[0], HORIZONTAL_ALIGNMENT_LEFT, BLOCK_W, FONT_SIZE, item[1])
		y += LINE_H
	for t in extra:
		draw_string(font, Vector2(x, y + 11.0), t, HORIZONTAL_ALIGNMENT_LEFT, BLOCK_W, FONT_SIZE, EXTRA_COL)
		y += LINE_H
