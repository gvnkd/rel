class_name CursorWidget
extends Control

const FONT_SIZE := 12
const PAD := 8.0
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
	var line_h := font.get_height(FONT_SIZE) + 4.0
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
	var block_w := PAD * 2.0
	for item in lines:
		block_w = maxf(block_w, font.get_string_size(item[0], HORIZONTAL_ALIGNMENT_LEFT, -1, FONT_SIZE).x + PAD * 2.0)
	for t in extra:
		block_w = maxf(block_w, font.get_string_size(t, HORIZONTAL_ALIGNMENT_LEFT, -1, FONT_SIZE).x + PAD * 2.0)
	var x := view.plot_rect.end.x - block_w - 10.0
	var y := view.plot_rect.position.y + 8.0
	var h := (lines.size() + extra.size()) * line_h + PAD * 2.0 - 4.0
	draw_rect(Rect2(x - PAD * 0.75, y - PAD * 0.5, block_w, h), Color(0.05, 0.06, 0.08, 0.75))
	var text_w := block_w - PAD * 2.0
	for item in lines:
		draw_string(font, Vector2(x, y + font.get_ascent(FONT_SIZE) + 2.0), item[0], HORIZONTAL_ALIGNMENT_LEFT, text_w, FONT_SIZE, item[1])
		y += line_h
	for t in extra:
		draw_string(font, Vector2(x, y + font.get_ascent(FONT_SIZE) + 2.0), t, HORIZONTAL_ALIGNMENT_LEFT, text_w, FONT_SIZE, EXTRA_COL)
		y += line_h
