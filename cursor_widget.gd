class_name CursorWidget
extends Control

const FONT_SIZE := 20
const PAD := 8.0
const EXTRA_COL := Color(0.7, 0.72, 0.75)
# MKV transition-matrix panel under the tableau
const MKV_CELL := 90.0
const MKV_LABEL_W := 72.0
const MKV_STATE_NAMES: Array[String] = ["Bu", "Sw", "Be"]

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
	var mkv_panels: Array = []
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
				elif ind.kind == "TEMP":
					txt += "  Temp %.3f" % float(v)
				elif ind.kind == "CECP":
					txt += "  CECP H%.3f C%.3f" % [v[0], v[1]]
				elif ind.kind == "OBVOL":
					txt += "  OBVOL B%s A%s" % [VolumeWidget.fmt_vol(int(v[0])), VolumeWidget.fmt_vol(int(v[1]))]
				elif ind.kind == "MKV":
					# the 3x3 matrix is drawn as a panel under the
					# tableau, not inline text
					mkv_panels.append([s, ind, i, v as Array])
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
	for panel in mkv_panels:
		y = _draw_mkv_panel(font, panel, x, y + 6.0, block_w)

# one 3x3 transition matrix per (series, MKV indicator), right-aligned
# under the cursor tableau: rows = current state at the cursor candle,
# cols = next-bar probabilities; the diagonal (stay / stickiness) cells
# get a border, the background heat = probability; v is the 13-element
# value_at record (BSBP x3, matrix row-major x9, rolling surprise)
func _draw_mkv_panel(font: Font, panel: Array, x: float, y: float, block_w: float) -> float:
	var s: PlotData.Series = panel[0]
	var ind: IndicatorWidget.Indicator = panel[1]
	var v: Array = panel[3]
	var col := indicators.color_for(s, indicators.indicators.find(ind))
	var w := MKV_LABEL_W + 3 * MKV_CELL
	var px := x + block_w - w
	draw_string(font, Vector2(px, y + 30), "%s MKV [%d]" % [s.name, panel[2]], HORIZONTAL_ALIGNMENT_LEFT, w, 28, EXTRA_COL)
	y += 40
	for c in 3:
		draw_string(font, Vector2(px + MKV_LABEL_W + c * MKV_CELL, y + 26), MKV_STATE_NAMES[c], HORIZONTAL_ALIGNMENT_CENTER, MKV_CELL, 24, EXTRA_COL)
	y += 30
	for r in 3:
		draw_string(font, Vector2(px, y + 58), MKV_STATE_NAMES[r], HORIZONTAL_ALIGNMENT_RIGHT, MKV_LABEL_W - 10, 24, EXTRA_COL)
		for c in 3:
			var p: float = v[r * 3 + c]
			var rect := Rect2(px + MKV_LABEL_W + c * MKV_CELL + 2, y + 2, MKV_CELL - 4, MKV_CELL - 4)
			draw_rect(rect, Color(col, 0.10 + 0.45 * p))
			if r == c:
				draw_rect(rect, Color(1, 1, 1, 0.45), false, 2.0)
			draw_string(font, rect.position + Vector2(0, (MKV_CELL - 4) * 0.5 + 10), "%.2f" % p, HORIZONTAL_ALIGNMENT_CENTER, MKV_CELL - 4, 28, Color.WHITE)
		y += MKV_CELL
	# BSBP line: the window's state mix (Bull/SW/Bear probabilities) plus
	# the cursor bar's change to the previous bar, so the cell mix reads
	# next to the move that produced it; hidden while the BSBP window is
	# still filling (NaN)
	if not is_nan(float(v[0])):
		var chg := "  --"
		var bi: int = panel[2]
		if bi >= 1:
			var prev: float = s.candle_at(bi - 1).close
			if prev > 0.0:
				var pct := (s.candle_at(bi).close / prev - 1.0) * 100.0
				chg = "  %+.2f%%" % pct
		draw_string(font, Vector2(px, y + 30), "BS %.2f/%.2f/%.2f%s" % [v[0], v[1], v[2], chg], HORIZONTAL_ALIGNMENT_LEFT, w, 28, EXTRA_COL)
		y += 38
	draw_string(font, Vector2(px, y + 30), "S %.2f" % float(v[12]), HORIZONTAL_ALIGNMENT_LEFT, w, 28, Color(1.0, 0.45, 0.6))
	return y + 42
