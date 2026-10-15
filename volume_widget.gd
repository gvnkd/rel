class_name VolumeWidget
extends Control

const PANE_BG := Color(0.085, 0.095, 0.115)
const SEP_COL := Color(1.0, 1.0, 1.0, 0.2)
const LABEL_COL := Color(0.72, 0.75, 0.8)
const FONT_SIZE := 12
const TOP_PAD := 6.0

var view: ChartView
var data: PlotData
var pane_h := 0.0
var collapsed := false

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

static func fmt_vol(v: float) -> String:
	if v >= 1.0e9:
		return "%.1fB" % (v / 1.0e9)
	if v >= 1.0e6:
		return "%.1fM" % (v / 1.0e6)
	if v >= 1.0e3:
		return "%.1fK" % (v / 1.0e3)
	return str(int(v))

func _draw() -> void:
	if view == null or data == null:
		return
	var r := view.plot_rect
	if r.size.x <= 0.0 or r.size.y <= 0.0:
		return
	var font := ThemeDB.fallback_font
	var sep_y := r.end.y - pane_h
	if pane_h >= 1.0:
		draw_rect(Rect2(r.position.x, sep_y, r.size.x, pane_h), PANE_BG)
	draw_line(Vector2(r.position.x, sep_y), Vector2(r.end.x, sep_y), SEP_COL)
	if collapsed or pane_h < 8.0:
		draw_string(font, Vector2(r.position.x, sep_y - 6.0),
			"volume — V to show, drag edge to resize",
			HORIZONTAL_ALIGNMENT_CENTER, r.size.x, FONT_SIZE, LABEL_COL)
		return
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	var vmax := 0.0
	for s in data.series:
		if not s.visible or s.size() == 0:
			continue
		var last := mini(i1, s.size() - 1)
		for i in range(maxi(i0, 0), last + 1):
			vmax = maxf(vmax, s.candle_at(i).vol)
	if vmax <= 0.0:
		return
	var body := clampf(view.px_per_index * 0.7, 1.0, 30.0)
	var max_h := pane_h - TOP_PAD
	for s in data.series:
		if not s.visible or s.size() == 0:
			continue
		var last := mini(i1, s.size() - 1)
		if last < i0:
			continue
		for i in range(i0, last + 1):
			var c: Candle = s.candle_at(i)
			if c.vol <= 0:
				continue
			var x := view.index_to_x(i) + view.px_per_index / 2.0
			var h := maxf(c.vol / vmax * max_h, 1.0)
			var col: Color = s.col_pos if c.close >= c.open else s.col_neg
			draw_rect(Rect2(x - body / 2.0, r.end.y - h, body, h), col)
	draw_string(font, Vector2(2.0, sep_y + 14.0), fmt_vol(vmax),
		HORIZONTAL_ALIGNMENT_RIGHT, ChartView.MARGIN_LEFT - 6.0, FONT_SIZE, LABEL_COL)
