class_name GridWidget
extends Control

const BG_COL := Color(0.1, 0.11, 0.13)
const PLOT_BG_COL := Color(0.07, 0.08, 0.1)
const GRID_COL := Color(1.0, 1.0, 1.0, 0.06)
const BORDER_COL := Color(1.0, 1.0, 1.0, 0.2)
const LABEL_COL := Color(0.72, 0.75, 0.8)
const FONT_SIZE := 12

var view: ChartView
var data: PlotData

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func _draw() -> void:
	if view == null:
		return
	draw_rect(Rect2(Vector2.ZERO, size), BG_COL)
	var r := view.plot_rect
	if r.size.x <= 0.0 or r.size.y <= 0.0:
		return
	draw_rect(r, PLOT_BG_COL)
	var font := ThemeDB.fallback_font
	var ir := view.visible_index_range()
	var i_step := maxi(1, int(round(ChartView.nice_step((ir.y - ir.x) / 8.0))))
	var i := int(ceil(ir.x / i_step)) * i_step
	while i <= int(floor(ir.y)):
		var x := view.index_to_x(i)
		draw_line(Vector2(x, r.position.y), Vector2(x, r.end.y), GRID_COL)
		draw_string(font, Vector2(x - 40.0, r.end.y + 16.0), str(i), HORIZONTAL_ALIGNMENT_CENTER, 80.0, FONT_SIZE, LABEL_COL)
		i += i_step
	var pr := view.visible_price_range()
	var p_step := ChartView.nice_step((pr.y - pr.x) / 6.0)
	var n := int(floor(pr.x / p_step))
	while n * p_step <= pr.y:
		var p := n * p_step
		var y := view.price_to_y(p)
		draw_line(Vector2(r.position.x, y), Vector2(r.end.x, y), GRID_COL)
		draw_string(font, Vector2(2.0, y + 4.0), view.fmt_price(p), HORIZONTAL_ALIGNMENT_RIGHT, ChartView.MARGIN_LEFT - 6.0, FONT_SIZE, LABEL_COL)
		n += 1
	draw_rect(r, BORDER_COL, false)
