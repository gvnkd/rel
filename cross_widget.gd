class_name CrossWidget
extends Control

const CROSS_COL := Color(1.0, 1.0, 1.0, 0.35)
const LABEL_BG := Color(0.08, 0.09, 0.11, 0.9)
const LABEL_BORDER := Color(1.0, 1.0, 1.0, 0.25)

var view: ChartView
var data: PlotData
# the sub-pane stack (shared by identity from chart.gd) — the vertical
# line spans the chart and every pane; the horizontal line + axis tags
# stay chart-only
var panes: Array = []
var mouse := Vector2(-10000.0, -10000.0)

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func _draw() -> void:
	if view == null or data == null:
		return
	var r := view.plot_rect
	var in_plot := r.has_point(mouse)
	if not in_plot:
		# hovering a sub-pane still shows the shared vertical line
		var in_pane := false
		for p in panes:
			if p.pane_rect.has_point(mouse):
				in_pane = true
		if not in_pane:
			return
	var total := data.max_len()
	var i := maxi(0, int(floor(view.x_to_index(mouse.x))))
	if total > 0:
		i = mini(i, total - 1)
	var x := view.index_to_x(i) + view.px_per_index / 2.0
	# vertical line across the whole stack (chart pane + sub-panes tile
	# full_rect exactly, so one line covers every pane)
	draw_line(Vector2(x, view.full_rect.position.y), Vector2(x, view.full_rect.end.y), CROSS_COL)
	if not in_plot:
		return
	draw_line(Vector2(r.position.x, mouse.y), Vector2(r.end.x, mouse.y), CROSS_COL)
	var font := ThemeDB.fallback_font
	var y := clampf(mouse.y, r.position.y + 9.0, r.end.y - 9.0)
	_tag(font, Rect2(2.0, y - 9.0, ChartView.MARGIN_LEFT - 6.0, 18.0), view.fmt_price(view.y_to_price(mouse.y)), HORIZONTAL_ALIGNMENT_RIGHT)
	var lx := clampf(x, r.position.x + 44.0, r.end.x - 44.0)
	_tag(font, Rect2(lx - 44.0, r.end.y + 4.0, 88.0, 18.0), str(i), HORIZONTAL_ALIGNMENT_CENTER)

func _tag(font: Font, rect: Rect2, text: String, align: HorizontalAlignment) -> void:
	draw_rect(rect, LABEL_BG)
	draw_rect(rect, LABEL_BORDER, false)
	draw_string(font, Vector2(rect.position.x, rect.position.y + 13.0), text, align, rect.size.x, 12, Color.WHITE)
