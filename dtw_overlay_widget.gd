class_name DtwOverlayWidget
extends Control

# DTW matched-window overlay: every matched A window with dist <= max_dist
# is drawn as a semi-opaque rectangle over the plot rect (x = window's
# index range, full price height), colored by dist (green = good). A sorted
# key array keeps drawing/hit-testing to the visible slice only — matches
# dicts can hold 300k entries. Click routing happens in chart.gd, which
# asks window_at(); filter UI lives in a top-right row owned here.

var view: ChartView
var data: PlotData
var matches := {}      # i -> [j, dist] (shared by identity with the DTW page)
var win := 0
var max_dist := 1.5
var sel_i := -1
var _keys := PackedInt64Array()
var _slider: HSlider
var _val_label: Label

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE
	var row := HBoxContainer.new()
	row.set_anchors_preset(Control.PRESET_TOP_RIGHT)
	row.position = Vector2(-236, 4)
	row.add_theme_constant_override("separation", 4)
	var lab := Label.new()
	lab.text = "dtw max dist"
	lab.add_theme_font_size_override("font_size", 11)
	row.add_child(lab)
	_slider = HSlider.new()
	_slider.custom_minimum_size = Vector2(110, 0)
	_slider.min_value = 0.0
	_slider.max_value = 1.5
	_slider.step = 0.001
	_slider.value = max_dist
	_slider.value_changed.connect(_on_slider)
	row.add_child(_slider)
	_val_label = Label.new()
	_val_label.text = "%.4f" % max_dist
	_val_label.add_theme_font_size_override("font_size", 11)
	_val_label.custom_minimum_size.x = 46
	row.add_child(_val_label)
	var clear_btn := Button.new()
	clear_btn.text = "x"
	clear_btn.tooltip_text = "remove the DTW overlay"
	clear_btn.pressed.connect(clear)
	row.add_child(clear_btn)
	add_child(row)
	visible = false

func _on_slider(v: float) -> void:
	max_dist = v
	_val_label.text = "%.4f" % v
	queue_redraw()

func set_data(m: Dictionary, pwin: int) -> void:
	matches = m
	win = maxi(1, pwin)
	sel_i = -1
	var ks := m.keys()
	ks.sort()
	_keys = PackedInt64Array(ks)
	visible = not m.is_empty()
	queue_redraw()

func clear() -> void:
	matches = {}
	_keys = PackedInt64Array()
	visible = false
	queue_redraw()

func j_for(i: int) -> int:
	if matches.has(i):
		return int(matches[i][0])
	return -1

func _draw() -> void:
	if view == null or _keys.is_empty() or win <= 0:
		return
	var r: Rect2 = view.plot_rect
	var ir := view.visible_index_range()
	# visible key slice via binary search
	var lo := _keys.bsearch(int(floor(ir.x)) - win)
	var hi := _keys.bsearch(int(ceil(ir.y)) + 1, false)
	for n in range(lo, mini(hi + 1, _keys.size())):
		var i := int(_keys[n])
		var md: Array = matches[i]
		if md[1] > max_dist:
			continue
		var x0 := maxf(view.index_to_x(i), r.position.x)
		var x1 := minf(view.index_to_x(i + win), r.end.x)
		if x1 <= x0:
			continue
		var t: float = clampf(md[1] / 1.5, 0.0, 1.0)
		var col := Color(0.10 + 0.85 * t, 0.85 - 0.62 * t, 0.30 - 0.22 * t)
		var fill := col
		fill.a = 0.16
		draw_rect(Rect2(x0, r.position.y, x1 - x0, r.size.y), fill)
		if i == sel_i:
			var border := col.lightened(0.4)
			draw_rect(Rect2(x0, r.position.y, x1 - x0, r.size.y), border, false, 1.5)
		elif x1 - x0 > 3.0:
			var edge := col
			edge.a = 0.45
			draw_rect(Rect2(x0, r.position.y, x1 - x0, r.size.y), edge, false, 1.0)

# chart.gd asks this on LMB: the matched window whose index range contains
# pos (dist filter applies), -1 when none
func window_at(pos: Vector2) -> int:
	if view == null or _keys.is_empty():
		return -1
	var r: Rect2 = view.plot_rect
	if not r.has_point(pos):
		return -1
	var i := int(floor(view.x_to_index(pos.x)))
	# candidates: the last key <= i and the next one (windows can overlap
	# the cursor at high zoom)
	var idx := _keys.bsearch(i)
	var cand: Array = []
	if idx < _keys.size():
		cand.append(int(_keys[idx]))
	if idx > 0:
		cand.append(int(_keys[idx - 1]))
	for k in cand:
		if k <= i and i < k + win and matches.has(k):
			if matches[k][1] <= max_dist:
				return k
	return -1
