class_name SeriesPanel
extends Control

signal series_toggled(idx: int, visible: bool)
signal series_removed(idx: int)
signal series_loaded(tl_name: String, path: String)
signal color_picked(c: Color, idx: int, field: String)
signal ind_color_picked(c: Color, si: int, k: int)
signal series_reset(idx: int)
signal series_saved(idx: int)
signal tf_picked(sel: int, idx: int)

var chart  # chart.gd (anonymous script): data.series + indicator_widget

var _list: VBoxContainer
var _box: VBoxContainer
var _load_dialog: FileDialog

func _ready() -> void:
	_build_ui()

func _build_ui() -> void:
	var box := VBoxContainer.new()
	_box = box
	box.set_anchors_preset(Control.PRESET_FULL_RECT)
	box.add_theme_constant_override("separation", 4)
	add_child(box)

	var scroll := ScrollContainer.new()
	scroll.custom_minimum_size.y = 160
	scroll.size_flags_vertical = SIZE_EXPAND_FILL
	box.add_child(scroll)
	_list = VBoxContainer.new()
	_list.size_flags_horizontal = SIZE_EXPAND_FILL
	scroll.add_child(_list)

	custom_minimum_size = box.get_combined_minimum_size()

	_load_dialog = FileDialog.new()
	_load_dialog.file_mode = FileDialog.FILE_MODE_OPEN_FILE
	_load_dialog.access = FileDialog.ACCESS_FILESYSTEM
	_load_dialog.filters = PackedStringArray(["*.csv ; CSV candles", "*.rbin ; Binary candles"])
	_load_dialog.file_selected.connect(_on_file_selected)
	add_child(_load_dialog)

func open_load_dialog() -> void:
	_load_dialog.popup_centered(Vector2i(700, 500))

func _on_file_selected(path: String) -> void:
	series_loaded.emit(path.get_file().get_basename(), path)

# rebuilds the rows from chart.data.series (single source of truth)
func refresh() -> void:
	if _list == null:
		return
	for c in _list.get_children():
		c.queue_free()
	if chart == null:
		return
	var data = chart.data
	if data.series.is_empty():
		var none := Label.new()
		none.text = "(no series)"
		_list.add_child(none)
		return
	for i in data.series.size():
		_list.add_child(_build_row(i))

func _build_row(i: int) -> Control:
	var s = chart.data.series[i]
	var box := VBoxContainer.new()
	box.add_theme_constant_override("separation", 2)
	var row := HBoxContainer.new()
	var check := CheckBox.new()
	check.button_pressed = s.visible
	check.toggled.connect(_on_row_toggled.bind(i))
	row.add_child(check)
	var label := Label.new()
	label.text = s.name
	label.size_flags_horizontal = SIZE_EXPAND_FILL
	row.add_child(label)
	var rm := Button.new()
	rm.text = "×"
	rm.tooltip_text = "remove timeline"
	rm.pressed.connect(_on_row_remove.bind(i))
	row.add_child(rm)
	box.add_child(row)

	var row2 := HBoxContainer.new()
	row2.add_theme_constant_override("separation", 2)
	var up := _color_button(s.body_up if s.body_up.a > 0.0 else s.col_pos)
	up.color_changed.connect(_on_color.bind(i, "body_up"))
	row2.add_child(up)
	var down := _color_button(s.body_down if s.body_down.a > 0.0 else s.col_neg)
	down.color_changed.connect(_on_color.bind(i, "body_down"))
	row2.add_child(down)
	var wick := _color_button(s.wick if s.wick.a > 0.0 else s.col_pos)
	wick.color_changed.connect(_on_color.bind(i, "wick"))
	row2.add_child(wick)
	var reset := Button.new()
	reset.text = "r"
	reset.tooltip_text = "reset colors"
	reset.pressed.connect(_on_reset.bind(i))
	row2.add_child(reset)
	var save := Button.new()
	save.text = "bin"
	save.tooltip_text = "save series as binary (.rbin): candles + books + levels"
	save.pressed.connect(_on_save.bind(i))
	row2.add_child(save)
	var tf_opt := OptionButton.new()
	for tname in QuoteStore.TFS:
		tf_opt.add_item(tname)
	tf_opt.selected = maxi(0, QuoteStore.TFS.find(s.tf))
	tf_opt.item_selected.connect(_on_tf.bind(i))
	row2.add_child(tf_opt)
	for k in chart.indicator_widget.indicators.size():
		var icb := _color_button(chart.indicator_widget.color_for(s, k))
		icb.color_changed.connect(_on_ind_color.bind(i, k))
		row2.add_child(icb)
	box.add_child(row2)
	return box

func _color_button(c: Color) -> ColorPickerButton:
	var b := ColorPickerButton.new()
	b.edit_alpha = false
	b.custom_minimum_size = Vector2(32, 20)
	b.color = c
	return b

func _on_row_toggled(on: bool, i: int) -> void:
	series_toggled.emit(i, on)

func _on_row_remove(i: int) -> void:
	series_removed.emit(i)

func _on_color(c: Color, i: int, field: String) -> void:
	color_picked.emit(c, i, field)

func _on_ind_color(c: Color, si: int, k: int) -> void:
	ind_color_picked.emit(c, si, k)

func _on_reset(i: int) -> void:
	series_reset.emit(i)

func _on_save(i: int) -> void:
	series_saved.emit(i)

func _on_tf(sel: int, i: int) -> void:
	tf_picked.emit(sel, i)
