class_name SimPanel
extends Control

signal timeline_added(tl_name: String, path: String)
signal timeline_removed(index: int)
signal timeline_toggled(index: int, visible: bool)

const PARAM_FIELDS := [
	["name", "name", "sim1"],
	["seed", "seed", "42"],
	["candles", "candles", "1000"],
	["orders-per-candle", "orders/candle", "500"],
	["tick-range", "tick range", "10"],
	["vol", "vol lo,hi", "1,20"],
	["start-bid", "start bid", "102190"],
	["start-ask", "start ask", "102195"],
	["start-vol", "start vol", "200"],
	["buy-thresh", "buy thresh", "0.25"],
	["sell-thresh", "sell thresh", "0.75"],
	["book-depth", "book depth", "50"],
]

const MODES := ["candles", "tick-trades", "tick-steps"]

const DATA_DIR := "user://simdata"

var fields := {}
var timelines: Array[Dictionary] = []
var timeline_box: VBoxContainer
var status_label: Label
var sim_available := false
var _sim: Variant
var _box: VBoxContainer
var mode_option: OptionButton

func _ready() -> void:
	sim_available = ClassDB.class_exists("SimNative")
	if sim_available:
		_sim = ClassDB.instantiate("SimNative")
	_build_ui()
	_update_status()

func _build_ui() -> void:
	var box := VBoxContainer.new()
	_box = box
	box.set_anchors_preset(Control.PRESET_FULL_RECT)
	box.add_theme_constant_override("separation", 4)
	add_child(box)

	var title := Label.new()
	title.text = "Simulation"
	box.add_child(title)

	var grid := GridContainer.new()
	grid.columns = 2
	box.add_child(grid)
	for spec in PARAM_FIELDS:
		var label := Label.new()
		label.text = spec[1]
		grid.add_child(label)
		var edit := LineEdit.new()
		edit.text = spec[2]
		edit.custom_minimum_size.x = 90
		grid.add_child(edit)
		fields[spec[0]] = edit

	var mode_label := Label.new()
	mode_label.text = "mode"
	grid.add_child(mode_label)
	mode_option = OptionButton.new()
	for m: String in MODES:
		mode_option.add_item(m)
	mode_option.custom_minimum_size.x = 90
	grid.add_child(mode_option)

	var gen_btn := Button.new()
	gen_btn.text = "Generate"
	gen_btn.pressed.connect(_on_generate)
	box.add_child(gen_btn)

	var load_btn := Button.new()
	load_btn.text = "Load CSV..."
	load_btn.pressed.connect(_on_load_clicked)
	box.add_child(load_btn)

	var scroll := ScrollContainer.new()
	scroll.custom_minimum_size.y = 140
	box.add_child(scroll)
	timeline_box = VBoxContainer.new()
	timeline_box.size_flags_horizontal = SIZE_EXPAND_FILL
	scroll.add_child(timeline_box)

	status_label = Label.new()
	box.add_child(status_label)

	custom_minimum_size = box.get_combined_minimum_size()

	var dialog := FileDialog.new()
	dialog.file_mode = FileDialog.FILE_MODE_OPEN_FILE
	dialog.access = FileDialog.ACCESS_FILESYSTEM
	dialog.filters = PackedStringArray(["*.csv ; CSV candles"])
	dialog.file_selected.connect(_on_file_selected)
	add_child(dialog)
	_load_dialog = dialog

var _load_dialog: FileDialog

func _update_status(msg := "") -> void:
	if msg.is_empty():
		status_label.text = "extension: %s" % ("ok" if sim_available else "MISSING (nix run .#link-sim-extension)")
	else:
		status_label.text = msg

func _params_string() -> String:
	var parts: PackedStringArray = []
	for spec in PARAM_FIELDS:
		var key: String = spec[0]
		if key == "name":
			continue
		parts.append("%s=%s" % [key, fields[key].text.strip_edges()])
	var mode: String = MODES[mode_option.selected]
	parts.append("mode=%s" % mode)
	return ";".join(parts)

func _on_generate() -> void:
	if not sim_available:
		_update_status("cannot generate: SimNative extension missing")
		return
	var tl_name: String = fields["name"].text.strip_edges()
	if tl_name.is_empty():
		tl_name = "sim%d" % (timelines.size() + 1)
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(DATA_DIR))
	var out_path := "%s/%s.csv" % [DATA_DIR, tl_name]
	var err: int = _sim.generate(_params_string(), ProjectSettings.globalize_path(out_path))
	if err != 0:
		_update_status("generation failed (code %d)" % err)
		return
	timelines.append({ "name": tl_name, "path": out_path, "visible": true })
	_refresh_list()
	_update_status("generated %s (%d timelines)" % [tl_name, timelines.size()])
	timeline_added.emit(tl_name, out_path)

func _on_load_clicked() -> void:
	_load_dialog.popup_centered(Vector2i(700, 500))

func _on_file_selected(path: String) -> void:
	var tl_name: String = path.get_file().get_basename()
	timelines.append({ "name": tl_name, "path": path, "visible": true })
	_refresh_list()
	timeline_added.emit(tl_name, path)

func _refresh_list() -> void:
	for c in timeline_box.get_children():
		c.queue_free()
	for i in timelines.size():
		var tl: Dictionary = timelines[i]
		var row := HBoxContainer.new()
		var check := CheckBox.new()
		check.button_pressed = tl.get("visible", true)
		check.toggled.connect(_on_row_toggled.bind(i))
		row.add_child(check)
		var label := Label.new()
		label.text = tl["name"]
		label.size_flags_horizontal = SIZE_EXPAND_FILL
		row.add_child(label)
		var rm := Button.new()
		rm.text = "×"
		rm.tooltip_text = "remove timeline"
		rm.pressed.connect(_on_row_remove.bind(i))
		row.add_child(rm)
		timeline_box.add_child(row)
	custom_minimum_size = _box.get_combined_minimum_size()

func _on_row_toggled(pressed: bool, idx: int) -> void:
	var tl: Dictionary = timelines[idx]
	tl["visible"] = pressed
	timelines[idx] = tl
	timeline_toggled.emit(idx, pressed)

func _on_row_remove(idx: int) -> void:
	timelines.remove_at(idx)
	_refresh_list()
	timeline_removed.emit(idx)
