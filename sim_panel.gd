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
]

const DATA_DIR := "user://simdata"

var fields := {}
var timelines: Array[Dictionary] = []
var timeline_list: ItemList
var status_label: Label
var sim_available := false
var _sim: Variant

func _ready() -> void:
	sim_available = ClassDB.class_exists("SimNative")
	if sim_available:
		_sim = ClassDB.instantiate("SimNative")
	_build_ui()
	_update_status()

func _build_ui() -> void:
	var box := VBoxContainer.new()
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

	var gen_btn := Button.new()
	gen_btn.text = "Generate"
	gen_btn.pressed.connect(_on_generate)
	box.add_child(gen_btn)

	var load_btn := Button.new()
	load_btn.text = "Load CSV..."
	load_btn.pressed.connect(_on_load_clicked)
	box.add_child(load_btn)

	timeline_list = ItemList.new()
	timeline_list.custom_minimum_size.y = 140
	timeline_list.item_selected.connect(_on_item_selected)
	box.add_child(timeline_list)

	var rm_btn := Button.new()
	rm_btn.text = "Remove selected"
	rm_btn.pressed.connect(_on_remove)
	box.add_child(rm_btn)

	status_label = Label.new()
	box.add_child(status_label)

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

func _on_item_selected(idx: int) -> void:
	var tl: Dictionary = timelines[idx]
	tl["visible"] = not tl.get("visible", true)
	timelines[idx] = tl
	timeline_list.deselect(idx)
	timeline_list.set_item_custom_fg_color(idx, Color.WHITE if tl["visible"] else Color(1, 1, 1, 0.35))
	timeline_toggled.emit(idx, tl["visible"])

func _on_remove() -> void:
	var selected := timeline_list.get_selected_items()
	if selected.is_empty():
		return
	var idx: int = selected[0]
	timelines.remove_at(idx)
	_refresh_list()
	timeline_removed.emit(idx)

func _refresh_list() -> void:
	timeline_list.clear()
	for i in timelines.size():
		var tl: Dictionary = timelines[i]
		timeline_list.add_item(tl["name"])
		timeline_list.set_item_custom_fg_color(i, Color.WHITE if tl.get("visible", true) else Color(1, 1, 1, 0.35))
