class_name MlPanel
extends Control

signal timeline_added(tl_name: String, path: String)

const PARAM_FIELDS := [
	["sessions", "sessions", "4"],
	["seed", "seed base", "1"],
	["epochs", "epochs", "50"],
	["lr", "learn rate", "0.05"],
	["hidden", "hidden layers", "16"],
	["trainpct", "train %", "0.6"],
	["valpct", "val %", "0.2"],
	["fee", "fee", "0.0002"],
	["kswin", "ks window (0=off)", "0"],
	["ksm", "ks patterns", "5"],
]

const DATA_DIR := "user://mldata"

var fields := {}
var sim_available := false
var _sim: Variant
var _box: VBoxContainer
var _train_btn: Button
var _status_label: Label
var _table: VBoxContainer
var _data_edit: LineEdit
var _dialog: FileDialog
var _thread: Thread
var _train_name := ""
var _results := {}
var _poll_ms := -1
var _base_seed := 1
var _session_count := 0

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
	title.text = "ML Trainer"
	box.add_child(title)

	var grid := GridContainer.new()
	grid.columns = 2
	box.add_child(grid)

	var data_label := Label.new()
	data_label.text = "data csv"
	grid.add_child(data_label)
	var data_row := HBoxContainer.new()
	_data_edit = LineEdit.new()
	_data_edit.size_flags_horizontal = SIZE_EXPAND_FILL
	_data_edit.placeholder_text = "candles csv (sim or real quotes)"
	data_row.add_child(_data_edit)
	var browse := Button.new()
	browse.text = "..."
	browse.pressed.connect(_on_browse)
	data_row.add_child(browse)
	grid.add_child(data_row)

	for spec in PARAM_FIELDS:
		var label := Label.new()
		label.text = spec[1]
		grid.add_child(label)
		var edit := LineEdit.new()
		edit.text = spec[2]
		edit.custom_minimum_size.x = 90
		grid.add_child(edit)
		fields[spec[0]] = edit

	_train_btn = Button.new()
	_train_btn.text = "Train"
	_train_btn.pressed.connect(_on_train)
	box.add_child(_train_btn)

	var scroll := ScrollContainer.new()
	scroll.custom_minimum_size.y = 160
	scroll.size_flags_vertical = SIZE_EXPAND_FILL
	box.add_child(scroll)
	_table = VBoxContainer.new()
	_table.size_flags_horizontal = SIZE_EXPAND_FILL
	scroll.add_child(_table)

	_status_label = Label.new()
	box.add_child(_status_label)

	custom_minimum_size = box.get_combined_minimum_size()

	_dialog = FileDialog.new()
	_dialog.file_mode = FileDialog.FILE_MODE_OPEN_FILE
	_dialog.access = FileDialog.ACCESS_FILESYSTEM
	_dialog.filters = PackedStringArray(["*.csv ; CSV candles"])
	_dialog.file_selected.connect(_on_data_picked)
	add_child(_dialog)

func _update_status(msg := "") -> void:
	if msg.is_empty():
		_status_label.text = "extension: %s" % ("ok" if sim_available else "MISSING (nix run .#link-sim-extension)")
	else:
		_status_label.text = msg

func _on_browse() -> void:
	_dialog.popup_centered(Vector2i(700, 500))

func _on_data_picked(path: String) -> void:
	_data_edit.text = path

func _run_dir() -> String:
	return "%s/%s" % [DATA_DIR, _train_name]

func _params_string() -> String:
	var parts: PackedStringArray = []
	parts.append("data=%s" % _data_edit.text.strip_edges())
	parts.append("out=%s" % ProjectSettings.globalize_path(_run_dir()))
	for key in fields:
		parts.append("%s=%s" % [key, fields[key].text.strip_edges()])
	return ";".join(parts)

func _on_train() -> void:
	if _thread != null:
		_sim.ml_cancel()
		_update_status("cancelling…")
		return
	if not sim_available:
		_update_status("cannot train: SimNative extension missing")
		return
	var data_path := _data_edit.text.strip_edges()
	if data_path.is_empty() or not FileAccess.file_exists(data_path):
		_update_status("pick an existing candles csv first")
		return
	_train_name = "run%d" % (Time.get_ticks_msec() % 100000)
	_base_seed = fields["seed"].text.to_int()
	_session_count = maxi(1, fields["sessions"].text.to_int())
	_results = {}
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(_run_dir()))
	_train_btn.disabled = true
	_train_btn.text = "Cancel"
	_thread = Thread.new()
	var start_err: int = _thread.start(Callable(_sim, "ml_train").bind(_params_string()))
	if start_err != OK:
		_thread = null
		_train_btn.disabled = false
		_train_btn.text = "Train"
		_update_status("failed to start training thread (code %d)" % start_err)
		return
	set_process(true)

func _process(_delta: float) -> void:
	if _thread == null:
		set_process(false)
		return
	var now := Time.get_ticks_msec()
	if now - _poll_ms > 500:
		_poll_ms = now
		_update_status("training %s… %d%%" % [_train_name, int(_sim.ml_progress() * 100)])
		var parsed: Variant = JSON.parse_string(_sim.ml_sessions())
		if parsed is Array:
			_refresh_table(parsed)
	if _thread.is_alive():
		return
	_thread.wait_to_finish()
	_thread = null
	set_process(false)
	_train_btn.disabled = false
	_train_btn.text = "Train"
	_load_results()
	var parsed: Variant = JSON.parse_string(_sim.ml_sessions())
	if parsed is Array:
		_refresh_table(parsed)
	var best := -INF
	var best_seed := 0
	for seed in _results:
		var m: Dictionary = _results[seed]
		if m["calmar"] > best:
			best = m["calmar"]
			best_seed = seed
	if best_seed > 0:
		_update_status("done; best seed %d (calmar %.4f)" % [best_seed, best])
	else:
		_update_status("done (no session produced a result)")

func _load_results() -> void:
	for k in _session_count:
		var seed := _base_seed + k
		var p := "%s/ml_%d.json" % [ProjectSettings.globalize_path(_run_dir()), seed]
		if not FileAccess.file_exists(p):
			continue
		var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string(p))
		if parsed is Dictionary:
			_results[seed] = parsed

func _refresh_table(live: Array) -> void:
	for c in _table.get_children():
		c.queue_free()
	for entry in live:
		var s: Dictionary = entry
		var seed: int = s["seed"]
		var row := HBoxContainer.new()
		var lab := Label.new()
		lab.text = "#%d" % seed
		lab.custom_minimum_size.x = 34
		row.add_child(lab)
		var prog := Label.new()
		prog.text = "%d/%d" % [s["epoch"], s["epochs"]]
		prog.custom_minimum_size.x = 56
		row.add_child(prog)
		var info := Label.new()
		info.size_flags_horizontal = SIZE_EXPAND_FILL
		if _results.has(seed):
			var m: Dictionary = _results[seed]
			info.text = "calmar %.4f  acc %.4f  pnl %.4f (%d trades)" % [
				m["calmar"], m["accuracy"], m["pnl"], m["trades"]]
		else:
			info.text = "%s  best %.4f" % [s["status"], s["best"]]
		row.add_child(info)
		var chart_btn := Button.new()
		chart_btn.text = "chart"
		var eq := "%s/equity_%d.csv" % [ProjectSettings.globalize_path(_run_dir()), seed]
		chart_btn.disabled = not FileAccess.file_exists(eq)
		chart_btn.pressed.connect(_on_chart_pressed.bind(seed))
		row.add_child(chart_btn)
		_table.add_child(row)

func _on_chart_pressed(seed: int) -> void:
	var eq := "%s/equity_%d.csv" % [ProjectSettings.globalize_path(_run_dir()), seed]
	timeline_added.emit("eq_%s_%d" % [_train_name, seed], eq)
