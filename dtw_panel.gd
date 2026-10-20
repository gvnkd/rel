class_name DtwPanel
extends Control

# DTW correlation scan panel: slide windows over series A, find
# best-matching parts of series B (affine Y-deformation and time warp
# within the band factored out). One FFI call runs all requested window
# lengths in parallel; artifacts land in user://dtwdata/<run>/.

const TF_CHOICES := ["1m", "5m", "1h", "2h", "1d", "1w"]

const PARAM_FIELDS := [
	["windows", "windows", "64,128,256,512"],
	["band", "band (0=win/4)", "0"],
	["step", "step (0=win/4)", "0"],
	["maxcand", "max cand", "64"],
	["paa", "paa segs", "8"],
	["card", "cardinality", "8"],
]

const DATA_DIR := "user://dtwdata"

var fields := {}
var sim_available := false
var _sim: Variant
var _box: VBoxContainer
var _scan_btn: Button
var _status_label: Label
var _table: VBoxContainer
var _data_a: LineEdit
var _data_b: LineEdit
var _tf_a: OptionButton
var _tf_b: OptionButton
var _dialog: FileDialog
var _picking_for := ""
var _thread: Thread
var _run_name := ""
var _poll_ms := -1
var _cancel_requested := false

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
	title.text = "DTW Correlation Scan"
	box.add_child(title)

	var grid := GridContainer.new()
	grid.columns = 2
	box.add_child(grid)

	_data_a = _add_data_row(grid, "series A csv", "a")
	_data_b = _add_data_row(grid, "series B csv", "b")

	var tfa_label := Label.new()
	tfa_label.text = "tf A"
	grid.add_child(tfa_label)
	_tf_a = OptionButton.new()
	for t in TF_CHOICES:
		_tf_a.add_item(t)
	grid.add_child(_tf_a)

	var tfb_label := Label.new()
	tfb_label.text = "tf B"
	grid.add_child(tfb_label)
	_tf_b = OptionButton.new()
	for t in TF_CHOICES:
		_tf_b.add_item(t)
	_tf_b.selected = 1
	grid.add_child(_tf_b)

	for spec in PARAM_FIELDS:
		var label := Label.new()
		label.text = spec[1]
		grid.add_child(label)
		var edit := LineEdit.new()
		edit.text = spec[2]
		edit.custom_minimum_size.x = 90
		grid.add_child(edit)
		fields[spec[0]] = edit

	_scan_btn = Button.new()
	_scan_btn.text = "Scan"
	_scan_btn.pressed.connect(_on_scan)
	box.add_child(_scan_btn)

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
	_dialog.filters = PackedStringArray(["*.csv ; CSV candles", "*.rbin ; Binary candles"])
	_dialog.file_selected.connect(_on_data_picked)
	add_child(_dialog)

func _add_data_row(grid: GridContainer, label_text: String, tag: String) -> LineEdit:
	var label := Label.new()
	label.text = label_text
	grid.add_child(label)
	var row := HBoxContainer.new()
	var edit := LineEdit.new()
	edit.size_flags_horizontal = SIZE_EXPAND_FILL
	edit.placeholder_text = "candles csv or .rbin store"
	row.add_child(edit)
	var browse := Button.new()
	browse.text = "..."
	browse.pressed.connect(_on_browse.bind(tag))
	row.add_child(browse)
	grid.add_child(row)
	return edit

func _update_status(msg := "") -> void:
	if msg.is_empty():
		_status_label.text = "extension: %s" % ("ok" if sim_available else "MISSING (nix run .#link-sim-extension)")
	else:
		_status_label.text = msg

func _on_browse(tag: String) -> void:
	_picking_for = tag
	_dialog.popup_centered(Vector2i(700, 500))

func _on_data_picked(path: String) -> void:
	if _picking_for == "a":
		_data_a.text = path
	else:
		_data_b.text = path

func _run_dir() -> String:
	return "%s/%s" % [DATA_DIR, _run_name]

func _params_string() -> String:
	var parts: PackedStringArray = []
	parts.append("dataa=%s" % _data_a.text.strip_edges())
	parts.append("datab=%s" % _data_b.text.strip_edges())
	parts.append("out=%s" % ProjectSettings.globalize_path(_run_dir()))
	parts.append("atf=%s" % TF_CHOICES[_tf_a.selected])
	parts.append("btf=%s" % TF_CHOICES[_tf_b.selected])
	for key in fields:
		parts.append("%s=%s" % [key, fields[key].text.strip_edges()])
	return ";".join(parts)

func _on_scan() -> void:
	if _thread != null:
		_sim.dtw_cancel()
		_cancel_requested = true
		_update_status("cancelling…")
		return
	if not sim_available:
		_update_status("cannot scan: SimNative extension missing")
		return
	if _data_a.text.strip_edges().is_empty() or not FileAccess.file_exists(_data_a.text.strip_edges()):
		_update_status("pick an existing series A file first")
		return
	if _data_b.text.strip_edges().is_empty() or not FileAccess.file_exists(_data_b.text.strip_edges()):
		_update_status("pick an existing series B file first")
		return
	_run_name = "run%d" % (Time.get_ticks_msec() % 100000)
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(_run_dir()))
	_scan_btn.text = "Cancel"
	_thread = Thread.new()
	var start_err: int = _thread.start(Callable(_sim, "dtw_scan").bind(_params_string()))
	if start_err != OK:
		_thread = null
		_scan_btn.text = "Scan"
		_update_status("failed to start scan thread (code %d)" % start_err)
		return
	set_process(true)

func _process(_delta: float) -> void:
	if _thread == null:
		set_process(false)
		return
	var now := Time.get_ticks_msec()
	if now - _poll_ms > 500:
		_poll_ms = now
		_update_status("scanning %s… %d%%" % [_run_name, int(_sim.dtw_progress() * 100)])
		var parsed: Variant = JSON.parse_string(_sim.dtw_sessions())
		if parsed is Array:
			_refresh_table(parsed)
	if _thread.is_alive():
		return
	_thread.wait_to_finish()
	_thread = null
	set_process(false)
	_scan_btn.text = "Scan"
	var parsed: Variant = JSON.parse_string(_sim.dtw_sessions())
	if parsed is Array:
		_refresh_table(parsed)
	var done := 0
	for entry in parsed:
		if entry is Dictionary and entry.get("status", "") == "done":
			done += 1
	if _cancel_requested:
		_update_status("cancelled")
	elif done > 0:
		_update_status("done; %d session(s) with results (load a position line via the chart buttons)" % done)
	else:
		_update_status("done (no session produced a result)")
	_cancel_requested = false

func _refresh_table(live: Array) -> void:
	for c in _table.get_children():
		c.queue_free()
	for entry in live:
		var s: Dictionary = entry
		var row := HBoxContainer.new()
		var lab := Label.new()
		lab.text = "w%d" % s["win"]
		lab.custom_minimum_size.x = 44
		row.add_child(lab)
		var prog := Label.new()
		prog.text = "%d/%d" % [s["done"], s["windows"]]
		prog.custom_minimum_size.x = 62
		row.add_child(prog)
		var info := Label.new()
		info.size_flags_horizontal = SIZE_EXPAND_FILL
		var best: float = s.get("best", 0.0)
		info.text = "%s  best %.6f" % [s["status"], best]
		row.add_child(info)
		var chart_btn := Button.new()
		chart_btn.text = "chart"
		chart_btn.tooltip_text = "load the match-position line as a chart timeline"
		var base := "dtw_%s_%s_%d" % [TF_CHOICES[_tf_a.selected], TF_CHOICES[_tf_b.selected], s["win"]]
		var json_path := "%s/%s.json" % [ProjectSettings.globalize_path(_run_dir()), base]
		chart_btn.disabled = not FileAccess.file_exists(json_path)
		chart_btn.pressed.connect(_on_chart_pressed.bind(base))
		row.add_child(chart_btn)
		_table.add_child(row)

func _on_chart_pressed(base: String) -> void:
	var dir := ProjectSettings.globalize_path(_run_dir())
	var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string("%s/%s.json" % [dir, base]))
	if not parsed is Dictionary:
		_update_status("bad result json for %s" % base)
		return
	var matches_path := "%s/%s.csv" % [dir, base]
	if not FileAccess.file_exists(matches_path):
		_update_status("no matches csv for %s" % base)
		return
	# convert (i,j,dist) into a candles csv with close=j so the chart can
	# plot the match-position line directly
	var pos_path := "%s/posline_%s.csv" % [dir, base]
	var out := FileAccess.open(pos_path, FileAccess.WRITE)
	if out == null:
		_update_status("cannot write %s" % pos_path)
		return
	out.store_line("open,high,low,close,vol")
	var f := FileAccess.open(matches_path, FileAccess.READ)
	f.get_line()
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 2:
			continue
		var j := parts[1].to_float()
		out.store_line("%.5f,%.5f,%.5f,%.5f,0" % [j, j, j, j])
	out.close()
	var chart := get_tree().root.find_child("Chart", true, false)
	if chart == null:
		_update_status("chart tab not found")
		return
	var best: Dictionary = (parsed as Dictionary).get("best", {})
	var label := "%s dist %.6f" % [base, best.get("dist", 0.0)]
	if chart.has_method("add_timeline"):
		chart.add_timeline(label, pos_path)
		_update_status("chart: loaded %s" % label)
	else:
		_update_status("chart has no add_timeline")
