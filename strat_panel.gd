class_name StratPanel
extends Control

signal session_selected(sess_seed: int)
signal results_ready(run_dir: String, results: Dictionary)

const PARAM_FIELDS := [
	["sessions", "sessions", "4"],
	["seed", "seed base", "1"],
	["slots", "rule slots", "4"],
	["candidates", "candidates", "200"],
	["valpct", "val %", "0.2"],
	["fee", "fee", "0.0002"],
	["fastmax", "fast max", "50"],
	["slowmax", "slow max", "200"],
]

const DATA_DIR := "user://stratdata"

var fields := {}
var sim_available := false
var _sim: Variant
var _box: VBoxContainer
var _opt_btn: Button
var _dsl_btn: Button
var _dsl_edit: TextEdit
var _status_label: Label
var _table: VBoxContainer
var _data_edit: LineEdit
var _dialog: FileDialog
var _thread: Thread
var _run_name := ""
var _results := {}
var _poll_ms := -1
var _base_seed := 1
var _session_count := 0
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
	title.text = "Strategy Optimizer"
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
	_data_edit.placeholder_text = "candles csv or .rbin store"
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

	_opt_btn = Button.new()
	_opt_btn.text = "Optimize"
	_opt_btn.pressed.connect(_on_optimize)
	box.add_child(_opt_btn)

	var dsl_label := Label.new()
	dsl_label.text = "DSL strategy (one rule per line: COND -> POS)"
	box.add_child(dsl_label)
	_dsl_edit = TextEdit.new()
	_dsl_edit.custom_minimum_size.y = 70
	_dsl_edit.text = "sma(10)>sma(50) -> 1\nsma(10)<sma(50) -> -1"
	_dsl_edit.placeholder_text = "sma(5)>sma(20) & close>sma(50) -> 1"
	box.add_child(_dsl_edit)
	_dsl_btn = Button.new()
	_dsl_btn.text = "Backtest DSL"
	_dsl_btn.tooltip_text = "evaluate the DSL strategy over the whole dataset (shown as session #0)"
	_dsl_btn.pressed.connect(_on_backtest_dsl)
	box.add_child(_dsl_btn)

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

func _update_status(msg := "") -> void:
	if msg.is_empty():
		_status_label.text = "extension: %s" % ("ok" if sim_available else "MISSING (nix run .#link-sim-extension)")
	else:
		_status_label.text = msg

func _on_browse() -> void:
	_dialog.popup_centered(Vector2i(700, 500))

func _on_data_picked(path: String) -> void:
	_data_edit.text = path

func _on_backtest_dsl() -> void:
	if not sim_available:
		_update_status("cannot backtest: SimNative extension missing")
		return
	var data_path := _data_edit.text.strip_edges()
	if data_path.is_empty() or not FileAccess.file_exists(data_path):
		_update_status("pick an existing candles csv first")
		return
	var spec := _dsl_edit.text.replace(";", "\n").strip_edges()
	if spec.is_empty():
		_update_status("write a DSL strategy first")
		return
	_run_name = "dsl"
	var dir := ProjectSettings.globalize_path(_run_dir())
	DirAccess.make_dir_recursive_absolute(dir)
	var params := "data=%s;out=%s;dsl=%s;fee=%s" % [
		data_path, dir, spec, fields["fee"].text.strip_edges()]
	var code: int = _sim.strat_backtest_dsl(params)
	if code != 0:
		_update_status("DSL backtest failed (code %d): check the strategy syntax" % code)
		return
	var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string("%s/strat_0.json" % dir))
	if parsed is Dictionary:
		_results = {0: parsed}
		results_ready.emit(dir, _results)
		_refresh_table([{"seed": 0, "candidate": 1, "candidates": 1,
			"best": (parsed as Dictionary).get("calmar", 0.0), "status": "done"}])
		session_selected.emit(0)
		_update_status("DSL backtest done: calmar %.4f (%d trades)" % [
			(parsed as Dictionary).get("calmar", 0.0), (parsed as Dictionary).get("trades", 0)])
	else:
		_update_status("DSL backtest produced no result")

func _run_dir() -> String:
	return "%s/%s" % [DATA_DIR, _run_name]

func _params_string() -> String:
	var parts: PackedStringArray = []
	parts.append("data=%s" % _data_edit.text.strip_edges())
	parts.append("out=%s" % ProjectSettings.globalize_path(_run_dir()))
	for key in fields:
		parts.append("%s=%s" % [key, fields[key].text.strip_edges()])
	return ";".join(parts)

func _on_optimize() -> void:
	if _thread != null:
		_sim.strat_cancel()
		_cancel_requested = true
		_update_status("cancelling…")
		return
	if not sim_available:
		_update_status("cannot optimize: SimNative extension missing")
		return
	var data_path := _data_edit.text.strip_edges()
	if data_path.is_empty() or not FileAccess.file_exists(data_path):
		_update_status("pick an existing candles csv first")
		return
	_run_name = "run%d" % (Time.get_ticks_msec() % 100000)
	_base_seed = fields["seed"].text.to_int()
	_session_count = maxi(1, fields["sessions"].text.to_int())
	_results = {}
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(_run_dir()))
	_opt_btn.text = "Cancel"
	_thread = Thread.new()
	var start_err: int = _thread.start(Callable(_sim, "strat_optimize").bind(_params_string()))
	if start_err != OK:
		_thread = null
		_opt_btn.text = "Optimize"
		_update_status("failed to start optimizer thread (code %d)" % start_err)
		return
	set_process(true)

func _process(_delta: float) -> void:
	if _thread == null:
		set_process(false)
		return
	var now := Time.get_ticks_msec()
	if now - _poll_ms > 500:
		_poll_ms = now
		_update_status("optimizing %s… %d%%" % [_run_name, int(_sim.strat_progress() * 100)])
		var parsed: Variant = JSON.parse_string(_sim.strat_sessions())
		if parsed is Array:
			_refresh_table(parsed)
	if _thread.is_alive():
		return
	_thread.wait_to_finish()
	_thread = null
	set_process(false)
	_opt_btn.text = "Optimize"
	_load_results()
	results_ready.emit(ProjectSettings.globalize_path(_run_dir()), _results)
	var parsed: Variant = JSON.parse_string(_sim.strat_sessions())
	if parsed is Array:
		_refresh_table(parsed)
	var best := -INF
	var best_seed := 0
	for sess_seed in _results:
		var m: Dictionary = _results[sess_seed]
		if m["calmar"] > best:
			best = m["calmar"]
			best_seed = sess_seed
	if best_seed > 0:
		_update_status("done; best seed %d (calmar %.4f)" % [best_seed, best])
	elif _cancel_requested:
		_update_status("cancelled")
	else:
		_update_status("done (no session produced a result)")
	_cancel_requested = false

func _load_results() -> void:
	for k in _session_count:
		var sess_seed := _base_seed + k
		var p := "%s/strat_%d.json" % [ProjectSettings.globalize_path(_run_dir()), sess_seed]
		if not FileAccess.file_exists(p):
			continue
		var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string(p))
		if parsed is Dictionary:
			_results[sess_seed] = parsed

func _refresh_table(live: Array) -> void:
	for c in _table.get_children():
		c.queue_free()
	for entry in live:
		var s: Dictionary = entry
		var sess_seed: int = s["seed"]
		var row := HBoxContainer.new()
		var lab := Label.new()
		lab.text = "#%d" % sess_seed
		lab.custom_minimum_size.x = 34
		row.add_child(lab)
		var prog := Label.new()
		prog.text = "%d/%d" % [s["candidate"], s["candidates"]]
		prog.custom_minimum_size.x = 56
		row.add_child(prog)
		var info := Label.new()
		info.size_flags_horizontal = SIZE_EXPAND_FILL
		if _results.has(sess_seed):
			var m: Dictionary = _results[sess_seed]
			info.text = "%s  calmar %.4f  pnl %.4f (%d trades)" % [
				m["slots"], m["calmar"], m["pnl"], m["trades"]]
		else:
			info.text = "%s  best %.4f" % [s["status"], s["best"]]
		row.add_child(info)
		var plot_btn := Button.new()
		plot_btn.text = "plot"
		plot_btn.tooltip_text = "show this session in the page plots"
		plot_btn.disabled = not _results.has(sess_seed)
		plot_btn.pressed.connect(_on_plot_pressed.bind(sess_seed))
		row.add_child(plot_btn)
		var trades_btn := Button.new()
		trades_btn.text = "trades"
		trades_btn.tooltip_text = "show this run's trades on the chart tab"
		var trades_path := "%s/trades_%d.bin" % [ProjectSettings.globalize_path(_run_dir()), sess_seed]
		trades_btn.disabled = not _results.has(sess_seed) or not FileAccess.file_exists(trades_path)
		trades_btn.pressed.connect(_on_trades_pressed.bind(sess_seed))
		row.add_child(trades_btn)
		_table.add_child(row)

func _on_plot_pressed(sess_seed: int) -> void:
	session_selected.emit(sess_seed)

func _on_trades_pressed(sess_seed: int) -> void:
	var path := "%s/trades_%d.bin" % [ProjectSettings.globalize_path(_run_dir()), sess_seed]
	var chart := get_tree().root.find_child("Chart", true, false)
	if chart == null:
		_update_status("chart tab not found")
		return
	var n: int = chart.add_trades_from_file(path, "strat#%d" % sess_seed)
	if n < 0:
		_update_status("failed to load trades from %s" % path)
	else:
		_update_status("chart: %d trades from seed %d (see the Chart tab)" % [n, sess_seed])
