class_name DtwPanel
extends Control

signal plot_requested(dir: String, base: String, meta: Dictionary)
signal overlay_requested(dir: String, base: String, meta: Dictionary)
signal drill_done(d: Dictionary)

# DTW correlation scan panel. File mode: scan the two picked files as one
# (tfA, tfB) pair. Grid mode: pick two series loaded in the chart — every
# timeframe of A is scanned against every timeframe of B in one batch
# (store-backed series use their per-TF rbin caches, sim series are
# materialized to temp csvs). Artifacts land in user://dtwdata/<run>/.

const TFS: Array[String] = QuoteStore.TFS

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
var _series_a: OptionButton
var _series_b: OptionButton
var _dialog: FileDialog
var _load_dialog: FileDialog
var _picking_for := ""
var _thread: Thread
var _run_name := ""
var _poll_ms := -1
var _cancel_requested := false
# last scan's data paths for drill-downs (file mode: the two pickers;
# grid mode: per-TF materialized paths)
var _last_grid := false
var _paths_a := {}
var _paths_b := {}

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

	var sa_label := Label.new()
	sa_label.text = "series A"
	grid.add_child(sa_label)
	_series_a = OptionButton.new()
	grid.add_child(_series_a)

	var sb_label := Label.new()
	sb_label.text = "series B"
	grid.add_child(sb_label)
	_series_b = OptionButton.new()
	grid.add_child(_series_b)

	_data_a = _add_data_row(grid, "series A csv", "a")
	_data_b = _add_data_row(grid, "series B csv", "b")

	var tfa_label := Label.new()
	tfa_label.text = "tf A (file mode)"
	grid.add_child(tfa_label)
	_tf_a = OptionButton.new()
	for t in TFS:
		_tf_a.add_item(t)
	grid.add_child(_tf_a)

	var tfb_label := Label.new()
	tfb_label.text = "tf B (file mode)"
	grid.add_child(tfb_label)
	_tf_b = OptionButton.new()
	for t in TFS:
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

	var load_btn := Button.new()
	load_btn.text = "Load results…"
	load_btn.tooltip_text = "open a previous run's dtw_*.json into the plots"
	load_btn.pressed.connect(_on_load_results)
	box.add_child(load_btn)
	var clear_ov_btn := Button.new()
	clear_ov_btn.text = "Clear chart overlay"
	clear_ov_btn.tooltip_text = "remove the matched-window overlay from the Chart tab (same as the x on the chart)"
	clear_ov_btn.pressed.connect(_on_clear_overlay)
	box.add_child(clear_ov_btn)
	_load_dialog = FileDialog.new()
	_load_dialog.file_mode = FileDialog.FILE_MODE_OPEN_FILE
	_load_dialog.access = FileDialog.ACCESS_FILESYSTEM
	_load_dialog.filters = PackedStringArray(["*.json ; DTW session summary"])
	_load_dialog.file_selected.connect(_on_results_picked)
	add_child(_load_dialog)

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
	_refresh_series_pickers()

func _chart() -> Variant:
	return get_tree().root.find_child("Chart", true, false)

func _refresh_series_pickers() -> void:
	for ob in [_series_a, _series_b]:
		ob.clear()
		ob.add_item("(file below)")
	var chart: Variant = _chart()
	if chart == null or not chart.has_method("get"):
		return
	var data: Variant = chart.get("data")
	if data == null:
		return
	for s in data.series:
		_series_a.add_item(s.name)
		_series_b.add_item(s.name)

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

func _on_load_results() -> void:
	# always start from the results dir, not wherever the last dialog went
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(DATA_DIR))
	_load_dialog.current_dir = ProjectSettings.globalize_path(DATA_DIR)
	_load_dialog.popup_centered(Vector2i(700, 500))

func _on_clear_overlay() -> void:
	var chart: Variant = _chart()
	if chart != null and chart.has_method("clear_dtw_overlay"):
		chart.clear_dtw_overlay()
		_update_status("chart overlay cleared")

func _on_results_picked(path: String) -> void:
	# a picked dtw_<tfA>_<tfB>_<win>.json identifies the run dir + session
	var file := path.get_file()
	if not file.begins_with("dtw_") or not file.ends_with(".json"):
		_update_status("pick a dtw_*.json file")
		return
	var dir := path.get_base_dir()
	var base := file.substr(0, file.length() - 5)
	var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string(path))
	if not parsed is Dictionary:
		_update_status("bad json: %s" % path)
		return
	# remember the run so chart/plot buttons resolve against it
	var rel := dir.trim_prefix(ProjectSettings.globalize_path(DATA_DIR) + "/")
	_run_name = rel if not rel.is_empty() else _run_name
	_last_grid = false
	plot_requested.emit(dir, base, parsed)

func _grid_mode() -> bool:
	return _series_a.selected > 0 and _series_b.selected > 0

# resolve one chart series into {tf: data path} for every timeframe.
# store-backed series reuse the per-TF rbin caches (building missing ones
# down the aggregation chain); sim series are materialized to temp csvs.
func _materialize_series(s: PlotData.Series, tmp_dir: String) -> Dictionary:
	var out := {}
	if s.store != null:
		var is_rbin: bool = s.store_key.to_lower().ends_with(".rbin")
		for tf in TFS:
			var p := ""
			if is_rbin:
				p = s.store_key if tf == "1m" else QuoteStore.cache_path(s.store_key, tf)
				if tf != "1m" and not FileAccess.file_exists(p):
					var pf := FileAccess.open(s.store_key, FileAccess.READ)
					if pf == null:
						continue
					var ph := QuoteStore._peek_header(pf)
					pf.close()
					if ph.is_empty():
						continue
					var chain: Array[String] = []
					var t := tf
					while t != "1m":
						chain.push_front(t)
						t = QuoteStore._parent_tf(t)
					var parent := s.store_key
					var ok := true
					for ct in chain:
						var cpath := QuoteStore.cache_path(s.store_key, ct)
						_update_status("building %s for %s…" % [ct, s.name])
						var err: int = await QuoteStore.new()._aggregate_tf_async(parent, cpath, ct, ph["dec"], Callable())
						if err != OK:
							ok = false
							break
						parent = cpath
					if not ok:
						continue
			else:
				p = QuoteStore.cache_path(s.store_key, tf)
				if not FileAccess.file_exists(p):
					_update_status("building %s for %s…" % [tf, s.name])
					var err2: int = await QuoteStore.new().ensure_tf_async(s.store_key, tf, Callable())
					if err2 != OK:
						continue
			if FileAccess.file_exists(p):
				out[tf] = ProjectSettings.globalize_path(p)
	else:
		var src: Array[Candle] = s._base if not s._base.is_empty() else s.candles
		for tf in TFS:
			var g: int = QuoteStore.TF_MINUTES[tf]
			var p2 := "%s/%s_%s.csv" % [tmp_dir, s.name.validate_filename(), tf]
			if _write_agg_csv(src, g, p2):
				out[tf] = ProjectSettings.globalize_path(p2)
	return out

# chunked sim aggregation into a candles csv; false when nothing written
func _write_agg_csv(src: Array[Candle], g: int, path: String) -> bool:
	if src.is_empty():
		return false
	var f := FileAccess.open(path, FileAccess.WRITE)
	if f == null:
		return false
	f.store_line("open,high,low,close,vol")
	var i := 0
	var buf := PackedStringArray()
	while i < src.size():
		var c := src[i]
		var hi := c.high
		var lo := c.low
		var cl := c.close
		var vol: int = c.vol
		var last := mini(i + g - 1, src.size() - 1)
		for j in range(i + 1, last + 1):
			hi = maxf(hi, src[j].high)
			lo = minf(lo, src[j].low)
			cl = src[j].close
			vol += src[j].vol
		buf.append("%.5f,%.5f,%.5f,%.5f,%d" % [c.open, hi, lo, cl, vol])
		if buf.size() >= 4096:
			f.store_string("\n".join(buf) + "\n")
			buf.clear()
		i += g
	if not buf.is_empty():
		f.store_string("\n".join(buf) + "\n")
	f.close()
	return i > 0

func _run_dir() -> String:
	return "%s/%s" % [DATA_DIR, _run_name]

func _params_string(paths_a: Dictionary, paths_b: Dictionary) -> String:
	var parts: PackedStringArray = []
	if _grid_mode():
		var ta: PackedStringArray = []
		var tb: PackedStringArray = []
		var pa: PackedStringArray = []
		var pb: PackedStringArray = []
		for tf in TFS:
			ta.append(tf)
			tb.append(tf)
			pa.append(paths_a.get(tf, ""))
			pb.append(paths_b.get(tf, ""))
		parts.append("tfs=%s" % ",".join(ta))
		parts.append("dataa=%s" % ",".join(pa))
		parts.append("datab=%s" % ",".join(pb))
	else:
		parts.append("dataa=%s" % _data_a.text.strip_edges())
		parts.append("datab=%s" % _data_b.text.strip_edges())
		parts.append("atf=%s" % TFS[_tf_a.selected])
		parts.append("btf=%s" % TFS[_tf_b.selected])
	parts.append("out=%s" % ProjectSettings.globalize_path(_run_dir()))
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
	_run_scan()

func _run_scan() -> void:
	var paths_a := {}
	var paths_b := {}
	if _grid_mode():
		_refresh_series_pickers()
		if _series_a.selected <= 0 or _series_b.selected <= 0:
			_update_status("pick chart series for both A and B (or use file mode)")
			return
		var chart: Variant = _chart()
		var data: Variant = chart.get("data")
		if data == null:
			_update_status("chart has no data")
			return
		var sa: Array = data.series
		var s_a: PlotData.Series = sa[_series_a.selected - 1]
		var s_b: PlotData.Series = sa[_series_b.selected - 1]
		_run_name = "grid%d" % (Time.get_ticks_msec() % 100000)
		var tmp := ProjectSettings.globalize_path("%s/%s" % [DATA_DIR, _run_name])
		DirAccess.make_dir_recursive_absolute(tmp)
		_update_status("materializing %s…" % s_a.name)
		paths_a = await _materialize_series(s_a, tmp)
		_update_status("materializing %s…" % s_b.name)
		paths_b = await _materialize_series(s_b, tmp)
		_last_grid = true
		_paths_a = paths_a
		_paths_b = paths_b
		if paths_a.is_empty() or paths_b.is_empty():
			_update_status("could not materialize chart series")
			return
	else:
		if _data_a.text.strip_edges().is_empty() or not FileAccess.file_exists(_data_a.text.strip_edges()):
			_update_status("pick an existing series A file first")
			return
		if _data_b.text.strip_edges().is_empty() or not FileAccess.file_exists(_data_b.text.strip_edges()):
			_update_status("pick an existing series B file first")
			return
		_run_name = "run%d" % (Time.get_ticks_msec() % 100000)
		_last_grid = false
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(_run_dir()))
	_update_status("starting scan %s…" % _run_name)
	_scan_btn.text = "Cancel"
	_thread = Thread.new()
	var start_err: int = _thread.start(Callable(_sim, "dtw_scan").bind(_params_string(paths_a, paths_b)))
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
	_annotate_run_paths()

# stamp each session json with the data paths used, so drill-downs work
# when the run is reloaded later (grid runs especially)
func _annotate_run_paths() -> void:
	var dir := ProjectSettings.globalize_path(_run_dir())
	var fa := _data_a.text.strip_edges()
	var fb := _data_b.text.strip_edges()
	var d := DirAccess.open(dir)
	if d == null:
		return
	for file in d.get_files():
		if not file.begins_with("dtw_") or not file.ends_with(".json"):
			continue
		var p := "%s/%s" % [dir, file]
		var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string(p))
		if not parsed is Dictionary:
			continue
		var data_a := fa
		var data_b := fb
		if _last_grid:
			var tfA := str((parsed as Dictionary).get("tfA", ""))
			var tfB := str((parsed as Dictionary).get("tfB", ""))
			data_a = str(_paths_a.get(tfA, ""))
			data_b = str(_paths_b.get(tfB, ""))
		if data_a.is_empty() or data_b.is_empty():
			continue
		(parsed as Dictionary)["dataa"] = data_a
		(parsed as Dictionary)["datab"] = data_b
		var out := FileAccess.open(p, FileAccess.WRITE)
		if out != null:
			out.store_string(JSON.stringify(parsed))
			out.close()

func _refresh_table(live: Array) -> void:
	for c in _table.get_children():
		c.queue_free()
	for entry in live:
		var s: Dictionary = entry
		var row := HBoxContainer.new()
		var lab := Label.new()
		lab.text = "%s/%s w%d" % [s.get("tfA", "?"), s.get("tfB", "?"), s["win"]]
		lab.custom_minimum_size.x = 90
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
		chart_btn.tooltip_text = "load the match-offset line as a chart timeline"
		var base := "dtw_%s_%s_%d" % [s.get("tfA", ""), s.get("tfB", ""), s["win"]]
		var json_path := "%s/%s.json" % [ProjectSettings.globalize_path(_run_dir()), base]
		chart_btn.disabled = not FileAccess.file_exists(json_path)
		chart_btn.pressed.connect(_on_chart_pressed.bind(base))
		row.add_child(chart_btn)
		var plot_btn := Button.new()
		plot_btn.text = "plot"
		plot_btn.tooltip_text = "show the alignment scatter / dist line / drill-down in the page plots"
		plot_btn.disabled = not FileAccess.file_exists(json_path)
		plot_btn.pressed.connect(_on_plot_pressed.bind(base))
		row.add_child(plot_btn)
		var ov_btn := Button.new()
		ov_btn.text = "overlay"
		ov_btn.tooltip_text = "mark matched windows on the Chart tab's price graph (click a rect to drill)"
		ov_btn.disabled = not FileAccess.file_exists(json_path)
		ov_btn.pressed.connect(_on_overlay_pressed.bind(base))
		row.add_child(ov_btn)
		_table.add_child(row)

func _on_overlay_pressed(base: String) -> void:
	var dir := ProjectSettings.globalize_path(_run_dir())
	var jp := "%s/%s.json" % [dir, base]
	if not FileAccess.file_exists(jp):
		return
	var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string(jp))
	if parsed is Dictionary:
		overlay_requested.emit(dir, base, parsed)

func _on_plot_pressed(base: String) -> void:
	var dir := ProjectSettings.globalize_path(_run_dir())
	var jp := "%s/%s.json" % [dir, base]
	if not FileAccess.file_exists(jp):
		return
	var parsed: Variant = JSON.parse_string(FileAccess.get_file_as_string(jp))
	if parsed is Dictionary:
		plot_requested.emit(dir, base, parsed)

# drill-down for one matched window: FFI returns the warping path plus the
# two z-normalized windows; runs on a thread (first call pays the load)
func _on_drill(i: int, j: int, meta: Dictionary) -> void:
	if not sim_available:
		return
	var tfA := str(meta.get("tfA", ""))
	var tfB := str(meta.get("tfB", ""))
	var data_a := str(meta.get("dataa", ""))
	var data_b := str(meta.get("datab", ""))
	if data_a.is_empty() or data_b.is_empty():
		if _last_grid:
			data_a = str(_paths_a.get(tfA, ""))
			data_b = str(_paths_b.get(tfB, ""))
		else:
			data_a = _data_a.text.strip_edges()
			data_b = _data_b.text.strip_edges()
	if data_a.is_empty() or data_b.is_empty():
		_update_status("drill-down: data paths unavailable for %s/%s" % [tfA, tfB])
		return
	_drill_async("dataa=%s;datab=%s;i=%d;j=%d;win=%d;band=%d" % [
		data_a, data_b, i, j,
		int(meta.get("win", 128)), int(meta.get("band", 32))], i, j)

func _drill_async(params: String, i: int, j: int) -> void:
	_update_status("drill-down…")
	var t := Thread.new()
	t.start(Callable(_sim, "dtw_path").bind(params))
	while t.is_alive():
		await get_tree().process_frame
	var res: String = t.wait_to_finish()
	var parsed: Variant = JSON.parse_string(res)
	if parsed is Dictionary:
		(parsed as Dictionary)["i"] = i
		(parsed as Dictionary)["j"] = j
		drill_done.emit(parsed)
		_update_status("")
	else:
		drill_done.emit({"error": "bad drill json"})

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
	# convert (i,j,dist) into a candles csv charting the LEAD/LAG OFFSET
	# j-i per A window (constant offset = stable correlation; slope =
	# relative speed). One row per stepped A window so the x-axis stays
	# aligned with A; unmatched windows carry the previous offset (the
	# matches csv omits j=-1 rows, which would otherwise compress the axis)
	var pos_path := "%s/posline_%s.csv" % [dir, base]
	var out := FileAccess.open(pos_path, FileAccess.WRITE)
	if out == null:
		_update_status("cannot write %s" % pos_path)
		return
	out.store_line("open,high,low,close,vol")
	var by_i := {}
	var f := FileAccess.open(matches_path, FileAccess.READ)
	f.get_line()
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 2:
			continue
		by_i[parts[0].to_int()] = parts[1].to_float()
	# step: smallest positive gap between consecutive sorted i (windows
	# are stepped regularly, so the min gap recovers the step even with
	# unmatched windows missing from the csv)
	var keys := by_i.keys()
	keys.sort()
	var step := 0
	for k in range(1, keys.size()):
		var d: int = keys[k] - keys[k - 1]
		if d > 0 and (step == 0 or d < step):
			step = d
	if step == 0:
		step = 1
	# one row per stepped A window starting at 0: the x-axis aligns with
	# A's candle indices; unmatched windows carry the previous offset
	var last := 0.0
	var n_win: int = int((parsed as Dictionary).get("windows", 0))
	var buf := PackedStringArray()
	for k in n_win:
		var i: int = k * step
		if by_i.has(i):
			last = by_i[i] - i
		buf.append("%.5f,%.5f,%.5f,%.5f,0" % [last, last, last, last])
		if buf.size() >= 4096:
			out.store_string("\n".join(buf) + "\n")
			buf.clear()
	if not buf.is_empty():
		out.store_string("\n".join(buf) + "\n")
	out.close()
	var chart: Variant = _chart()
	if chart == null:
		_update_status("chart tab not found")
		return
	var best: Dictionary = (parsed as Dictionary).get("best", {})
	var label := "%s offset dist %.6f" % [base, best.get("dist", 0.0)]
	if chart.has_method("add_timeline"):
		chart.add_timeline(label, pos_path)
		_update_status("chart: loaded %s" % label)
	else:
		_update_status("chart has no add_timeline")
