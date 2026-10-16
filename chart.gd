extends Control

const PlotDataScript := preload("res://plot_data.gd")

const ZOOM_FACTOR := 1.2
const VOL_GRIP := 4.0
const VOL_SNAP := 0.02
const VOL_MAX_FRAC := 0.6
const RMB_CLICK_TOL := 4.0

@export var replay_rate := 60.0

var view := ChartView.new()
var data: PlotData = PlotDataScript.new()

var vol_frac := 0.2
var vol_collapsed := false
var vol_dragging := false

var head := -1
var playing := false
var paused := false
var pan_mode := false
var t_acc := 0.0
var _rmb_down := false
var _rmb_down_pos := Vector2.ZERO
var _fft_drag: Array = []
var _menu: PopupPanel
var _menu_box: VBoxContainer
var _fft_spins := {}
var _opt_rmse_pct := 50
var _save_dialog: FileDialog
var _save_series_idx := -1

const PALETTE: Array[Color] = [
	Color(0.56, 0.93, 0.56),
	Color(0.96, 0.64, 0.38),
	Color(0.47, 0.71, 0.97),
	Color(0.93, 0.45, 0.66),
	Color(0.72, 0.56, 0.97),
	Color(0.93, 0.88, 0.42),
]

@onready var grid: GridWidget = $Grid
@onready var series_widget: SeriesWidget = $Series
@onready var indicator_widget: IndicatorWidget = $Indicators
@onready var volume_widget: VolumeWidget = $Volume
@onready var book_widget: OrderBookWidget = $Book
@onready var cross_widget: CrossWidget = $Cross
@onready var cursor_widget: CursorWidget = $Cursor
@onready var sim_window: ToolWindow = $SimWindow
@onready var sim_panel: SimPanel = $SimWindow/Box/SimPanel
@onready var ml_window: ToolWindow = $MlWindow
@onready var ml_panel: MlPanel = $MlWindow/Box/MlPanel

func _ready() -> void:
	for w in [grid, series_widget, indicator_widget, volume_widget, book_widget, cross_widget, cursor_widget]:
		w.view = view
		w.data = data
	_sync_volume()
	_sync_mode_hud()
	_sync_indicator_hud()
	cursor_widget.cross = cross_widget
	cursor_widget.indicators = indicator_widget
	view.layout(size)
	sim_panel.timeline_added.connect(_on_timeline_added)
	sim_panel.timeline_removed.connect(_on_timeline_removed)
	sim_panel.timeline_toggled.connect(_on_timeline_toggled)
	ml_panel.timeline_added.connect(_on_timeline_added)
	_menu = PopupPanel.new()
	var panel := PanelContainer.new()
	_menu_box = VBoxContainer.new()
	panel.add_child(_menu_box)
	_menu.add_child(panel)
	add_child(_menu)

func _notification(what: int) -> void:
	if what == NOTIFICATION_RESIZED:
		view.layout(size)
		_sync_volume()
		_refresh()

func _input(event: InputEvent) -> void:
	if event.is_action_pressed("pause"):
		paused = not paused
	elif event.is_action_pressed("speed_up"):
		replay_rate *= 2.0
	elif event.is_action_pressed("speed_down"):
		replay_rate = maxf(replay_rate / 2.0, 1.0)
	elif event.is_action_pressed("pan_mode"):
		pan_mode = true
	elif event.is_action_released("pan_mode"):
		pan_mode = false
	elif event is InputEventMouseButton:
		var button := event as InputEventMouseButton
		if button.pressed and not _over_ui(button.position):
			if button.button_index == MOUSE_BUTTON_WHEEL_UP:
				view.zoom_x_at(button.position, ZOOM_FACTOR)
				_refresh()
			elif button.button_index == MOUSE_BUTTON_WHEEL_DOWN:
				view.zoom_x_at(button.position, 1.0 / ZOOM_FACTOR)
				_refresh()
			elif button.button_index == MOUSE_BUTTON_LEFT:
				var hit: Array = indicator_widget.fft_handle_at(button.position)
				if not hit.is_empty():
					_begin_fft_drag(hit)
				elif button.double_click and _near_vol_grip(button.position):
					_toggle_volume()
				elif _near_vol_grip(button.position):
					vol_dragging = true
			elif button.button_index == MOUSE_BUTTON_RIGHT:
				_rmb_down = true
				_rmb_down_pos = button.position
		elif not button.pressed:
			if button.button_index == MOUSE_BUTTON_LEFT:
				vol_dragging = false
				if not _fft_drag.is_empty():
					_fft_drag = []
					Input.set_default_cursor_shape(Input.CURSOR_ARROW)
				if vol_frac < VOL_SNAP:
					vol_collapsed = true
				_sync_volume()
			elif button.button_index == MOUSE_BUTTON_RIGHT:
				if _rmb_down and button.position.distance_to(_rmb_down_pos) < RMB_CLICK_TOL:
					_open_popup(button.position)
				_rmb_down = false
	elif event is InputEventMouseMotion:
		var motion := event as InputEventMouseMotion
		cross_widget.mouse = motion.position
		if not _fft_drag.is_empty():
			_drag_fft_to(motion.position)
		elif vol_dragging:
			var plot_h := maxf(view.plot_rect.size.y, 1.0)
			vol_collapsed = false
			vol_frac = clampf((view.plot_rect.end.y - motion.position.y) / plot_h, 0.0, VOL_MAX_FRAC)
			_sync_volume()
		elif not sim_window.is_dragging() and not ml_window.is_dragging() and not _over_ui(motion.position):
			if motion.button_mask & MOUSE_BUTTON_MASK_RIGHT != 0:
				view.scale_by(motion.relative, motion.position)
				view.follow = false
			elif motion.button_mask & (MOUSE_BUTTON_MASK_LEFT | MOUSE_BUTTON_MASK_MIDDLE) != 0 or pan_mode:
				view.pan_by(motion.relative)
				view.follow = false
		_refresh()

func _over_ui(pos: Vector2) -> bool:
	return (sim_window.visible and sim_window.get_global_rect().has_point(pos)) \
		or (ml_window.visible and ml_window.get_global_rect().has_point(pos))

func _unhandled_key_input(event: InputEvent) -> void:
	var key := event as InputEventKey
	if key == null or not key.pressed or key.echo:
		return
	if key.keycode == KEY_M:
		series_widget.cycle_mode()
		_sync_mode_hud()
		_refresh()
	elif key.keycode == KEY_I:
		indicator_widget.enabled = not indicator_widget.enabled
		_sync_indicator_hud()
		_refresh()
	elif key.keycode == KEY_F1:
		sim_window.toggle()
	elif key.keycode == KEY_F2:
		ml_window.toggle()
	elif key.keycode == KEY_V:
		_toggle_volume()

func vol_pane_h() -> float:
	if vol_collapsed:
		return 0.0
	return view.plot_rect.size.y * vol_frac

func _near_vol_grip(pos: Vector2) -> bool:
	var r := view.plot_rect
	return pos.x >= r.position.x and pos.x <= r.end.x \
		and absf(pos.y - (r.end.y - vol_pane_h())) <= VOL_GRIP

func _toggle_volume() -> void:
	vol_collapsed = not vol_collapsed
	if not vol_collapsed and vol_frac < VOL_SNAP:
		vol_frac = 0.2
	_sync_volume()

func _sync_volume() -> void:
	if volume_widget == null:
		return
	volume_widget.pane_h = vol_pane_h()
	volume_widget.collapsed = vol_collapsed
	indicator_widget.vol_pane_h = vol_pane_h()
	_refresh()

func _sync_mode_hud() -> void:
	cursor_widget.mode_text = SeriesWidget.MODE_NAMES[series_widget.mode]

func _sync_indicator_hud() -> void:
	if not indicator_widget.enabled:
		cursor_widget.indicators_text = "off"
		return
	var parts: Array[String] = []
	for ind in indicator_widget.indicators:
		if ind.enabled:
			parts.append(_ind_label(ind))
	cursor_widget.indicators_text = ", ".join(parts)

func _ind_label(ind: IndicatorWidget.Indicator) -> String:
	if ind.kind == "FFT":
		var w := "x%d" % ind.waves if ind.waves > 0 else ""
		var sc := " (%.3f)" % ind.opt_score if ind.opt_score >= 0.0 else ""
		return "FFT [%d..%d]+%d%s%s" % [ind.start, ind.end, ind.predict, w, sc]
	if ind.kind == "KS":
		return "KS (%d, m%d)" % [ind.period, ind.m]
	if ind.kind == "TEMP":
		return "Temp (%d, m%d)" % [ind.period, ind.m]
	if ind.kind == "CECP":
		return "CECP (%d, m%d)" % [ind.period, ind.m]
	if ind.kind == "OBVOL":
		return "OBVOL %s%s" % ["snap" if ind.obvol_mode == 1 else "life", " b%g" % ind.bucket if ind.bucket > 0.0 else ""]
	return "%s (%d)" % [ind.kind, ind.period]

func _open_popup(at: Vector2) -> void:
	_rebuild_menu()
	_menu.position = (get_screen_position() + at).round()
	_menu.popup()

func _rebuild_menu() -> void:
	for c in _menu_box.get_children():
		_menu_box.remove_child(c)
		c.queue_free()
	_fft_spins.clear()
	_add_section("chart mode")
	var modes := OptionButton.new()
	for mname in SeriesWidget.MODE_NAMES:
		modes.add_item(mname)
	modes.selected = series_widget.mode
	modes.item_selected.connect(_on_menu_mode)
	_menu_box.add_child(modes)
	_add_section("indicators")
	if indicator_widget.indicators.is_empty():
		var none := Label.new()
		none.text = "(none)"
		_menu_box.add_child(none)
	for i in indicator_widget.indicators.size():
		_build_indicator_row(i)
	var add_row := HBoxContainer.new()
	var b_sma := Button.new()
	b_sma.text = "add SMA"
	b_sma.pressed.connect(_on_add_sma)
	var b_fft := Button.new()
	b_fft.text = "add FFT"
	b_fft.pressed.connect(_on_add_fft)
	var b_ks := Button.new()
	b_ks.text = "add KS"
	b_ks.pressed.connect(_on_add_ks)
	var b_temp := Button.new()
	b_temp.text = "add TEMP"
	b_temp.pressed.connect(_on_add_temp)
	var b_cecp := Button.new()
	b_cecp.text = "add CECP"
	b_cecp.pressed.connect(_on_add_cecp)
	var b_obvol := Button.new()
	b_obvol.text = "add OBVOL"
	b_obvol.pressed.connect(_on_add_obvol)
	add_row.add_child(b_sma)
	add_row.add_child(b_fft)
	add_row.add_child(b_ks)
	add_row.add_child(b_temp)
	add_row.add_child(b_cecp)
	add_row.add_child(b_obvol)
	_menu_box.add_child(add_row)
	if not data.series.is_empty():
		_add_section("series")
		for i in data.series.size():
			_build_series_row(i)

func _add_section(title: String) -> void:
	if _menu_box.get_child_count() > 0:
		_menu_box.add_child(HSeparator.new())
	var lbl := Label.new()
	lbl.text = title
	_menu_box.add_child(lbl)

func _add_spin(grid: GridContainer, label: String, value: float, on_change: Callable, step := 1.0, max_value := 99999999.0) -> SpinBox:
	var lbl := Label.new()
	lbl.text = label
	lbl.custom_minimum_size = Vector2(64, 0)
	grid.add_child(lbl)
	var spin := SpinBox.new()
	spin.step = step
	spin.min_value = 0
	spin.max_value = max_value
	spin.alignment = HORIZONTAL_ALIGNMENT_RIGHT
	spin.custom_minimum_size = Vector2(96, 0)
	spin.value = value
	spin.value_changed.connect(on_change)
	spin.gui_input.connect(_on_spin_wheel.bind(spin))
	grid.add_child(spin)
	return spin

func _on_spin_wheel(event: InputEvent, spin: SpinBox) -> void:
	if event is InputEventMouseButton and event.pressed:
		if event.button_index == MOUSE_BUTTON_WHEEL_UP:
			spin.value += spin.step
		elif event.button_index == MOUSE_BUTTON_WHEEL_DOWN:
			spin.value = maxf(spin.min_value, spin.value - spin.step)

func _build_indicator_row(i: int) -> void:
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[i]
	var row := HBoxContainer.new()
	var cb := CheckBox.new()
	cb.text = ind.kind
	cb.button_pressed = ind.enabled
	cb.toggled.connect(_on_menu_ind_toggle.bind(i))
	row.add_child(cb)
	if ind.kind == "FFT":
		var lock := CheckBox.new()
		lock.text = "lock"
		lock.button_pressed = ind.locked
		lock.toggled.connect(_on_menu_fft_lock.bind(i))
		row.add_child(lock)
		var opt := Button.new()
		opt.text = "opt"
		opt.tooltip_text = "search best window/waves for the prediction length"
		opt.pressed.connect(_on_menu_fft_optimize.bind(i))
		row.add_child(opt)
	if ind.kind == "OBVOL":
		var mode := OptionButton.new()
		mode.add_item("life")
		mode.add_item("snap")
		mode.selected = ind.obvol_mode
		mode.item_selected.connect(_on_menu_obvol_mode.bind(i))
		row.add_child(mode)
	var del := Button.new()
	del.text = "x"
	del.tooltip_text = "delete"
	del.pressed.connect(_on_menu_ind_delete.bind(i))
	row.add_child(del)
	_menu_box.add_child(row)
	var grid := GridContainer.new()
	grid.columns = 2
	if ind.kind == "FFT":
		_fft_spins["start"] = _add_spin(grid, "start", ind.start, _on_menu_fft_value.bind(i, "start"))
		_fft_spins["end"] = _add_spin(grid, "end", ind.end, _on_menu_fft_value.bind(i, "end"))
		_fft_spins["predict"] = _add_spin(grid, "predict", ind.predict, _on_menu_fft_value.bind(i, "predict"))
		_fft_spins["waves"] = _add_spin(grid, "waves", ind.waves, _on_menu_fft_value.bind(i, "waves"))
		_fft_spins["edge w"] = _add_spin(grid, "edge w", ind.edge_w, _on_menu_fft_value.bind(i, "edge_w"), 1.0, 1000.0)
		_add_spin(grid, "opt rmse %", _opt_rmse_pct, _on_menu_opt_weight)
	elif ind.kind == "KS" or ind.kind == "TEMP" or ind.kind == "CECP":
		_add_spin(grid, "period", ind.period, _on_menu_sma_period.bind(i))
		_add_spin(grid, "m", ind.m, _on_menu_ks_m.bind(i))
	elif ind.kind == "OBVOL":
		_add_spin(grid, "bucket", ind.bucket, _on_menu_obvol_bucket.bind(i), 0.00001)
	else:
		_add_spin(grid, "period", ind.period, _on_menu_sma_period.bind(i))
	if grid.get_child_count() > 0:
		_menu_box.add_child(grid)

func _build_series_row(i: int) -> void:
	var s: PlotData.Series = data.series[i]
	var row := HBoxContainer.new()
	var lbl := Label.new()
	lbl.text = s.name
	lbl.custom_minimum_size = Vector2(64, 0)
	row.add_child(lbl)
	var up := _color_button(s.body_up if s.body_up.a > 0.0 else s.col_pos)
	up.color_changed.connect(_on_menu_series_color.bind(i, "body_up"))
	row.add_child(up)
	var down := _color_button(s.body_down if s.body_down.a > 0.0 else s.col_neg)
	down.color_changed.connect(_on_menu_series_color.bind(i, "body_down"))
	row.add_child(down)
	var wick := _color_button(s.wick if s.wick.a > 0.0 else s.col_pos)
	wick.color_changed.connect(_on_menu_series_color.bind(i, "wick"))
	row.add_child(wick)
	var reset := Button.new()
	reset.text = "r"
	reset.tooltip_text = "reset colors"
	reset.pressed.connect(_on_menu_series_reset.bind(i))
	row.add_child(reset)
	var save := Button.new()
	save.text = "bin"
	save.tooltip_text = "save series as binary (.rbin): candles + books + levels"
	save.pressed.connect(_on_menu_series_save.bind(i))
	row.add_child(save)
	var tf_opt := OptionButton.new()
	for tname in QuoteStore.TFS:
		tf_opt.add_item(tname)
	tf_opt.selected = maxi(0, QuoteStore.TFS.find(s.tf))
	tf_opt.item_selected.connect(_on_menu_series_tf.bind(i))
	row.add_child(tf_opt)
	for k in indicator_widget.indicators.size():
		var icb := _color_button(indicator_widget.color_for(s, k))
		icb.color_changed.connect(_on_menu_ind_color.bind(i, k))
		row.add_child(icb)
	_menu_box.add_child(row)

func _color_button(c: Color) -> ColorPickerButton:
	var b := ColorPickerButton.new()
	b.edit_alpha = false
	b.custom_minimum_size = Vector2(32, 20)
	b.color = c
	return b

func _on_menu_mode(idx: int) -> void:
	series_widget.mode = idx as SeriesWidget.Mode
	_sync_mode_hud()
	_refresh()

func _on_menu_ind_toggle(on: bool, i: int) -> void:
	indicator_widget.indicators[i].enabled = on
	_sync_indicator_hud()
	_refresh()

func _on_menu_fft_lock(on: bool, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].locked = on

func _on_menu_opt_weight(v: float) -> void:
	_opt_rmse_pct = clampi(int(v), 0, 100)

func _on_menu_fft_optimize(i: int) -> void:
	if i >= indicator_widget.indicators.size():
		return
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[i]
	if ind.kind != "FFT" or ind.predict < 1:
		return
	if not indicator_widget._ensure_sim():
		return
	var best_s: PlotData.Series = null
	for s in data.series:
		if best_s == null or s.size() > best_s.size():
			best_s = s
	if best_s == null:
		return
	# the optimized window must leave at least 50 candles of real data
	# overlapping the prediction; store-backed series are bounded by the
	# cached window, not the full series
	var w0 := 0
	var w1 := best_s.size()
	if best_s.store != null:
		w0 = best_s.win_off
		w1 = best_s.win_off + best_s.candles.size()
	if w1 - w0 < IndicatorWidget.FFT_MIN_WINDOW + 50:
		return
	var closes := PackedFloat64Array()
	closes.resize(w1 - w0)
	for t in w1 - w0:
		closes[t] = best_s.candle_at(w0 + t).close
	var ret: Variant = indicator_widget._sim.call("fft_best", closes, ind.predict, _opt_rmse_pct)
	var r: PackedFloat64Array = ret as PackedFloat64Array
	if r.size() != 3:
		return
	ind.waves = int(r[1])
	ind.end = w1 - 50
	ind.start = maxi(w0, ind.end - int(r[0]))
	ind.opt_score = r[2]
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_menu_ind_delete(i: int) -> void:
	indicator_widget.remove_indicator(i)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_menu_sma_period(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].period = maxi(1, int(v))
		_sync_indicator_hud()
		_refresh()

func _on_menu_fft_value(v: float, i: int, field: String) -> void:
	if i >= indicator_widget.indicators.size():
		return
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[i]
	match field:
		"start":
			ind.start = int(v)
		"end":
			ind.end = int(v)
		"predict":
			ind.predict = int(v)
		"waves":
			ind.waves = int(v)
		"edge_w":
			ind.edge_w = maxf(1.0, v)
	_sync_indicator_hud()
	_refresh()

func _on_add_sma() -> void:
	indicator_widget.add_indicator("SMA", 20)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_menu_ks_m(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].m = clampi(int(v), 2, 7)
		_sync_indicator_hud()
		_refresh()

func _on_menu_obvol_bucket(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].bucket = maxf(0.0, v)
		_sync_indicator_hud()
		_refresh()

func _on_menu_obvol_mode(idx: int, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].obvol_mode = idx
		_sync_indicator_hud()
		_refresh()

func _on_add_ks() -> void:
	indicator_widget.add_indicator("KS", 200)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_add_temp() -> void:
	indicator_widget.add_indicator("TEMP", 200)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_add_cecp() -> void:
	# m=4 (24 bins) rather than the KS default m=5: C is only meaningful
	# once the window covers m! bins, so smaller pattern alphabets need
	# shorter windows for the same fidelity
	indicator_widget.add_indicator("CECP", 240)
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[indicator_widget.indicators.size() - 1]
	ind.m = 4
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_add_obvol() -> void:
	indicator_widget.add_indicator("OBVOL", 0)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_add_fft() -> void:
	indicator_widget.add_indicator("FFT", 0)
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[indicator_widget.indicators.size() - 1]
	ind.end = data.max_len()
	ind.start = maxi(0, ind.end - 256)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_menu_series_color(c: Color, i: int, field: String) -> void:
	if i >= data.series.size():
		return
	var s: PlotData.Series = data.series[i]
	match field:
		"body_up":
			s.body_up = c
		"body_down":
			s.body_down = c
		"wick":
			s.wick = c
	_refresh()

func _on_menu_ind_color(c: Color, si: int, k: int) -> void:
	if si < data.series.size():
		data.series[si].ind_colors[k] = c
		_refresh()

func _on_menu_series_reset(i: int) -> void:
	if i >= data.series.size():
		return
	var s: PlotData.Series = data.series[i]
	s.body_up = Color.TRANSPARENT
	s.body_down = Color.TRANSPARENT
	s.wick = Color.TRANSPARENT
	_rebuild_menu()
	_refresh()

func _on_menu_series_tf(sel: int, idx: int) -> void:
	if idx >= data.series.size():
		return
	var s: PlotData.Series = data.series[idx]
	var tf: String = QuoteStore.TFS[sel]
	if tf == s.tf:
		return
	if s.store != null:
		var err: int
		if QuoteStore.peek_magic(s.store_key) == QuoteStore.MAGIC:
			# a directly loaded rbin aggregates from itself, not from a csv
			err = await _open_binary(s, s.store_key, tf)
		else:
			err = await _open_quotes(s, s.store_key, tf)
		if err != OK:
			_set_load_status("timeframe failed: %s" % s.name)
			return
	else:
		s.switch_tf(tf)
	# the candle count changed by orders of magnitude — re-anchor the view
	# to the new series or everything renders outside the visible range
	head = mini(head, s.size())
	data.head = head
	_fit_view(s)
	view.px_per_index = clampf(view.plot_rect.size.x / maxf(float(s.size()), 1.0), 1.0, 400.0)
	view.follow = true
	view.follow_tail(head)
	_set_load_status("")
	_refresh()

func _begin_fft_drag(hit: Array) -> void:
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[hit[0]]
	# materialize an auto end (0 = follow tail) so the drag works against
	# a concrete index; start 0 is a valid first-candle index
	if ind.end <= 0:
		ind.end = data.max_len()
	_fft_drag = hit
	Input.set_default_cursor_shape(Input.CURSOR_HSIZE)
	_refresh()

func _drag_fft_to(pos: Vector2) -> void:
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[_fft_drag[0]]
	var i := maxi(0, int(round(view.x_to_index(pos.x))))
	if _fft_drag[1] == 0:
		var ns := clampi(i - 1, 0, ind.end - IndicatorWidget.FFT_MIN_WINDOW)
		if ind.locked:
			_shift_fft_window(ind, ns - ind.start)
		else:
			ind.start = ns
	else:
		var ne := maxi(i, ind.start + IndicatorWidget.FFT_MIN_WINDOW)
		if ind.locked:
			_shift_fft_window(ind, ne - ind.end)
		else:
			ind.end = ne
	_sync_indicator_hud()
	_refresh()

# moves the whole FFT window by delta, clamping so start stays >= 0
func _shift_fft_window(ind: IndicatorWidget.Indicator, delta: int) -> void:
	var d := maxi(-ind.start, delta)
	ind.start += d
	ind.end += d


func _process(delta: float) -> void:
	if not playing or paused:
		return
	t_acc += delta * replay_rate
	var n := int(t_acc)
	if n <= 0:
		return
	t_acc -= n
	head = mini(head + n, data.max_len())
	data.head = head
	if view.follow:
		view.follow_tail(head)
	_update_book()
	_refresh()
	if head >= data.max_len():
		playing = false

func _refresh() -> void:
	if grid == null:
		return
	_ensure_windows()
	for w in [grid, series_widget, indicator_widget, volume_widget, book_widget, cross_widget, cursor_widget]:
		w.queue_redraw()

# sliding window refill for binary-backed (real quotes) series: keeps
# `s.candles` covering the visible range (and the replay head) plus a pad
const WIN_PAD := 4096

func _ensure_windows() -> void:
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	for s in data.series:
		if s.store == null or s.store.count == 0:
			continue
		var lo := maxi(0, i0 - WIN_PAD)
		var hi := mini(s.store.count, max(i1, head) + WIN_PAD)
		# FFT indicators read absolute index ranges that may be far outside
		# the visible window — extend the cache to cover them
		if indicator_widget != null:
			for ind in indicator_widget.indicators:
				if ind.enabled and ind.kind == "FFT":
					var w := indicator_widget.fft_window(ind, s.size())
					lo = mini(lo, w.x)
					hi = maxi(hi, mini(w.y, s.store.count))
		if not s.candles.is_empty() and s.win_off <= lo and hi <= s.win_off + s.candles.size():
			continue
		s.win_off = s.store.read_range(lo, hi, s.candles)

func _set_load_status(msg: String) -> void:
	cursor_widget.load_text = msg

func _load_progress(p: float) -> void:
	cursor_widget.load_text = "loading %s… %d%%" % [_load_name, int(p * 100)]

var _load_name := ""

# opens (or builds) the binary store for a real-quotes csv at `tf`,
# converting the csv and aggregating parent timeframes on demand
func _open_quotes(s: PlotData.Series, csv_path: String, tf: String) -> int:
	if s.store == null:
		s.store = QuoteStore.new()
	s.store_key = csv_path
	var conv := QuoteStore.new()
	_load_name = csv_path.get_file()
	_set_load_status("converting %s…" % _load_name)
	var err: int = await conv.convert_csv_async(csv_path, _load_progress)
	if err != OK:
		return err
	if tf != "1m":
		var chain: Array[String] = []
		var t := tf
		while t != "1m":
			chain.push_front(t)
			t = QuoteStore._parent_tf(t)
		for ct in chain:
			_set_load_status("building %s…" % ct)
			err = await conv.ensure_tf_async(csv_path, ct, _load_progress)
			if err != OK:
				return err
	if not s.store.open(QuoteStore.cache_path(csv_path, tf)):
		return ERR_CANT_OPEN
	s.tf = tf
	s.win_off = 0
	s.candles.clear()
	return OK

# opens a directly saved RELQ rbin (no csv conversion); higher timeframes
# aggregate down the parent chain starting from the file itself
func _open_binary(s: PlotData.Series, path: String, tf: String) -> int:
	if s.store == null:
		s.store = QuoteStore.new()
	s.store_key = path
	_load_name = path.get_file()
	if tf != "1m":
		var pf := FileAccess.open(path, FileAccess.READ)
		if pf == null:
			return ERR_CANT_OPEN
		var ph := QuoteStore._peek_header(pf)
		pf.close()
		if ph.is_empty():
			return ERR_PARSE_ERROR
		var chain: Array[String] = []
		var t := tf
		while t != "1m":
			chain.push_front(t)
			t = QuoteStore._parent_tf(t)
		var parent := path
		for ct in chain:
			_set_load_status("building %s…" % ct)
			var out := QuoteStore.cache_path(path, ct)
			var err: int = await QuoteStore.new()._aggregate_tf_async(parent, out, ct, ph["dec"], _load_progress)
			if err != OK:
				return err
			parent = out
	var base := path if tf == "1m" else QuoteStore.cache_path(path, tf)
	if not s.store.open(base):
		return ERR_CANT_OPEN
	s.tf = tf
	s.win_off = 0
	s.candles.clear()
	return OK

# ---- binary save/load (RELS) ----------------------------------------------

func _on_menu_series_save(i: int) -> void:
	if _save_dialog == null:
		_save_dialog = FileDialog.new()
		_save_dialog.file_mode = FileDialog.FILE_MODE_SAVE_FILE
		_save_dialog.access = FileDialog.ACCESS_FILESYSTEM
		_save_dialog.filters = PackedStringArray(["*.rbin ; Binary candles"])
		_save_dialog.file_selected.connect(_on_save_file_selected)
		add_child(_save_dialog)
	_save_series_idx = i
	_save_dialog.current_file = data.series[i].name + ".rbin"
	_save_dialog.popup_centered(Vector2i(700, 500))

func _on_save_file_selected(path: String) -> void:
	var i := _save_series_idx
	_save_series_idx = -1
	if i < 0 or i >= data.series.size():
		return
	if not path.ends_with(".rbin"):
		path += ".rbin"
	_load_name = path.get_file()
	_set_load_status("saving %s…" % _load_name)
	var err := await _save_series_binary(i, path)
	if err == OK:
		_set_load_status("saved %s" % _load_name)
	else:
		_set_load_status("save failed: %s (code %d)" % [_load_name, err])

# store-backed series: byte-copy the 1m cache (keeps timestamps and the
# sliding-window store on reload); memory-backed series: full RELS export
# (base candles + book snapshots + level lifetimes + vev events)
func _save_series_binary(i: int, path: String) -> Error:
	var s: PlotData.Series = data.series[i]
	if s.store != null:
		# directly loaded rbins copy themselves; csv-backed series copy
		# their converted 1m cache
		var src := s.store_key
		if QuoteStore.peek_magic(src) != QuoteStore.MAGIC:
			src = QuoteStore.cache_path(src)
		return await _copy_file_async(src, path)
	var base: Array[Candle] = s._base if not s._base.is_empty() else s.candles
	var books: Array = s._base_books
	var use_books := books.size() == base.size() and not books.is_empty()
	var depth := 0
	if use_books:
		for b in books:
			depth = maxi(depth, (b["bids"] as Array).size())
			depth = maxi(depth, (b["asks"] as Array).size())
		use_books = depth > 0
	var vev := PackedByteArray()
	var nvev := 0
	if s.levels_path != "" and FileAccess.file_exists(s.levels_path):
		var vf := FileAccess.open(s.levels_path, FileAccess.READ)
		if vf != null and vf.get_length() >= 16:
			vf.seek(8)
			nvev = int(vf.get_64())
			vf.seek(0)
			vev = vf.get_buffer(16 + nvev * 20)
			if vev.size() < 16 + nvev * 20:
				nvev = 0
		if vf != null:
			vf.close()
	var f := FileAccess.open(path, FileAccess.WRITE)
	if f == null:
		return ERR_CANT_OPEN
	var flags := 0
	if use_books:
		flags |= QuoteStore.SIM_FLAG_BOOKS
	if not s.levels.is_empty():
		flags |= QuoteStore.SIM_FLAG_LEVELS
	var n := base.size()
	QuoteStore.write_sim_header(f, flags, view.decimals, n, depth if use_books else 0, s.levels.size() / 7, nvev, 0.0, 0.0)
	var rec := PackedByteArray()
	rec.resize(QuoteStore.REC_SIZE)
	var lo := INF
	var hi := -INF
	for k in n:
		var c := base[k]
		rec.encode_u64(0, k)
		rec.encode_double(8, c.open)
		rec.encode_double(16, c.high)
		rec.encode_double(24, c.low)
		rec.encode_double(32, c.close)
		rec.encode_u64(40, c.vol)
		f.store_buffer(rec)
		lo = minf(lo, c.low)
		hi = maxf(hi, c.high)
		if k % QuoteStore.CHUNK == QuoteStore.CHUNK - 1:
			_load_progress(minf(float(k) / maxf(n, 1), 0.99))
			await Engine.get_main_loop().process_frame
	if use_books:
		for k in n:
			_write_book_rec(f, books[k], depth)
			if k % QuoteStore.CHUNK == QuoteStore.CHUNK - 1:
				_load_progress(minf(float(k) / maxf(n, 1), 0.99))
				await Engine.get_main_loop().process_frame
	if flags & QuoteStore.SIM_FLAG_LEVELS:
		var lbuf := PackedByteArray()
		lbuf.resize(s.levels.size() * 8)
		for k in s.levels.size():
			lbuf.encode_double(k * 8, s.levels[k])
		f.store_buffer(lbuf)
	if nvev > 0:
		# records only — the 16-byte header is rebuilt on load
		f.store_buffer(vev.slice(16))
	var pos := f.get_position()
	f.seek(0)
	QuoteStore.write_sim_header(f, flags, view.decimals, n, depth if use_books else 0, s.levels.size() / 7, nvev, lo, hi)
	f.seek(pos)
	f.close()
	if view.decimals >= 0:
		var mf := FileAccess.open(path + ".meta", FileAccess.WRITE)
		if mf != null:
			mf.store_line("decimals=%d" % view.decimals)
			mf.close()
	_load_progress(1.0)
	return OK

static func _write_book_rec(f: FileAccess, book: Dictionary, depth: int) -> void:
	var b: Array = book["bids"]
	var a: Array = book["asks"]
	var buf := PackedByteArray()
	buf.resize(depth * 32)
	for d in depth:
		if d < b.size():
			buf.encode_double(d * 16, b[d].x)
			buf.encode_double(d * 16 + 8, b[d].y)
		if d < a.size():
			buf.encode_double(depth * 16 + d * 16, a[d].x)
			buf.encode_double(depth * 16 + d * 16 + 8, a[d].y)
	f.store_buffer(buf)

func _copy_file_async(src: String, dst: String) -> Error:
	var f := FileAccess.open(src, FileAccess.READ)
	if f == null:
		return ERR_CANT_OPEN
	var w := FileAccess.open(dst, FileAccess.WRITE)
	if w == null:
		f.close()
		return ERR_CANT_OPEN
	var size := f.get_length()
	var k := 0
	while f.get_position() < size:
		w.store_buffer(f.get_buffer(1 << 20))
		k += 1
		if k % 64 == 0:
			_load_progress(minf(float(f.get_position()) / maxf(size, 1), 0.99))
			await Engine.get_main_loop().process_frame
	f.close()
	w.close()
	_load_progress(1.0)
	return OK

# loads a RELS full export; writes the embedded vev block back out as a
# `<base>.vev` sidecar next to the file (the OBVOL indicator reads it there)
func _load_binary(path: String) -> Dictionary:
	var out := { "candles": [], "books": [], "levels": PackedFloat64Array(), "vev_path": "" }
	var f := FileAccess.open(path, FileAccess.READ)
	if f == null:
		push_error("cannot open binary file: " + path)
		return out
	var h := QuoteStore.read_sim_header(f)
	if h.is_empty():
		push_error("bad RELS header: " + path)
		f.close()
		return out
	var n: int = h["n"]
	var depth: int = h["depth"]
	var candles: Array[Candle] = []
	var i := 0
	while i < n:
		var upto := mini(i + QuoteStore.CHUNK, n)
		var want := (upto - i) * QuoteStore.REC_SIZE
		var buf := f.get_buffer(want)
		if buf.size() < want:
			buf.resize(want)
		for off in range(0, want, QuoteStore.REC_SIZE):
			var c := Candle.new()
			c.open = buf.decode_double(off + 8)
			c.high = buf.decode_double(off + 16)
			c.low = buf.decode_double(off + 24)
			c.close = buf.decode_double(off + 32)
			c.vol = int(buf.decode_u64(off + 40))
			candles.append(c)
		i = upto
	var books: Array = []
	if int(h["flags"]) & QuoteStore.SIM_FLAG_BOOKS:
		for k in n:
			var bbuf := f.get_buffer(depth * 32)
			if bbuf.size() < depth * 32:
				bbuf.resize(depth * 32)
			var bids: Array[Vector2] = []
			var asks: Array[Vector2] = []
			for d in depth:
				var v := bbuf.decode_double(d * 16 + 8)
				if v > 0.0:
					bids.append(Vector2(bbuf.decode_double(d * 16), v))
				v = bbuf.decode_double(depth * 16 + d * 16 + 8)
				if v > 0.0:
					asks.append(Vector2(bbuf.decode_double(depth * 16 + d * 16), v))
			books.append({ "bids": bids, "asks": asks })
	if int(h["flags"]) & QuoteStore.SIM_FLAG_LEVELS:
		var levels := PackedFloat64Array()
		var lbuf := f.get_buffer(int(h["nlevels"]) * 56)
		if lbuf.size() < int(h["nlevels"]) * 56:
			lbuf.resize(int(h["nlevels"]) * 56)
		for r in int(h["nlevels"]):
			for k in 7:
				levels.append(lbuf.decode_double(r * 56 + k * 8))
		out["levels"] = levels
	if int(h["nvev"]) > 0:
		var vbuf := f.get_buffer(int(h["nvev"]) * 20)
		if vbuf.size() == int(h["nvev"]) * 20:
			var base := path
			if base.ends_with(".rbin"):
				base = base.substr(0, base.length() - ".rbin".length())
			var vev_path := base + ".vev"
			var vf := FileAccess.open(vev_path, FileAccess.WRITE)
			if vf != null:
				var hdr := PackedByteArray()
				hdr.resize(16)
				hdr.encode_u32(0, 0x564C4552)  # "RELV"
				hdr.encode_u32(4, 1)
				hdr.encode_u64(8, h["nvev"])
				vf.store_buffer(hdr)
				vf.store_buffer(vbuf)
				vf.close()
				out["vev_path"] = vev_path
	f.close()
	out["candles"] = candles
	out["books"] = books
	return out

func add_timeline(tl_name: String, path: String) -> int:
	# a same-name timeline is a reload (e.g. after resuming generation):
	# refresh its data in place, keeping user colors and indicators
	for i in data.series.size():
		if data.series[i].name == tl_name:
			return await _reload_series(i, path)
	var s := PlotDataScript.Series.new()
	s.name = tl_name
	var magic := QuoteStore.peek_magic(path)
	if magic == QuoteStore.MAGIC_SIM:
		var t := _load_binary(path)
		s.set_base(t["candles"], t["books"], t["levels"])
		s.levels_path = t["vev_path"]
	elif magic == QuoteStore.MAGIC:
		if await _open_binary(s, path, "1m") != OK:
			_set_load_status("load failed: %s" % tl_name)
			return -1
	else:
		var ticks_path := "%s.ticks.csv" % path.get_basename()
		if FileAccess.file_exists(ticks_path):
			var t := _load_ticks(ticks_path)
			s.set_base(t["candles"], t["books"], t["levels"])
			s.levels_path = t["vev_path"]
		elif QuoteStore.sniff_csv(path) == "quotes":
			if await _open_quotes(s, path, "1m") != OK:
				_set_load_status("load failed: %s" % tl_name)
				return -1
		else:
			s.set_base(_load_candles(path), [], PackedFloat64Array())
	var idx: int = data.series.size()
	var col: Color = PALETTE[idx % PALETTE.size()]
	s.col_pos = col
	s.col_neg = col.darkened(0.35)
	data.add_series(s)
	if data.series.size() == 1:
		_read_meta(path)
		if s.store != null and s.store.decimals >= 0:
			view.decimals = s.store.decimals
		_fit_view(s)
	view.follow = true
	head = 0
	data.head = head
	playing = true
	_update_book()
	_set_load_status("")
	_refresh()
	return idx

func _reload_series(i: int, path: String) -> int:
	var s: PlotData.Series = data.series[i]
	var magic := QuoteStore.peek_magic(path)
	if magic == QuoteStore.MAGIC_SIM:
		var t := _load_binary(path)
		s.levels_path = t["vev_path"]
		s.set_base(t["candles"], t["books"], t["levels"])
	elif magic == QuoteStore.MAGIC:
		s.levels = PackedFloat64Array()
		s.levels_path = ""
		if await _open_binary(s, path, s.tf) != OK:
			_set_load_status("reload failed: %s" % s.name)
			return i
	else:
		var ticks_path := "%s.ticks.csv" % path.get_basename()
		if FileAccess.file_exists(ticks_path):
			var t := _load_ticks(ticks_path)
			s.set_base(t["candles"], t["books"], t["levels"])
			s.levels_path = t["vev_path"]
		elif s.store != null:
			s.levels = PackedFloat64Array()
			s.levels_path = ""
			if await _open_quotes(s, path, s.tf) != OK:
				_set_load_status("reload failed: %s" % s.name)
				return i
		elif QuoteStore.sniff_csv(path) == "quotes":
			s.levels = PackedFloat64Array()
			s.levels_path = ""
			if await _open_quotes(s, path, "1m") != OK:
				_set_load_status("reload failed: %s" % s.name)
				return i
		else:
			s.set_base(_load_candles(path), [], PackedFloat64Array())
			s.levels_path = ""
	_update_book()
	_refresh()
	return i

func remove_timeline(idx: int) -> void:
	var s: PlotData.Series = data.series[idx]
	if s.store != null:
		s.store.close()
	data.series.remove_at(idx)
	for i in data.series.size():
		var col: Color = PALETTE[i % PALETTE.size()]
		data.series[i].col_pos = col
		data.series[i].col_neg = col.darkened(0.35)
	head = -1
	data.head = head
	_update_book()
	_refresh()

func set_timeline_visible(idx: int, vis: bool) -> void:
	data.series[idx].visible = vis
	_update_book()
	_refresh()

func _read_meta(path: String) -> void:
	view.decimals = -1
	var f := FileAccess.open(path + ".meta", FileAccess.READ)
	if f == null:
		return
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.begins_with("decimals="):
			view.decimals = line.get_slice("=", 1).to_int()

func _fit_view(s) -> void:
	if s.size() == 0:
		return
	var lo: float
	var hi: float
	if s.store != null:
		lo = s.store.min_low
		hi = s.store.max_high
	else:
		lo = s.candles[0].low
		hi = s.candles[0].high
		for c in s.candles:
			lo = minf(lo, c.low)
			hi = maxf(hi, c.high)
	view.fit_prices(lo, hi, view.plot_rect.size.y - vol_pane_h())

func _update_book() -> void:
	book_widget.clear()
	for s in data.series:
		if not s.visible or s.size() == 0:
			continue
		var n := s.size()
		var i := clampi(head - 1, 0, n - 1)
		var snap: Variant = s.book_at(i)
		if snap != null:
			var sb: Array[Vector2] = snap["bids"]
			var sa: Array[Vector2] = snap["asks"]
			var mid := 0.0
			if not sb.is_empty() and not sa.is_empty():
				mid = (sb[0].x + sa[0].x) / 2.0
			elif not sb.is_empty():
				mid = sb[0].x
			elif not sa.is_empty():
				mid = sa[0].x
			book_widget.set_book(sb, sa, mid)
			return
		if s.store != null and (s.candles.is_empty() or i < s.win_off or i >= s.win_off + s.candles.size()):
			return
		var probe := mini(i + 1, 100)
		var span_sum := 0.0
		for k in range(i - probe + 1, i + 1):
			span_sum += s.candle_at(k).high - s.candle_at(k).low
		var step := maxf(span_sum / probe / 4.0, 1.0e-9)
		book_widget.set_ladder(s.candle_at(i).close, step)
		return

func _on_timeline_added(tl_name: String, path: String) -> void:
	add_timeline(tl_name, path)

func _on_timeline_removed(idx: int) -> void:
	remove_timeline(idx)

func _on_timeline_toggled(idx: int, vis: bool) -> void:
	set_timeline_visible(idx, vis)

func _load_candles(path: String) -> Array[Candle]:
	var loaded: Array[Candle] = []
	var f := FileAccess.open(path, FileAccess.READ)
	if f == null:
		push_error("cannot open candles file: " + path)
		return loaded
	f.get_line()
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 4:
			continue
		var c := Candle.new()
		c.open = parts[0].to_float()
		c.high = parts[1].to_float()
		c.low = parts[2].to_float()
		c.close = parts[3].to_float()
		if parts.size() >= 5:
			c.vol = parts[4].to_int()
		loaded.append(c)
	return loaded

# ticks export: one row per event — c,price,vol, then book-depth
# (bid px,bid vol,ask px,ask vol) quads; rows with vol == 0 carry no trade
# (book-only placement or end-of-candle marker). Ticks are aggregated to
# candles; each candle keeps the book of its last row.
func _load_ticks(path: String) -> Dictionary:
	var candles: Array[Candle] = []
	var books: Array = []
	var f := FileAccess.open(path, FileAccess.READ)
	if f == null:
		push_error("cannot open ticks file: " + path)
		return { "candles": candles, "books": books }
	f.get_line()
	var cur := -1
	var c: Candle = null
	var bids: Array[Vector2] = []
	var asks: Array[Vector2] = []
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 7:
			continue
		var ci := parts[0].to_int()
		if ci != cur:
			if c != null:
				books.append({ "bids": bids, "asks": asks })
			c = Candle.new()
			candles.append(c)
			cur = ci
		var vol := parts[2].to_int()
		if vol > 0:
			c.update(parts[1].to_float())
			c.vol += vol
		bids = _levels(parts, 3)
		asks = _levels(parts, 5)
	if c != null:
		books.append({ "bids": bids, "asks": asks })
	# the levels sidecar is named after the CANDLES file (foo.levels.csv),
	# not the ticks file — strip ".ticks.csv", not just the extension
	var base := path
	if base.ends_with(".ticks.csv"):
		base = base.substr(0, base.length() - ".ticks.csv".length())
	return { "candles": candles, "books": books, "levels": _load_levels(base + ".levels.csv"), "vev_path": base + ".vev" }

# level-lifetime sidecar for the OBVOL indicator: one row per continuous
# (side, price) existence: c0,c1,side,price,peak,ev0,evn (the last two
# address volume-change records in the .vev binary sidecar; absent on
# exports written before the event export existed). Stored flat (7
# floats/row) — the indicator iterates it per window slide
func _load_levels(path: String) -> PackedFloat64Array:
	var out := PackedFloat64Array()
	var f := FileAccess.open(path, FileAccess.READ)
	if f == null:
		return out
	f.get_line()
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 5:
			continue
		var ev0 := 0.0
		var evn := 0.0
		if parts.size() >= 7:
			ev0 = parts[5].to_float()
			evn = parts[6].to_float()
		out.append_array([parts[0].to_float(), parts[1].to_float(), parts[2].to_float(), parts[3].to_float(), parts[4].to_float(), ev0, evn])
	return out

func _levels(parts: PackedStringArray, offset: int) -> Array[Vector2]:
	var out: Array[Vector2] = []
	var i := offset
	while i + 1 < parts.size():
		var px := parts[i].to_float()
		var v := parts[i + 1].to_int()
		if v > 0:
			out.append(Vector2(px, v))
		i += 4
	return out
