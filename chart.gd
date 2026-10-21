extends Control

const PlotDataScript := preload("res://plot_data.gd")

const ZOOM_FACTOR := 1.2
const VOL_GRIP := 4.0
const MIN_GRAPH_H := 80.0
const RMB_CLICK_TOL := 4.0

@export var replay_rate := 60.0

var view := ChartView.new()
var data: PlotData = PlotDataScript.new()

# sub-pane stack in stack order: the chart pane (flex, header + graph)
# first, then one pane per pane-drawn indicator (MACD/RSI/VOLUME), in
# indicator order. chart.gd owns the layout (heights, rects, collapse
# buttons, grip drags); SubPaneWidget only draws
const PANE_MIN_H := 28.0
const PANE_MAX_H := 600.0

var _panes: Array = []
var _chart_pane: SubPaneWidget
var chart_collapsed := false
var _pane_drag: SubPaneWidget = null

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
@onready var book_widget: OrderBookWidget = $Book
@onready var cross_widget: CrossWidget = $Cross
@onready var cursor_widget: CursorWidget = $Cursor
@onready var sim_window: ToolWindow = $SimWindow
@onready var sim_panel: SimPanel = $SimWindow/Box/SimPanel
@onready var series_window: ToolWindow = $SeriesWindow
@onready var series_panel: SeriesPanel = $SeriesWindow/Box/SeriesPanel
@onready var trades_widget: TradesWidget = $Trades
@onready var dtw_overlay: DtwOverlayWidget = $DtwOverlay

# backtest trades overlays: shared with trades_widget by array identity.
# Each set: {name, enabled, trades: Array of Dicts, color}
var _trades_sets: Array = []
var _trades_dialog: FileDialog
var _trades_save_dialog: FileDialog
var _trades_save_idx := -1

func _ready() -> void:
	_mcp_start()
	for w in [grid, series_widget, indicator_widget, book_widget, cross_widget, cursor_widget, trades_widget]:
		w.view = view
		w.data = data
	dtw_overlay.view = view
	dtw_overlay.data = data
	trades_widget.trades_sets = _trades_sets
	cross_widget.panes = _panes
	_chart_pane = _make_pane("chart", null)
	_panes.append(_chart_pane)
	# chart chrome above the graph widgets (header strip), the rest of the
	# panes below the crosshair/cursor overlays
	move_child(_chart_pane, indicator_widget.get_index() + 1)
	_sync_panes()
	_sync_mode_hud()
	_sync_indicator_hud()
	cursor_widget.cross = cross_widget
	cursor_widget.indicators = indicator_widget
	view.layout(size)
	sim_panel.generated.connect(_on_sim_generated)
	series_panel.chart = self
	series_panel.series_toggled.connect(set_timeline_visible)
	series_panel.series_removed.connect(_on_series_removed)
	series_panel.series_loaded.connect(_on_series_loaded)
	series_panel.color_picked.connect(_on_menu_series_color)
	series_panel.ind_color_picked.connect(_on_menu_ind_color)
	series_panel.series_reset.connect(_on_series_reset)
	series_panel.series_saved.connect(_on_menu_series_save)
	series_panel.tf_picked.connect(_on_menu_series_tf)
	_menu = PopupPanel.new()
	var panel := PanelContainer.new()
	_menu_box = VBoxContainer.new()
	panel.add_child(_menu_box)
	_menu.add_child(panel)
	add_child(_menu)
	_trades_dialog = FileDialog.new()
	_trades_dialog.file_mode = FileDialog.FILE_MODE_OPEN_FILE
	_trades_dialog.access = FileDialog.ACCESS_FILESYSTEM
	_trades_dialog.filters = PackedStringArray(["*.bin ; Trades binary"])
	_trades_dialog.file_selected.connect(_on_trades_file_picked)
	add_child(_trades_dialog)
	_trades_save_dialog = FileDialog.new()
	_trades_save_dialog.file_mode = FileDialog.FILE_MODE_SAVE_FILE
	_trades_save_dialog.access = FileDialog.ACCESS_FILESYSTEM
	_trades_save_dialog.filters = PackedStringArray(["*.bin ; Trades binary"])
	_trades_save_dialog.file_selected.connect(_on_trades_save_picked)
	add_child(_trades_save_dialog)

# --- trades overlays ---------------------------------------------------------

# load a RELT trades binary (see Sim.Csv.tradesBin) and register it as an
# overlay; returns the trade count, or -1 when the file is missing/invalid
func add_trades_from_file(path: String, tname: String) -> int:
	var trades := _load_trades_bin(path)
	if trades.is_empty():
		return -1
	_trades_sets.append({
		"name": tname,
		"enabled": true,
		"trades": trades,
		"path": path,
		"color": PALETTE[_trades_sets.size() % PALETTE.size()],
	})
	_rebuild_menu()
	_refresh()
	return trades.size()

func _load_trades_bin(path: String) -> Array:
	var f := FileAccess.open(path, FileAccess.READ)
	if f == null:
		return []
	var bytes := f.get_buffer(f.get_length())
	f.close()
	if bytes.size() < 16 or bytes.decode_u32(0) != 0x544C4552:
		return []
	var n: int = bytes.decode_u64(8)
	var rec_len := 48
	var blob_off := 16 + n * rec_len
	if bytes.size() < blob_off + 8:
		return []
	var blob_len: int = bytes.decode_u64(blob_off)
	if bytes.size() < blob_off + 8 + blob_len:
		return []
	var causes := PackedStringArray()
	var pos := blob_off + 8
	var blob_end := pos + blob_len
	while pos < blob_end:
		var e := pos
		while e < blob_end and bytes[e] != 0:
			e += 1
		causes.append(bytes.slice(pos, e).get_string_from_utf8())
		pos = e + 1
	var out: Array = []
	for k in n:
		var o := 16 + k * rec_len
		var cause_id := bytes.decode_u32(o + 12)
		if cause_id >= causes.size():
			return []
		out.append({
			"i0": bytes.decode_u32(o),
			"i1": bytes.decode_u32(o + 4),
			"side": bytes.decode_s32(o + 8),
			"cause": causes[cause_id],
			"entry": bytes.decode_double(o + 16),
			"exit": bytes.decode_double(o + 24),
			"pnl": bytes.decode_double(o + 32),
			"vol": bytes.decode_double(o + 40),
		})
	return out

func _on_trades_file_picked(path: String) -> void:
	add_trades_from_file(path, path.get_file().get_basename())

func _on_menu_trades_save(k: int) -> void:
	if k < _trades_sets.size():
		_trades_save_idx = k
		_trades_save_dialog.popup_centered(Vector2i(700, 500))

func _on_trades_save_picked(target: String) -> void:
	if _trades_save_idx < 0 or _trades_save_idx >= _trades_sets.size():
		return
	var src := str(_trades_sets[_trades_save_idx].get("path", ""))
	var out := target
	if not out.ends_with(".bin"):
		out += ".bin"
	var rf := FileAccess.open(src, FileAccess.READ)
	if rf == null:
		return
	var bytes := rf.get_buffer(rf.get_length())
	rf.close()
	var wf := FileAccess.open(out, FileAccess.WRITE)
	if wf == null:
		return
	wf.store_buffer(bytes)
	wf.close()
	_trades_sets[_trades_save_idx]["path"] = out

func _on_menu_trades_toggle(on: bool, k: int) -> void:
	if k < _trades_sets.size():
		_trades_sets[k]["enabled"] = on
		_refresh()

func _on_menu_trades_remove(k: int) -> void:
	if k < _trades_sets.size():
		_trades_sets.remove_at(k)
		_rebuild_menu()
		_refresh()

func _notification(what: int) -> void:
	if what == NOTIFICATION_RESIZED:
		view.layout(size)
		_sync_panes()
		_refresh()

func _input(event: InputEvent) -> void:
	# Node._input fires even while this tab is hidden — without the guard
	# every RMB click on other tabs (e.g. the DTW page) opens this chart's
	# context menu, and space/keys drive playback from anywhere
	if not is_visible_in_tree():
		return
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
			# event positions are viewport-global; all chart geometry
			# (view rects, pane rects) is chart-local — the chart sits
			# below the main tab bar, so convert before hit tests
			var pos: Vector2 = button.position - get_screen_position()
			if button.button_index == MOUSE_BUTTON_LEFT and dtw_overlay.visible:
				var hit_i := dtw_overlay.window_at(pos)
				if hit_i >= 0:
					_on_dtw_overlay_clicked(hit_i)
					return
			if button.button_index == MOUSE_BUTTON_WHEEL_UP:
				view.zoom_x_at(pos, ZOOM_FACTOR)
				_refresh()
			elif button.button_index == MOUSE_BUTTON_WHEEL_DOWN:
				view.zoom_x_at(pos, 1.0 / ZOOM_FACTOR)
				_refresh()
			elif button.button_index == MOUSE_BUTTON_LEFT:
				var pane: SubPaneWidget = _pane_btn_at(pos)
				if pane != null:
					_toggle_pane(pane)
				else:
					var hit: Array = indicator_widget.fft_handle_at(pos)
					if not hit.is_empty():
						_begin_fft_drag(hit)
					elif _pane_grip_at(pos) != null:
						if button.double_click:
							_toggle_pane(_pane_grip_at(pos))
						else:
							_pane_drag = _pane_grip_at(pos)
			elif button.button_index == MOUSE_BUTTON_RIGHT:
				_rmb_down = true
				_rmb_down_pos = pos
		elif not button.pressed:
			if button.button_index == MOUSE_BUTTON_LEFT:
				_pane_drag = null
				if not _fft_drag.is_empty():
					_fft_drag = []
					Input.set_default_cursor_shape(Input.CURSOR_ARROW)
			elif button.button_index == MOUSE_BUTTON_RIGHT:
				var pos: Vector2 = button.position - get_screen_position()
				if _rmb_down and pos.distance_to(_rmb_down_pos) < RMB_CLICK_TOL:
					_open_popup(pos)
				_rmb_down = false
	elif event is InputEventMouseMotion:
		var motion := event as InputEventMouseMotion
		var pos: Vector2 = motion.position - get_screen_position()
		cross_widget.mouse = pos
		if not _fft_drag.is_empty():
			_drag_fft_to(pos)
		elif _pane_drag != null:
			_drag_pane_to(_pane_drag, pos)
		elif not sim_window.is_dragging() and not _over_ui(motion.position):
			if motion.button_mask & MOUSE_BUTTON_MASK_RIGHT != 0:
				view.scale_by(motion.relative, pos)
				view.follow = false
			elif motion.button_mask & (MOUSE_BUTTON_MASK_LEFT | MOUSE_BUTTON_MASK_MIDDLE) != 0 or pan_mode:
				view.pan_by(motion.relative)
				view.follow = false
		_refresh()

func _over_ui(pos: Vector2) -> bool:
	return (sim_window.visible and sim_window.get_global_rect().has_point(pos)) \
		or (series_window.visible and series_window.get_global_rect().has_point(pos))

# --- DTW matched-window overlay (data comes from the DTW page) --------------

func set_dtw_overlay(matches: Dictionary, win: int) -> void:
	dtw_overlay.set_data(matches, win)
	_refresh()

func clear_dtw_overlay() -> void:
	dtw_overlay.clear()

func _on_dtw_overlay_clicked(i: int) -> void:
	dtw_overlay.sel_i = i
	dtw_overlay.queue_redraw()
	var page := get_tree().root.find_child("DtwPage", true, false)
	if page != null and page.has_method("show_drill"):
		page.show_drill(i, dtw_overlay.j_for(i))

func _unhandled_key_input(event: InputEvent) -> void:
	if not is_visible_in_tree():
		return
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
	elif key.keycode == KEY_V:
		# toggle the first volume indicator, like any other indicator
		for ind in indicator_widget.indicators:
			if ind.kind == "VOLUME":
				ind.enabled = not ind.enabled
				_sync_indicator_hud()
				_sync_panes()
				_refresh()
				break

# --- sub-pane stack ---------------------------------------------------------

func _make_pane(kind: String, ind: IndicatorWidget.Indicator) -> SubPaneWidget:
	var p := SubPaneWidget.new()
	p.kind = kind
	p.ind = ind
	p.view = view
	p.data = data
	p.indicators = indicator_widget
	add_child(p)
	return p

# hit test for the collapse/expand buttons; returns the pane or null
func _pane_btn_at(pos: Vector2) -> SubPaneWidget:
	for p in _panes:
		if p.button_rect().has_point(pos):
			return p
	return null

# hit test for the resize grips: the top edge of any fixed pane (the
# chart pane is flex — it absorbs the space the grips give/take)
func _pane_grip_at(pos: Vector2) -> SubPaneWidget:
	for i in range(1, _panes.size()):
		var p: SubPaneWidget = _panes[i]
		var r := p.pane_rect
		if pos.x >= r.position.x and pos.x <= r.end.x \
			and absf(pos.y - r.position.y) <= VOL_GRIP:
			return p
	return null

func _toggle_pane(p: SubPaneWidget) -> void:
	if p == _chart_pane:
		chart_collapsed = not chart_collapsed
		_sync_panes()
		return
	p.ind.pane_collapsed = not p.ind.pane_collapsed
	_sync_panes()

# vertical grip drag: the pane under the cursor takes/loses height, the
# chart absorbs the difference
func _drag_pane_to(pane: SubPaneWidget, pos: Vector2) -> void:
	if pane == _chart_pane:
		return
	var h := clampf(pane.pane_rect.end.y - pos.y, PANE_MIN_H, PANE_MAX_H)
	pane.ind.pane_h = h
	pane.ind.pane_collapsed = h < SubPaneWidget.HEADER_H + 8.0
	_sync_panes()

# reconciles the pane list with the indicator list (chart first, then one
# pane per MACD/RSI/VOLUME indicator in indicator order), computes
# heights/rects and drives the view (set_header_h/set_pane_h)
func _sync_panes() -> void:
	if _chart_pane == null:
		return
	# rebuild the wanted pane list in indicator order (mutate `_panes` in
	# place: cross_widget shares the array by identity)
	var wanted: Array = []
	for ind in indicator_widget.indicators:
		if ind.kind != "MACD" and ind.kind != "RSI" and ind.kind != "VOLUME":
			continue
		var found: SubPaneWidget = null
		for p in _panes:
			if p != _chart_pane and p.ind == ind:
				found = p
		if found == null:
			found = _make_pane(ind.kind, ind)
		wanted.append(found)
	for p in _panes:
		if p != _chart_pane and not wanted.has(p):
			remove_child(p)
			p.queue_free()
	_panes.clear()
	_panes.append(_chart_pane)
	_panes.append_array(wanted)
	# child draw order: chart chrome above the graph widgets, then the
	# panes, all below the crosshair/cursor overlays
	move_child(_chart_pane, indicator_widget.get_index() + 1)
	var idx := _chart_pane.get_index() + 1
	for p in wanted:
		move_child(p, idx)
		idx += 1
	# heights of the fixed panes; the graph keeps at least MIN_GRAPH_H by
	# shrinking panes bottom-up to the header-strip minimum
	var heights := {}
	var total := 0.0
	for i in range(1, _panes.size()):
		var p: SubPaneWidget = _panes[i]
		var h := SubPaneWidget.HEADER_H
		if not p.ind.pane_collapsed and p.ind.enabled:
			h = maxf(p.ind.pane_h, PANE_MIN_H)
		heights[p] = h
		total += h
	view.set_header_h(0.0 if chart_collapsed else SubPaneWidget.HEADER_H)
	if chart_collapsed:
		# panes fill everything below the chart header strip
		view.set_pane_h(maxf(view.full_rect.size.y - SubPaneWidget.HEADER_H, 0.0))
	else:
		var max_total := maxf(view.full_rect.size.y - SubPaneWidget.HEADER_H - MIN_GRAPH_H, SubPaneWidget.HEADER_H)
		var excess := total - max_total
		var i := _panes.size() - 1
		while excess > 0.0 and i >= 1:
			var p: SubPaneWidget = _panes[i]
			var cut: float = minf(heights[p] - SubPaneWidget.HEADER_H, excess)
			if cut > 0.0:
				heights[p] -= cut
				excess -= cut
			i -= 1
		view.set_pane_h(mini(total, max_total))
	# chart pane rect covers the header strip + graph (or just the header
	# when collapsed); fixed panes stack below it
	var stack_top := maxf(view.plot_rect.end.y, SubPaneWidget.HEADER_H)
	_chart_pane.collapsed = chart_collapsed
	_chart_pane.pane_rect = Rect2(view.full_rect.position.x, 0.0, view.full_rect.size.x, stack_top)
	var y := stack_top
	for i in range(1, _panes.size()):
		var p: SubPaneWidget = _panes[i]
		p.collapsed = p.ind.pane_collapsed
		p.pane_rect = Rect2(view.full_rect.position.x, y, view.full_rect.size.x, heights[p])
		y += heights[p]
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
		return "OBVOL %s%s" % ["snap" if ind.obvol_mode == 1 else "life", " b%.4f" % ind.bucket if ind.bucket > 0.0 else ""]
	if ind.kind == "MKV":
		return "MKV (%d, s%d, t%s%s)" % [ind.period, ind.surp, "auto" if ind.auto_thresh else "%.3g" % ind.thresh, " w" if ind.weighted else ""]
	if ind.kind == "MACD":
		return "MACD (%d/%d/%d)" % [ind.fast, ind.slow, ind.sig]
	if ind.kind == "BB":
		return "BB (%d, %.1f)" % [ind.period, ind.mult]
	if ind.kind == "VOLUME":
		return "VOL"
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
	var b_mkv := Button.new()
	b_mkv.text = "add MKV"
	b_mkv.pressed.connect(_on_add_mkv)
	var b_macd := Button.new()
	b_macd.text = "add MACD"
	b_macd.pressed.connect(_on_add_macd)
	var b_rsi := Button.new()
	b_rsi.text = "add RSI"
	b_rsi.pressed.connect(_on_add_rsi)
	var b_bb := Button.new()
	b_bb.text = "add BB"
	b_bb.pressed.connect(_on_add_bb)
	var b_vol := Button.new()
	b_vol.text = "add VOL"
	b_vol.pressed.connect(_on_add_vol)
	add_row.add_child(b_sma)
	add_row.add_child(b_fft)
	add_row.add_child(b_ks)
	add_row.add_child(b_temp)
	add_row.add_child(b_cecp)
	add_row.add_child(b_obvol)
	add_row.add_child(b_mkv)
	_menu_box.add_child(add_row)
	var add_row2 := HBoxContainer.new()
	add_row2.add_child(b_macd)
	add_row2.add_child(b_rsi)
	add_row2.add_child(b_bb)
	add_row2.add_child(b_vol)
	_menu_box.add_child(add_row2)
	_add_section("trades overlays")
	if _trades_sets.is_empty():
		var tnone := Label.new()
		tnone.text = "(none)"
		_menu_box.add_child(tnone)
	for k in _trades_sets.size():
		var trow := HBoxContainer.new()
		var tcb := CheckBox.new()
		tcb.text = _trades_sets[k]["name"]
		tcb.button_pressed = _trades_sets[k]["enabled"]
		tcb.toggled.connect(_on_menu_trades_toggle.bind(k))
		trow.add_child(tcb)
		var tdel := Button.new()
		tdel.text = "x"
		tdel.tooltip_text = "delete"
		tdel.pressed.connect(_on_menu_trades_remove.bind(k))
		trow.add_child(tdel)
		var tsave := Button.new()
		tsave.text = "save"
		tsave.tooltip_text = "copy the trades binary to a file"
		tsave.pressed.connect(_on_menu_trades_save.bind(k))
		trow.add_child(tsave)
		_menu_box.add_child(trow)
	var tload := Button.new()
	tload.text = "load trades…"
	tload.pressed.connect(func() -> void: _trades_dialog.popup_centered(Vector2i(700, 500)))
	_menu_box.add_child(tload)
	if series_panel != null:
		series_panel.refresh()

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
	if ind.kind == "MKV":
		var auto := CheckBox.new()
		auto.text = "auto"
		auto.tooltip_text = "auto-calibrate thresh as auto-mult x the median |window sum| of the loaded slice"
		auto.button_pressed = ind.auto_thresh
		auto.toggled.connect(_on_menu_mkv_auto.bind(i))
		row.add_child(auto)
		var w := CheckBox.new()
		w.text = "w"
		w.tooltip_text = "weighted window: linear recency ramp (newest bar ~2x the oldest), scale-preserving"
		w.button_pressed = ind.weighted
		w.toggled.connect(_on_menu_mkv_weighted.bind(i))
		row.add_child(w)
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
	elif ind.kind == "MKV":
		_add_spin(grid, "period", ind.period, _on_menu_sma_period.bind(i))
		_add_spin(grid, "surp", ind.surp, _on_menu_mkv_surp.bind(i))
		_add_spin(grid, "thresh %", ind.thresh, _on_menu_mkv_thresh.bind(i), 0.01)
		_add_spin(grid, "auto x", ind.auto_mult, _on_menu_mkv_auto_mult.bind(i), 0.5, 30.0)
	elif ind.kind == "OBVOL":
		_add_spin(grid, "bucket", ind.bucket, _on_menu_obvol_bucket.bind(i), 0.00001)
	elif ind.kind == "MACD":
		_add_spin(grid, "fast", ind.fast, _on_menu_macd_fast.bind(i), 1.0, 500.0)
		_add_spin(grid, "slow", ind.slow, _on_menu_macd_slow.bind(i), 1.0, 1000.0)
		_add_spin(grid, "signal", ind.sig, _on_menu_macd_sig.bind(i), 1.0, 500.0)
	elif ind.kind == "RSI":
		_add_spin(grid, "period", ind.period, _on_menu_sma_period.bind(i))
	elif ind.kind == "BB":
		_add_spin(grid, "period", ind.period, _on_menu_sma_period.bind(i))
		_add_spin(grid, "mult", ind.mult, _on_menu_bb_mult.bind(i), 0.1, 10.0)
	elif ind.kind == "VOLUME":
		pass
	else:
		_add_spin(grid, "period", ind.period, _on_menu_sma_period.bind(i))
	if grid.get_child_count() > 0:
		_menu_box.add_child(grid)

func _on_menu_mode(idx: int) -> void:
	series_widget.mode = idx as SeriesWidget.Mode
	_sync_mode_hud()
	_refresh()

func _on_menu_ind_toggle(on: bool, i: int) -> void:
	indicator_widget.indicators[i].enabled = on
	_sync_indicator_hud()
	_sync_panes()
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
	_sync_panes()
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

func _on_add_macd() -> void:
	indicator_widget.add_indicator("MACD", 0)
	_rebuild_menu()
	_sync_indicator_hud()
	_sync_panes()
	_refresh()

func _on_add_rsi() -> void:
	indicator_widget.add_indicator("RSI", 14)
	_rebuild_menu()
	_sync_indicator_hud()
	_sync_panes()
	_refresh()

func _on_add_bb() -> void:
	indicator_widget.add_indicator("BB", 20)
	_rebuild_menu()
	_sync_indicator_hud()
	_refresh()

func _on_add_vol() -> void:
	indicator_widget.add_indicator("VOLUME", 0)
	_rebuild_menu()
	_sync_indicator_hud()
	_sync_panes()
	_refresh()

func _on_menu_macd_fast(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].fast = maxi(1, int(v))
		_sync_indicator_hud()
		_refresh()

func _on_menu_macd_slow(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].slow = maxi(1, int(v))
		_sync_indicator_hud()
		_refresh()

func _on_menu_macd_sig(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].sig = maxi(1, int(v))
		_sync_indicator_hud()
		_refresh()

func _on_menu_bb_mult(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].mult = maxf(0.1, v)
		_refresh()

func _on_menu_mkv_surp(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].surp = maxi(1, int(v))
		_sync_indicator_hud()
		_refresh()

func _on_menu_mkv_thresh(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].thresh = maxf(0.0001, v)
		_sync_indicator_hud()
		_refresh()

func _on_menu_mkv_auto(on: bool, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].auto_thresh = on
		_sync_indicator_hud()
		_refresh()

func _on_menu_mkv_weighted(on: bool, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].weighted = on
		_sync_indicator_hud()
		_refresh()

func _on_menu_mkv_auto_mult(v: float, i: int) -> void:
	if i < indicator_widget.indicators.size():
		indicator_widget.indicators[i].auto_mult = clampf(v, 0.5, 30.0)
		_sync_indicator_hud()
		_refresh()

func _on_add_mkv() -> void:
	indicator_widget.add_indicator("MKV", 20)
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
	_mcp_poll()
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
	for w in [grid, series_widget, indicator_widget, book_widget, cross_widget, cursor_widget, trades_widget, dtw_overlay]:
		w.queue_redraw()
	for p in _panes:
		p.queue_redraw()

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
	series_panel.refresh()
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
	series_panel.refresh()
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
	series_panel.refresh()

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
	# plot_rect already excludes the sub-pane stack
	view.fit_prices(lo, hi, view.plot_rect.size.y)

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

func _on_sim_generated(tl_name: String, path: String) -> void:
	await add_timeline(tl_name, path)
	series_panel.refresh()

func _on_series_loaded(tl_name: String, path: String) -> void:
	await add_timeline(tl_name, path)
	series_panel.refresh()

func _on_series_removed(idx: int) -> void:
	remove_timeline(idx)
	series_panel.refresh()

func _on_series_reset(i: int) -> void:
	_on_menu_series_reset(i)
	series_panel.refresh()

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

# --- MCP bridge ---------------------------------------------------------------
#
# localhost TCP listener (ephemeral port, written to user://mcp_port) so
# the `sim mcp` server and agents can talk to a live editor: list/add/
# remove series, read the view state. One request per JSON line, one
# reply per JSON line: {"id", "ok", "result"}.

var _mcp_server := TCPServer.new()
var _mcp_peer: StreamPeerTCP
var _mcp_buf := ""

func _mcp_start() -> void:
	if _mcp_server.listen(0, "127.0.0.1") != OK:
		return
	var f := FileAccess.open("user://mcp_port", FileAccess.WRITE)
	if f:
		f.store_string(str(_mcp_server.get_local_port()))
		f.close()

func _mcp_poll() -> void:
	# single-client model: a new connection replaces the previous peer
	if _mcp_server.is_connection_available():
		_mcp_peer = _mcp_server.take_connection()
		_mcp_peer.set_no_delay(true)
		_mcp_buf = ""
	if _mcp_peer == null:
		return
	if _mcp_peer.get_status() != StreamPeerTCP.STATUS_CONNECTED:
		_mcp_peer = null
		return
	while _mcp_peer != null and _mcp_peer.get_available_bytes() > 0:
		var b := _mcp_peer.get_u8()
		if b == 10:
			var line := _mcp_buf
			_mcp_buf = ""
			_mcp_handle(line)
		else:
			_mcp_buf += char(b)

func _mcp_handle(line: String) -> void:
	var req = JSON.parse_string(line)
	if typeof(req) != TYPE_DICTIONARY:
		return
	_mcp_dispatch_async(req)

# async wrapper: sync ops complete (and reply) inside the call; ops that
# await (quotes/binary loads) resume and reply when done
func _mcp_dispatch_async(req: Dictionary) -> void:
	var result = await _mcp_dispatch(str(req.get("op", "")), req.get("args", {}))
	_mcp_reply(req.get("id", 0), result)

func _mcp_reply(id: Variant, result: Variant) -> void:
	if _mcp_peer == null:
		return
	var ok := true
	var payload: Variant = result
	if typeof(result) == TYPE_DICTIONARY and result.has("_mcp_error"):
		ok = false
		payload = result["_mcp_error"]
	_mcp_peer.put_data((JSON.stringify({"id": id, "ok": ok, "result": payload}) + "\n").to_utf8_buffer())

func _mcp_dispatch(op: String, args: Dictionary) -> Variant:
	match op:
		"status":
			return {
				"series": data.series.size(),
				"playing": playing,
				"paused": paused,
				"head": head,
				"max_len": data.max_len(),
				"follow": view.follow,
			}
		"list_series":
			var out: Array = []
			for s in data.series:
				out.append({
					"name": s.name,
					"size": s.size(),
					"tf": s.tf,
					"store_key": s.store_key,
					"col_pos": s.col_pos.to_html(),
				})
			return out
		"add_timeline":
			var path := str(args.get("path", ""))
			if path.is_empty() or not FileAccess.file_exists(path):
				return {"_mcp_error": "bad or missing path"}
			var tl_name := str(args.get("name", path.get_basename().get_file()))
			var idx: int = await add_timeline(tl_name, path)
			if idx < 0:
				return {"_mcp_error": "load failed"}
			return {"idx": idx, "name": tl_name}
		"remove_series":
			var idx2 := _mcp_series_index(args)
			if idx2 < 0:
				return {"_mcp_error": "no such series"}
			remove_timeline(idx2)
			return {"removed": idx2}
		"get_view":
			var vi := view.visible_index_range()
			var vp := view.visible_price_range()
			return {
				"px_per_index": view.px_per_index,
				"px_per_price": view.px_per_price,
				"follow": view.follow,
				"visible_index": [vi.x, vi.y],
				"visible_price": [vp.x, vp.y],
				"decimals": view.decimals,
			}
		"set_follow":
			view.follow = bool(args.get("follow", true))
			return {"follow": view.follow}
		"pause":
			paused = bool(args.get("paused", true))
			return {"paused": paused}
		_:
			return {"_mcp_error": "unknown op: " + op}

func _mcp_series_index(args: Dictionary) -> int:
	if args.has("idx"):
		return int(args["idx"])
	var name := str(args.get("name", ""))
	for i in data.series.size():
		if data.series[i].name == name:
			return i
	return -1
