extends Control

const PlotDataScript := preload("res://plot_data.gd")

const ZOOM_FACTOR := 1.2
const VOL_GRIP := 4.0
const VOL_SNAP := 0.02
const VOL_MAX_FRAC := 0.6
const RMB_CLICK_TOL := 4.0
const ID_IND_TOGGLE_BASE := 100
const SUB_ID_DELETE := 1
const SERIES_ID_BODY_UP := 1
const SERIES_ID_BODY_DOWN := 2
const SERIES_ID_WICK := 3
const SERIES_ID_RESET := 4
const SERIES_ID_IND_COLOR_BASE := 100
const SUB_ID_FFT_START := 10
const SUB_ID_FFT_END := 11
const SUB_ID_FFT_PREDICT := 12
const SUB_ID_FFT_WAVES := 13

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
enum PickTarget { NONE, SERIES_BODY_UP, SERIES_BODY_DOWN, SERIES_WICK, SERIES_IND }

var _popup: PopupMenu
var _color_popup: PopupPanel
var _picker: ColorPicker
var _num_popup: PopupPanel
var _num_edit: LineEdit
var _num_idx := -1
var _num_field := -1
var _pick_target: PickTarget = PickTarget.NONE
var _pick_idx := -1
var _pick_series := -1

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

func _ready() -> void:
	for w in [grid, series_widget, indicator_widget, volume_widget, book_widget, cross_widget, cursor_widget]:
		w.view = view
		w.data = data
	_sync_volume()
	_sync_mode_hud()
	_sync_indicator_hud()
	cursor_widget.cross = cross_widget
	view.layout(size)
	sim_panel.timeline_added.connect(_on_timeline_added)
	sim_panel.timeline_removed.connect(_on_timeline_removed)
	sim_panel.timeline_toggled.connect(_on_timeline_toggled)
	_popup = PopupMenu.new()
	_popup.hide_on_checkable_item_selection = false
	_popup.id_pressed.connect(_on_popup_id)
	add_child(_popup)
	_color_popup = PopupPanel.new()
	_picker = ColorPicker.new()
	_picker.color_changed.connect(_on_picker_color)
	_color_popup.add_child(_picker)
	add_child(_color_popup)
	_num_popup = PopupPanel.new()
	_num_edit = LineEdit.new()
	_num_edit.custom_minimum_size = Vector2(100, 0)
	_num_edit.text_submitted.connect(_on_num_submit)
	_num_popup.add_child(_num_edit)
	add_child(_num_popup)

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
		elif not sim_window.is_dragging() and not _over_ui(motion.position):
			if motion.button_mask & MOUSE_BUTTON_MASK_RIGHT != 0:
				view.scale_by(motion.relative, motion.position)
				view.follow = false
			elif motion.button_mask & (MOUSE_BUTTON_MASK_LEFT | MOUSE_BUTTON_MASK_MIDDLE) != 0 or pan_mode:
				view.pan_by(motion.relative)
				view.follow = false
		_refresh()

func _over_ui(pos: Vector2) -> bool:
	return sim_window.visible and sim_window.get_global_rect().has_point(pos)

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
		return "FFT [%d..%d]+%d%s" % [ind.start, ind.end, ind.predict, w]
	return "%s (%d)" % [ind.kind, ind.period]

func _open_popup(at: Vector2) -> void:
	for c in _popup.get_children():
		_popup.remove_child(c)
		c.free()
	_popup.clear()
	_popup.add_separator("chart mode")
	for m in SeriesWidget.MODE_NAMES.size():
		_popup.add_radio_check_item(SeriesWidget.MODE_NAMES[m], m)
		_popup.set_item_checked(_popup.item_count - 1, series_widget.mode == m)
	_popup.add_separator("indicators")
	if indicator_widget.indicators.is_empty():
		_popup.add_item("(none)")
		_popup.set_item_disabled(_popup.item_count - 1, true)
	for i in indicator_widget.indicators.size():
		var ind: IndicatorWidget.Indicator = indicator_widget.indicators[i]
		var label := _ind_label(ind)
		_popup.add_check_item(label, ID_IND_TOGGLE_BASE + i)
		_popup.set_item_checked(_popup.item_count - 1, ind.enabled)
		_popup.add_submenu_item(label + "…", _build_ind_submenu(i).name)
	var add_sub := PopupMenu.new()
	add_sub.name = "add_indicator"
	add_sub.add_item("SMA")
	add_sub.add_item("FFT forecast")
	add_sub.id_pressed.connect(_on_add_indicator)
	_popup.add_child(add_sub)
	_popup.add_separator()
	_popup.add_submenu_item("add indicator", add_sub.name)
	if not data.series.is_empty():
		_popup.add_separator("series")
		for i in data.series.size():
			var s: PlotData.Series = data.series[i]
			_popup.add_submenu_item(s.name, _build_series_submenu(i).name)
			_set_color_chip(_popup, _popup.item_count - 1, s.col_pos)
	_popup.popup(Rect2i((get_screen_position() + at).round(), Vector2i(1, 1)))

func _build_series_submenu(si: int) -> PopupMenu:
	var s: PlotData.Series = data.series[si]
	var sub := PopupMenu.new()
	sub.name = "series_%d" % si
	sub.add_item("body up", SERIES_ID_BODY_UP)
	_set_color_chip(sub, sub.item_count - 1, s.body_up if s.body_up.a > 0.0 else s.col_pos)
	sub.add_item("body down", SERIES_ID_BODY_DOWN)
	_set_color_chip(sub, sub.item_count - 1, s.body_down if s.body_down.a > 0.0 else s.col_neg)
	sub.add_item("wick", SERIES_ID_WICK)
	_set_color_chip(sub, sub.item_count - 1, s.wick if s.wick.a > 0.0 else s.col_pos)
	sub.add_separator()
	sub.add_item("reset", SERIES_ID_RESET)
	if not indicator_widget.indicators.is_empty():
		sub.add_separator()
		for k in indicator_widget.indicators.size():
			var ind: IndicatorWidget.Indicator = indicator_widget.indicators[k]
			sub.add_item(_ind_label(ind) + " color", SERIES_ID_IND_COLOR_BASE + k)
			_set_color_chip(sub, sub.item_count - 1, indicator_widget.color_for(s, k))
	sub.id_pressed.connect(_on_series_color_id.bind(si))
	_popup.add_child(sub)
	return sub

func _set_color_chip(pm: PopupMenu, idx: int, col: Color) -> void:
	var chip := Image.create(12, 12, false, Image.FORMAT_RGBA8)
	chip.fill(col)
	pm.set_item_icon(idx, ImageTexture.create_from_image(chip))

func _build_ind_submenu(idx: int) -> PopupMenu:
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[idx]
	var sub := PopupMenu.new()
	sub.name = "ind_%d" % idx
	if ind.kind == "FFT":
		sub.add_item("start: %d" % ind.start, SUB_ID_FFT_START)
		sub.add_item("end: %d" % ind.end, SUB_ID_FFT_END)
		sub.add_item("predict: %d" % ind.predict, SUB_ID_FFT_PREDICT)
		sub.add_item(("waves: %d" % ind.waves) if ind.waves > 0 else "waves: auto", SUB_ID_FFT_WAVES)
	else:
		sub.add_separator("period")
		for p: int in IndicatorWidget.PERIODS:
			sub.add_radio_check_item(str(p), p)
			sub.set_item_checked(sub.item_count - 1, ind.period == p)
	sub.add_separator()
	sub.add_item("delete", SUB_ID_DELETE)
	sub.id_pressed.connect(_on_ind_submenu_id.bind(idx))
	_popup.add_child(sub)
	return sub

func _on_popup_id(id: int) -> void:
	if id >= ID_IND_TOGGLE_BASE:
		var ind: IndicatorWidget.Indicator = indicator_widget.indicators[id - ID_IND_TOGGLE_BASE]
		ind.enabled = not ind.enabled
		_sync_indicator_hud()
		_refresh()
	elif id < SeriesWidget.MODE_COUNT:
		series_widget.mode = id as SeriesWidget.Mode
		_sync_mode_hud()
		_popup.hide()
		_refresh()

func _on_ind_submenu_id(id: int, idx: int) -> void:
	if id == SUB_ID_DELETE:
		indicator_widget.remove_indicator(idx)
		_popup.hide()
		_sync_indicator_hud()
		_refresh()
	elif id == SUB_ID_FFT_START or id == SUB_ID_FFT_END or id == SUB_ID_FFT_PREDICT or id == SUB_ID_FFT_WAVES:
		_open_num_input(idx, id)
	else:
		indicator_widget.indicators[idx].period = id
		_popup.hide()
		_sync_indicator_hud()
		_refresh()

func _open_num_input(idx: int, field: int) -> void:
	_num_idx = idx
	_num_field = field
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[idx]
	var current := ind.start
	if field == SUB_ID_FFT_END:
		current = ind.end
	elif field == SUB_ID_FFT_PREDICT:
		current = ind.predict
	elif field == SUB_ID_FFT_WAVES:
		current = ind.waves
	_num_edit.text = str(current)
	_popup.hide()
	_num_popup.position = _popup.position + Vector2i(maxi(_popup.size.x, 1) + 8, 0)
	_num_popup.popup()
	_num_edit.grab_focus()
	_num_edit.select_all()

func _on_num_submit(text: String) -> void:
	_num_popup.hide()
	if _num_idx < 0 or _num_idx >= indicator_widget.indicators.size():
		return
	if not text.is_valid_int() or text.to_int() < 0:
		return
	var v := text.to_int()
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[_num_idx]
	if _num_field == SUB_ID_FFT_START:
		ind.start = v
	elif _num_field == SUB_ID_FFT_END:
		ind.end = v
	elif _num_field == SUB_ID_FFT_PREDICT:
		ind.predict = v
	elif _num_field == SUB_ID_FFT_WAVES:
		ind.waves = v
	_num_idx = -1
	_sync_indicator_hud()
	_refresh()

func _on_series_color_id(id: int, si: int) -> void:
	var s: PlotData.Series = data.series[si]
	if id == SERIES_ID_RESET:
		s.body_up = Color.TRANSPARENT
		s.body_down = Color.TRANSPARENT
		s.wick = Color.TRANSPARENT
		_popup.hide()
		_refresh()
		return
	if id >= SERIES_ID_IND_COLOR_BASE:
		var k := id - SERIES_ID_IND_COLOR_BASE
		_open_color_picker(PickTarget.SERIES_IND, k, si, indicator_widget.color_for(s, k))
		return
	var target := PickTarget.SERIES_BODY_UP
	var current := s.body_up if s.body_up.a > 0.0 else s.col_pos
	if id == SERIES_ID_BODY_DOWN:
		target = PickTarget.SERIES_BODY_DOWN
		current = s.body_down if s.body_down.a > 0.0 else s.col_neg
	elif id == SERIES_ID_WICK:
		target = PickTarget.SERIES_WICK
		current = s.wick if s.wick.a > 0.0 else s.col_pos
	_open_color_picker(target, -1, si, current)

func _open_color_picker(target: PickTarget, idx: int, si: int, current: Color) -> void:
	_pick_target = target
	_pick_idx = idx
	_pick_series = si
	_picker.color = current
	_popup.hide()
	_color_popup.position = _popup.position + Vector2i(maxi(_popup.size.x, 1) + 8, 0)
	_color_popup.popup()

func _on_picker_color(c: Color) -> void:
	if _pick_series < 0 or _pick_series >= data.series.size():
		return
	var s: PlotData.Series = data.series[_pick_series]
	match _pick_target:
		PickTarget.SERIES_BODY_UP:
			s.body_up = c
		PickTarget.SERIES_BODY_DOWN:
			s.body_down = c
		PickTarget.SERIES_WICK:
			s.wick = c
		PickTarget.SERIES_IND:
			s.ind_colors[_pick_idx] = c
	_refresh()

func _begin_fft_drag(hit: Array) -> void:
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[hit[0]]
	# materialize auto (0) values so the drag works against concrete indices
	if ind.end <= 0:
		ind.end = data.max_len()
	if ind.start <= 0:
		ind.start = maxi(0, ind.end - 256)
	_fft_drag = hit
	Input.set_default_cursor_shape(Input.CURSOR_HSIZE)
	_refresh()

func _drag_fft_to(pos: Vector2) -> void:
	var ind: IndicatorWidget.Indicator = indicator_widget.indicators[_fft_drag[0]]
	var i := maxi(0, int(round(view.x_to_index(pos.x))))
	if _fft_drag[1] == 0:
		ind.start = clampi(i - 1, 0, ind.end - IndicatorWidget.FFT_MIN_WINDOW)
	else:
		ind.end = maxi(i, ind.start + IndicatorWidget.FFT_MIN_WINDOW)
	_sync_indicator_hud()
	_refresh()

func _on_add_indicator(id: int) -> void:
	if id == 1:
		indicator_widget.add_indicator("FFT", 0)
		var ind: IndicatorWidget.Indicator = indicator_widget.indicators[indicator_widget.indicators.size() - 1]
		ind.end = data.max_len()
		ind.start = maxi(0, ind.end - 256)
	else:
		indicator_widget.add_indicator("SMA", 20)
	_popup.hide()
	_sync_indicator_hud()
	_refresh()

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
	for w in [grid, series_widget, indicator_widget, volume_widget, book_widget, cross_widget, cursor_widget]:
		w.queue_redraw()

func add_timeline(tl_name: String, path: String) -> int:
	var s := PlotDataScript.Series.new()
	s.name = tl_name
	var ticks_path := "%s.ticks.csv" % path.get_basename()
	if FileAccess.file_exists(ticks_path):
		var t := _load_ticks(ticks_path)
		s.candles = t["candles"]
		s.books = t["books"]
	else:
		s.candles = _load_candles(path)
	var idx: int = data.series.size()
	var col: Color = PALETTE[idx % PALETTE.size()]
	s.col_pos = col
	s.col_neg = col.darkened(0.35)
	data.add_series(s)
	if data.series.size() == 1:
		_read_meta(path)
		_fit_view(s)
	view.follow = true
	head = 0
	data.head = head
	playing = true
	_update_book()
	_refresh()
	return idx

func remove_timeline(idx: int) -> void:
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
	if s.candles.is_empty():
		return
	var lo: float = s.candles[0].low
	var hi: float = s.candles[0].high
	for c in s.candles:
		lo = minf(lo, c.low)
		hi = maxf(hi, c.high)
	view.fit_prices(lo, hi, view.plot_rect.size.y - vol_pane_h())

func _update_book() -> void:
	book_widget.clear()
	for s in data.series:
		if not s.visible or s.candles.is_empty():
			continue
		var n := s.candles.size()
		var i := clampi(head - 1, 0, n - 1)
		if not s.books.is_empty() and i < s.books.size():
			var snap: Dictionary = s.books[i]
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
		var probe := mini(n, 100)
		var span_sum := 0.0
		for k in range(n - probe, n):
			span_sum += s.candles[k].high - s.candles[k].low
		var step := maxf(span_sum / probe / 4.0, 1.0e-9)
		book_widget.set_ladder(s.candles[n - 1].close, step)
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
	return { "candles": candles, "books": books }

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
