extends Control

const PlotDataScript := preload("res://plot_data.gd")

const ZOOM_FACTOR := 1.2

@export var replay_rate := 60.0

var view := ChartView.new()
var data: PlotData = PlotDataScript.new()

var head := -1
var playing := false
var paused := false
var pan_mode := false
var t_acc := 0.0

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

func _ready() -> void:
	for w in [grid, series_widget, indicator_widget, book_widget, cross_widget, cursor_widget]:
		w.view = view
		w.data = data
	cursor_widget.mode_text = SeriesWidget.MODE_NAMES[series_widget.mode]
	cursor_widget.indicators_text = "on" if indicator_widget.enabled else "off"
	cursor_widget.cross = cross_widget
	view.layout(size)
	sim_panel.timeline_added.connect(_on_timeline_added)
	sim_panel.timeline_removed.connect(_on_timeline_removed)
	sim_panel.timeline_toggled.connect(_on_timeline_toggled)

func _notification(what: int) -> void:
	if what == NOTIFICATION_RESIZED:
		view.layout(size)
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
	elif event is InputEventMouseMotion:
		var motion := event as InputEventMouseMotion
		cross_widget.mouse = motion.position
		if not sim_window.is_dragging() and not _over_ui(motion.position):
			if motion.button_mask & MOUSE_BUTTON_MASK_RIGHT != 0:
				view.scale_by(motion.relative, motion.position)
			elif motion.button_mask & (MOUSE_BUTTON_MASK_LEFT | MOUSE_BUTTON_MASK_MIDDLE) != 0 or pan_mode:
				view.pan_by(motion.relative)
		_refresh()

func _over_ui(pos: Vector2) -> bool:
	return sim_window.visible and sim_window.get_global_rect().has_point(pos)

func _unhandled_key_input(event: InputEvent) -> void:
	var key := event as InputEventKey
	if key == null or not key.pressed or key.echo:
		return
	if key.keycode == KEY_M:
		series_widget.cycle_mode()
		cursor_widget.mode_text = SeriesWidget.MODE_NAMES[series_widget.mode]
		_refresh()
	elif key.keycode == KEY_I:
		indicator_widget.enabled = not indicator_widget.enabled
		cursor_widget.indicators_text = "on" if indicator_widget.enabled else "off"
		_refresh()
	elif key.keycode == KEY_F1:
		sim_window.toggle()

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
	_refresh()
	if head >= data.max_len():
		playing = false

func _refresh() -> void:
	if grid == null:
		return
	for w in [grid, series_widget, indicator_widget, book_widget, cross_widget, cursor_widget]:
		w.queue_redraw()

func add_timeline(tl_name: String, path: String) -> int:
	var s := PlotDataScript.Series.new()
	s.name = tl_name
	s.candles = _load_candles(path)
	var idx: int = data.series.size()
	var col: Color = PALETTE[idx % PALETTE.size()]
	s.col_pos = col
	s.col_neg = col.darkened(0.35)
	data.add_series(s)
	if data.series.size() == 1:
		_fit_view(s)
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

func _fit_view(s) -> void:
	if s.candles.is_empty():
		return
	var lo: float = s.candles[0].low
	var hi: float = s.candles[0].high
	for c in s.candles:
		lo = minf(lo, c.low)
		hi = maxf(hi, c.high)
	view.fit_prices(lo, hi)

func _update_book() -> void:
	book_widget.clear()
	for s in data.series:
		if not s.visible or s.candles.is_empty():
			continue
		var n := s.candles.size()
		var probe := mini(n, 100)
		var span_sum := 0.0
		for i in range(n - probe, n):
			span_sum += s.candles[i].high - s.candles[i].low
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
