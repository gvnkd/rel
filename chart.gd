extends Node2D

const PlotDataScript := preload("res://plot_data.gd")

@export var replay_rate := 60.0

var head := -1
var playing := false
var paused := false
var t_acc := 0.0

const PALETTE: Array[Color] = [
	Color(0.56, 0.93, 0.56),
	Color(0.96, 0.64, 0.38),
	Color(0.47, 0.71, 0.97),
	Color(0.93, 0.45, 0.66),
	Color(0.72, 0.56, 0.97),
	Color(0.93, 0.88, 0.42),
]

@onready var plot_data: Node2D = $Plot/Data
@onready var sim_panel: SimPanel = $SimPanel

func _ready() -> void:
	sim_panel.timeline_added.connect(_on_timeline_added)
	sim_panel.timeline_removed.connect(_on_timeline_removed)
	sim_panel.timeline_toggled.connect(_on_timeline_toggled)

func _input(event: InputEvent) -> void:
	if event.is_action_pressed("pause"):
		paused = not paused
	elif event.is_action_pressed("speed_up"):
		replay_rate *= 2.0
	elif event.is_action_pressed("speed_down"):
		replay_rate = maxf(replay_rate / 2.0, 1.0)

func _process(delta: float) -> void:
	if not playing or paused:
		return
	t_acc += delta * replay_rate
	var n := int(t_acc)
	if n <= 0:
		return
	t_acc -= n
	head = mini(head + n, _max_len())
	plot_data.head = head
	plot_data.queue_redraw()
	if head >= _max_len():
		playing = false

func _max_len() -> int:
	var m := 0
	for s in plot_data.series:
		m = maxi(m, s.candles.size())
	return m

func add_timeline(tl_name: String, path: String) -> int:
	var s := PlotDataScript.Series.new()
	s.name = tl_name
	s.candles = _load_candles(path)
	var idx: int = plot_data.series.size()
	var col: Color = PALETTE[idx % PALETTE.size()]
	s.col_pos = col
	s.col_neg = col.darkened(0.35)
	plot_data.add_series(s)
	head = 0
	plot_data.head = head
	plot_data.queue_redraw()
	playing = true
	return idx

func remove_timeline(idx: int) -> void:
	plot_data.series.remove_at(idx)
	for i in plot_data.series.size():
		var col: Color = PALETTE[i % PALETTE.size()]
		plot_data.series[i].col_pos = col
		plot_data.series[i].col_neg = col.darkened(0.35)
	head = -1
	plot_data.head = head
	plot_data.queue_redraw()

func set_timeline_visible(idx: int, vis: bool) -> void:
	plot_data.series[idx].visible = vis
	plot_data.queue_redraw()

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
		loaded.append(c)
	return loaded
