class_name DtwPage
extends Control

@onready var _panel: DtwPanel = $Split/PanelHolder/DtwPanel
@onready var _plots: DtwPlots = $Split/Plots

func _ready() -> void:
	_panel.plot_requested.connect(_plots.show_run)
	_panel.overlay_requested.connect(_on_overlay_request)
	_plots.drill_requested.connect(_on_drill)
	_plots.locate_requested.connect(_on_locate)
	_panel.drill_done.connect(_plots.set_drill)

func _on_drill(i: int, j: int) -> void:
	_panel._on_drill(i, j, _plots.session_meta())

# matched-window overlay on the Chart tab: load the matches into the plots
# (shared by identity) and hand them to the chart widget
func _on_overlay_request(dir: String, base: String, meta: Dictionary) -> void:
	var n := _plots.show_run(dir, base, meta)
	if n <= 0:
		return
	var chart := get_tree().root.find_child("Chart", true, false)
	if chart != null and chart.has_method("set_dtw_overlay"):
		chart.set_dtw_overlay(_plots._matches, int(meta.get("win", 0)))

# the chart overlay calls here on rect click: switch to the DTW tab and
# drill into that window
func show_drill(i: int, j: int) -> void:
	var tabs := get_parent() as TabContainer
	if tabs != null:
		tabs.current_tab = get_index()
	if j >= 0:
		_panel._on_drill(i, j, _plots.session_meta())

# inverse: from the drill view, open the window's price range on the Chart tab
func _on_locate(i0: int, i1: int) -> void:
	var tabs := get_parent() as TabContainer
	if tabs == null:
		return
	var chart_tab := tabs.get_node_or_null("Chart")
	if chart_tab == null:
		return
	tabs.current_tab = chart_tab.get_index()
	chart_tab.focus_index_range(i0, i1)
