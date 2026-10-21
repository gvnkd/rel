class_name DtwPage
extends Control

@onready var _panel: DtwPanel = $Split/PanelHolder/DtwPanel
@onready var _plots: DtwPlots = $Split/Plots

func _ready() -> void:
	_panel.plot_requested.connect(_plots.show_run)
	_plots.drill_requested.connect(_on_drill)
	_panel.drill_done.connect(_plots.set_drill)

func _on_drill(i: int, j: int) -> void:
	_panel._on_drill(i, j, _plots.session_meta())
