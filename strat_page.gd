class_name StratPage
extends Control

@onready var _panel: StratPanel = $Split/PanelHolder/StratPanel
@onready var _plots: StratPlots = $Split/Plots

func _ready() -> void:
	_panel.results_ready.connect(_plots.show_run)
	_panel.session_selected.connect(_plots.select)
