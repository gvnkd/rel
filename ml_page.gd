class_name MlPage
extends Control

signal equity_overlay(tl_name: String, path: String)

const DOCS_PATH := "res://docs/ml_train.md"

@onready var _docs: TextEdit = $Split/Right/Docs
@onready var _panel: MlPanel = $Split/PanelHolder/MlPanel
@onready var _plots: MlPlots = $Split/Right/Plots

func _ready() -> void:
	_panel.equity_overlay.connect(_on_panel_overlay)
	_panel.results_ready.connect(_plots.show_run)
	_panel.session_selected.connect(_plots.select)
	var f := FileAccess.open(DOCS_PATH, FileAccess.READ)
	if f != null:
		_docs.text = f.get_as_text()
		f.close()
	else:
		_docs.text = "docs/ml_train.md not found"

func _on_panel_overlay(tl_name: String, path: String) -> void:
	equity_overlay.emit(tl_name, path)
