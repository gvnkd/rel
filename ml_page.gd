class_name MlPage
extends Control

signal equity_overlay(tl_name: String, path: String)

const DOCS_PATH := "res://docs/ml_train.md"

@onready var _docs: TextEdit = $Split/Docs
@onready var _panel: MlPanel = $Split/PanelHolder/MlPanel

func _ready() -> void:
	_panel.equity_overlay.connect(_on_panel_overlay)
	var f := FileAccess.open(DOCS_PATH, FileAccess.READ)
	if f != null:
		_docs.text = f.get_as_text()
		f.close()
	else:
		_docs.text = "docs/ml_train.md not found"

func _on_panel_overlay(tl_name: String, path: String) -> void:
	equity_overlay.emit(tl_name, path)
