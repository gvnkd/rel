class_name ToolWindow
extends PanelContainer

@export var title := "Tool"

var _dragging := false

@onready var _bar: HBoxContainer = $Box/Bar
@onready var _title_label: Label = $Box/Bar/Title

func _ready() -> void:
	_title_label.text = title
	_bar.gui_input.connect(_on_bar_gui_input)
	$Box/Bar/Close.pressed.connect(hide)

func toggle() -> void:
	visible = not visible

func is_dragging() -> bool:
	return _dragging

func _input(event: InputEvent) -> void:
	if not _dragging:
		return
	if event is InputEventMouseMotion:
		position += (event as InputEventMouseMotion).relative
		_clamp_position()
	elif event is InputEventMouseButton and not (event as InputEventMouseButton).pressed:
		_dragging = false

func _on_bar_gui_input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		var button := event as InputEventMouseButton
		if button.button_index == MOUSE_BUTTON_LEFT and button.pressed:
			_dragging = true

func _clamp_position() -> void:
	var parent_size := (get_parent() as Control).size
	position.x = clampf(position.x, -size.x + 80.0, parent_size.x - 80.0)
	position.y = clampf(position.y, 0.0, parent_size.y - 40.0)
