extends Node2D

const ZOOM_FACTOR := 1.1

var zoom := Vector2.ONE
var pan_mode := false

@onready var camera: Camera2D = $Camera2D

func _ready() -> void:
	$Grid.translate(Vector2(-100, -500))

func _input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		var button := event as InputEventMouseButton
		if button.pressed and button.button_index == MOUSE_BUTTON_WHEEL_UP:
			_zoom_at(1.0 / ZOOM_FACTOR, button.position)
		elif button.pressed and button.button_index == MOUSE_BUTTON_WHEEL_DOWN:
			_zoom_at(ZOOM_FACTOR, button.position)
	elif event is InputEventMouseMotion:
		var motion := event as InputEventMouseMotion
		var dragging := motion.button_mask & (MOUSE_BUTTON_MASK_LEFT | MOUSE_BUTTON_MASK_MIDDLE) != 0
		if dragging or pan_mode:
			camera.position -= motion.relative / zoom
	elif event.is_action_pressed("pan_mode"):
		pan_mode = true
	elif event.is_action_released("pan_mode"):
		pan_mode = false

func _zoom_at(factor: float, pivot: Vector2) -> void:
	var old := zoom.x
	var new := old * factor
	var offset := pivot - get_viewport_rect().size / 2
	camera.position += offset / old - offset / new
	zoom = Vector2(new, new)
	scale = zoom
	$Grid.queue_redraw()
