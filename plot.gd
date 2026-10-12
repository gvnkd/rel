extends Node2D

var zoom := Vector2.ONE
var pan_mode := false

func _ready() -> void:
	$Grid.translate(Vector2(-100, -500))

func _input(event: InputEvent) -> void:
	if pan_mode and event is InputEventMouseMotion:
		$Camera2D.translate(event.screen_relative)
	elif event.is_action_pressed("zoom_in"):
		_set_zoom(zoom * 1.1)
	elif event.is_action_pressed("zoom_out"):
		_set_zoom(zoom * 0.9)
	elif event.is_action_pressed("pan_mode"):
		pan_mode = true
	elif event.is_action_released("pan_mode"):
		pan_mode = false

func _set_zoom(z: Vector2) -> void:
	zoom = z
	scale = zoom
	$Grid.queue_redraw()
