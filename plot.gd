extends Node2D

var t: float = 0.0
var zoom: Vector2 = Vector2(1.0, 1.0)
var pan_mode: bool = false

func _ready() -> void:
	$Grid.translate(Vector2(-100, -500))
func _draw() -> void:
	scale = zoom
	for ch in get_children():
		print("draw plot")
		ch.queue_redraw()

func _process(delta: float) -> void:
	pass
	#t += delta
	#zoom = Vector2(sin(t), sin(t))
	#queue_redraw()

func _input(event: InputEvent) -> void:
	if pan_mode and event is InputEventMouseMotion:
		$Camera2D.translate(event.screen_relative)

	if event.is_action_pressed("zoom_in"):
		zoom *= 1.1
		queue_redraw()
	elif event.is_action_pressed("zoom_out"):
		zoom *= 0.9
		queue_redraw()
	elif event.is_action_pressed("pan_mode"):
		pan_mode = true
	elif event.is_action_released("pan_mode"):
		pan_mode = false
