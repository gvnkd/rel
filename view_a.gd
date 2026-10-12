extends Sprite2D

@export var hud_path: NodePath = "../HUD"

var t := 0.0

var radius := 1.0
var scale_factor := 100.0
var points_a: Array[Vector3] = []
var points_b: Array[Vector3] = []
var dist_x := 256.0
var link_ab := [0, 0]

@onready var hud: Control = get_node(hud_path)

func _ready() -> void:
	for a in Vector3(0.0, TAU, TAU / 5):
		var z := radius * sin(a)
		var y := radius * cos(a)
		points_a.push_back(Vector3(0.0, y, z).normalized())
		points_b.push_back(Vector3(0.0, y, z).normalized())

func _draw() -> void:
	var x0 := 256.0
	var y0 := 256.0
	for p in points_a:
		draw_line(Vector2(x0, y0), Vector2(x0 + p.x * scale_factor, y0 + p.y * scale_factor), Color.ROSY_BROWN)
	for p in points_b:
		draw_line(Vector2(x0 + dist_x, y0), Vector2(x0 + p.x * scale_factor + dist_x, y0 + p.y * scale_factor), Color.BURLYWOOD)

	var p_a := points_a[link_ab[0]] * scale_factor
	var p_b := points_b[link_ab[1]] * scale_factor
	draw_line(Vector2(x0 + p_a.x, y0 + p_a.y), Vector2(x0 + p_b.x + dist_x, y0 + p_b.y), Color.TOMATO)

func _process(delta: float) -> void:
	t += delta
	for i in points_a.size():
		points_a[i] = points_a[i].rotated(Vector3.RIGHT, delta)
		points_b[i] = points_b[i].rotated(Vector3.LEFT, delta)

	if t > 2:
		link_ab[0] = (link_ab[0] + 1) % 5
		link_ab[1] = (link_ab[1] + 1) % 5
		t = 0

	hud.set_distance(points_a[link_ab[0]].distance_to(points_b[link_ab[1]]))
	queue_redraw()
