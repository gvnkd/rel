extends Node2D

var step_x := 200
var step_y := 200
var grid_col := Color.GRAY

func _draw() -> void:
	var zoom: Vector2 = get_parent().zoom
	var size := get_viewport_rect().size / zoom
	for y in range(0, int(size.y), step_y):
		draw_line(Vector2(0, y), Vector2(size.x, y), grid_col)
	for x in range(0, int(size.x), step_x):
		draw_line(Vector2(x, 0), Vector2(x, size.y), grid_col)
