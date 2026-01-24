extends Node2D

var vline: Sprite2D
var hline: Sprite2D
var step_x: int = 200
var step_y: int = 200
var img: Image
var tex: ImageTexture
var grid_col: Color = Color.GRAY

func _ready() -> void:
	vline = $VLine
	hline = $HLine
	img = Image.create(1,1,false, Image.FORMAT_RGBA8)
	img.fill(Color.GRAY)
	tex = ImageTexture.create_from_image(img)
	vline.texture = tex
	#hline.texture = tex
	update_grid()

func _draw() -> void:
	var zoom = get_parent().zoom
	var vp = get_viewport_rect()
	var size = vp.size / zoom
	#print(size)
	for y in range(0, size.y, step_y):
		draw_line(Vector2(0, y), Vector2(size.x, y), grid_col)
		for x in range(0, size.x, step_x):
			draw_line(Vector2(x, 0), Vector2(x, size.y), grid_col)
	
func update_grid():
	pass
	#img.resize(vp.size.x, vp.size.y,Image.INTERPOLATE_BILINEAR)
	#img.fill(Color.RED)
	#tex = ImageTexture.create_from_image(img)
	#tex.update(img)
	#tex.set_size_override(vp.size)
	#var canvas = get_canvas()
	#tex.draw_rect(canvas, Rect2(10,10,100,100), false, Color.RED,false)
	#vline.scale = Vector2(vp.size.x, vp.size.y)
	#hline.scale = Vector2(10.0, screen.y)
