#@tool
extends Node2D

var ohlc: Vector4 = Vector4(
	1.02,
	1.10,
	0.90,
	1.05,
)
var _scale: Vector2i = Vector2i(128, 128) # distance 0..1 in pixels
var body_scale: Vector2 = Vector2(1.0, 1.0)
var shadow_scale: Vector2 = Vector2(1.0, 1.0)
var body_width_px: int = 40
var shadow_width_px: int = 8

func _ready() -> void:
	var img:Image = Image.create(1,1,false, Image.FORMAT_RGBA8)
	img.fill(Color.RED)
	var tex = ImageTexture.create_from_image(img)
	$Body.texture = tex
	set_ohlc(Vector4(
		1.02,
		1.10,
		0.90,
		1.05,
	))

func _draw() -> void:
	pass
	#$Body.scale *= Vector2(_scale)

func set_ohlc(x: Vector4) -> void:
	ohlc = x
	var body_size_px: int = abs(ohlc.w - ohlc.x) * _scale.x
	var shadow_size_px: int = abs(ohlc.y - ohlc.z) * _scale.y
	var _s = Vector2i(body_width_px, body_size_px)
	body_scale = Vector2(_s)
	$Body.scale = body_scale* 2
