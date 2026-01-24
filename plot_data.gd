extends Node2D

var candle = [
	1.2,
	1.3,
	1.0,
	1.15
]

var candles = [
	#[1.2,
	#1.3,
	#1.0,
	#1.15],
	#[1.15,
	#1.35,
	#0.95,
	#1.10],
	#[1.10,
	#1.45,
	#0.55,
	#0.80],
]
var scale_factor = 100.0
var step_x = 22
var col_pos = Color.YELLOW_GREEN
var col_neg = Color.WEB_GREEN
var col_sha = Color.PERU

func _ready() -> void:
	scale.y = -1

func draw_body(c, ndx):
	var x = step_x * ndx
	var by = min(c[0], c[3]) * scale_factor
	var w = 20
	var col = col_pos
	var sz = c[0] - c[3]
	if sz <= 0:
		col = col_neg
		
	var h = abs(sz) * scale_factor
	draw_rect(Rect2(x,by,w,h),col)

func draw_shadow(c, ndx):
	var x = step_x * ndx
	var by = min(c[1], c[2]) * scale_factor
	var w = 2
	var sz = c[1] - c[2]
	var h = abs(sz) * scale_factor
	draw_rect(Rect2(x+20/2-1,by,w,h),col_sha)

func _draw() -> void:
	print("draw data")
	print(candles)
	var ndx = 0
	for c in candles:
		draw_shadow(c, ndx)
		draw_body(c, ndx)
		ndx += 1
