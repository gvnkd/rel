extends Node2D

const BODY_W := 20.0
const SHADOW_W := 2.0

var candles: Array[Candle] = []
var current: Candle

var scale_factor := 100000.0
var base_price := 0.0
var step_x := 22.0
var col_pos := Color.YELLOW_GREEN
var col_neg := Color.WEB_GREEN
var col_sha := Color.PERU

func _ready() -> void:
	scale.y = -1

func _price_y(p: float) -> float:
	if base_price == 0.0:
		base_price = p
	return (p - base_price) * scale_factor

func _draw() -> void:
	for i in candles.size():
		_draw_candle(candles[i], i)
	if current != null and not current.is_empty():
		_draw_candle(current, candles.size())

func _draw_candle(c: Candle, ndx: int) -> void:
	_draw_shadow(c, ndx)
	_draw_body(c, ndx)

func _draw_body(c: Candle, ndx: int) -> void:
	var x := step_x * ndx
	var by := _price_y(minf(c.open, c.close))
	var h := absf(c.open - c.close) * scale_factor
	var col := col_neg if c.close >= c.open else col_pos
	draw_rect(Rect2(x, by, BODY_W, h), col)

func _draw_shadow(c: Candle, ndx: int) -> void:
	var x := step_x * ndx
	var by := _price_y(c.low)
	var h := (c.high - c.low) * scale_factor
	draw_rect(Rect2(x + BODY_W / 2 - SHADOW_W / 2, by, SHADOW_W, h), col_sha)
