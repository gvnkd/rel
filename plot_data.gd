extends Node2D

const BODY_W := 20.0
const SHADOW_W := 2.0

class Series:
	var name := ""
	var candles: Array[Candle] = []
	var current: Candle
	var col_pos := Color.YELLOW_GREEN
	var col_neg := Color.WEB_GREEN
	var visible := true
	var base_price := 0.0

var series: Array[Series] = []

var scale_factor := 100000.0
var step_x := 22.0
var window := 2000
var head := -1
var col_sha := Color.PERU

func _ready() -> void:
	scale.y = -1

func add_series(s: Series) -> void:
	series.append(s)

func _draw() -> void:
	for s in series:
		if not s.visible:
			continue
		var total := s.candles.size()
		var end := total if head < 0 else mini(head + 1, total)
		var start := maxi(0, end - window)
		for i in range(start, end):
			_draw_candle(s, s.candles[i], i - start)
		if s.current != null and not s.current.is_empty() and (head < 0 or end >= total):
			_draw_candle(s, s.current, end - start)

func _price_y(s: Series, p: float) -> float:
	if s.base_price == 0.0:
		s.base_price = p
	return (p - s.base_price) * scale_factor

func _draw_candle(s: Series, c: Candle, ndx: int) -> void:
	var x := step_x * ndx
	var by := _price_y(s, minf(c.open, c.close))
	var h := absf(c.open - c.close) * scale_factor
	var col := s.col_neg if c.close >= c.open else s.col_pos
	draw_rect(Rect2(x, by, BODY_W, h), col)
	var sy := _price_y(s, c.low)
	var sh := (c.high - c.low) * scale_factor
	draw_rect(Rect2(x + BODY_W / 2 - SHADOW_W / 2, sy, SHADOW_W, sh), col_sha)
