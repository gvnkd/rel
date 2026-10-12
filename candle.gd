class_name Candle
extends RefCounted

var open: float = 0.0
var high: float = 0.0
var low: float = 0.0
var close: float = 0.0
var vol: int = 0

func is_empty() -> bool:
	return open == 0.0

func update(p: float) -> void:
	if is_empty():
		open = p
		high = p
		low = p
	elif p > high:
		high = p
	elif p < low:
		low = p
	close = p
