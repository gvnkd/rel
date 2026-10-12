class_name PlotData
extends RefCounted

class Series:
	var name := ""
	var candles: Array[Candle] = []
	var col_pos := Color.YELLOW_GREEN
	var col_neg := Color.WEB_GREEN
	var visible := true

var series: Array[Series] = []
var head := -1

func add_series(s: Series) -> void:
	series.append(s)

func max_len() -> int:
	var m := 0
	for s in series:
		m = maxi(m, s.candles.size())
	return m
