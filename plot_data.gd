class_name PlotData
extends RefCounted

class Series:
	var name := ""
	var candles: Array[Candle] = []
	var books: Array = []
	# OBVOL level lifetimes (sim tick-mode only): flat 7 floats per row
	# (c0, c1, side, price, peak, ev0, evn) in base (1m) candle indices;
	# ev0/evn address volume-change records in `levels_path` (.vev sidecar)
	var levels := PackedFloat64Array()
	var levels_path := ""
	var col_pos := Color.YELLOW_GREEN
	var col_neg := Color.WEB_GREEN
	var body_up := Color.TRANSPARENT
	var body_down := Color.TRANSPARENT
	var wick := Color.TRANSPARENT
	var ind_colors := {}
	var visible := true
	# real-quotes backing: binary store + current timeframe; `candles` is
	# then only a sliding window starting at `win_off`
	var store: QuoteStore = null
	var store_key := ""
	var tf := "1m"
	var win_off := 0
	# sim backing: full 1-minute candles in `_base`, per-tf aggregates cached
	var _base: Array[Candle] = []
	var _base_books: Array = []
	var _agg: Dictionary = {}

	# candle count at the current timeframe
	func size() -> int:
		if store != null:
			return store.count
		if tf == "1m":
			return candles.size()
		return _agg_cache().size()

	# candle at series index i; the chart ensures the store window covers
	# the visible range before drawing
	func candle_at(i: int) -> Candle:
		if store != null:
			return candles[i - win_off]
		if tf == "1m":
			return candles[i]
		return _agg_cache()[i]

	# per-candle book snapshot for index i (null when unavailable)
	func book_at(i: int) -> Variant:
		if store != null or books.is_empty():
			return null
		if tf == "1m":
			return books[i] if i < books.size() else null
		var g: int = QuoteStore.TF_MINUTES[tf]
		var bi := mini(i * g + g - 1, books.size() - 1)
		return books[bi]

	# 1-minute (base) candle count — window refill margin
	func base_count() -> int:
		if store != null:
			return store.count if tf == "1m" else -1
		return _base.size()

	func set_base(base: Array[Candle], base_books: Array, base_levels: PackedFloat64Array) -> void:
		_base = base
		_base_books = base_books
		levels = base_levels
		_agg.clear()
		candles = _base
		books = _base_books

	func switch_tf(new_tf: String) -> void:
		tf = new_tf
		if store == null and not _base.is_empty():
			candles = _base if tf == "1m" else _agg_cache()

	# aggregates the in-memory base series into timeframe candles
	# (sim data has no timestamps: plain chunking, 1m units)
	func _agg_cache() -> Array[Candle]:
		if _agg.has(tf):
			return _agg[tf]
		var g: int = QuoteStore.TF_MINUTES[tf]
		var src: Array[Candle] = _base if not _base.is_empty() else candles
		var out: Array[Candle] = []
		var i := 0
		while i < src.size():
			var c := Candle.new()
			c.open = src[i].open
			c.high = src[i].high
			c.low = src[i].low
			c.close = src[i].close
			c.vol = src[i].vol
			var last := mini(i + g - 1, src.size() - 1)
			for j in range(i + 1, last + 1):
				c.high = maxf(c.high, src[j].high)
				c.low = minf(c.low, src[j].low)
				c.close = src[j].close
				c.vol += src[j].vol
			out.append(c)
			i += g
		_agg[tf] = out
		return out

var series: Array[Series] = []
var head := -1

func add_series(s: Series) -> void:
	series.append(s)

func max_len() -> int:
	var m := 0
	for s in series:
		m = maxi(m, s.size())
	return m
