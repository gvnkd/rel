class_name SubPaneWidget
extends Control

# one pane of the stack below the price chart (volume / MACD / RSI). The
# panes share the graph's index->x transform (view) but have their own y
# scale. chart.gd owns the stack layout (pane_rect assignment, heights,
# collapse state) and the collapse-button hit test.

const PANE_BG := Color(0.085, 0.095, 0.115)
const SEP_COL := Color(1.0, 1.0, 1.0, 0.2)
const LABEL_COL := Color(0.72, 0.75, 0.8)
const FONT_SIZE := 12
const HEADER_H := 18.0
const PANE_H := 90.0
const BTN_W := 16.0
const TOP_PAD := 6.0

var view: ChartView
var data: PlotData
var indicators: IndicatorWidget
var kind := "volume"
var ind: IndicatorWidget.Indicator = null
var pane_rect := Rect2()
var collapsed := false

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

static func fmt_vol(v: float) -> String:
	if v >= 1.0e9:
		return "%.1fB" % (v / 1.0e9)
	if v >= 1.0e6:
		return "%.1fM" % (v / 1.0e6)
	if v >= 1.0e3:
		return "%.1fK" % (v / 1.0e3)
	return str(int(v))

func title() -> String:
	if kind == "chart":
		return "chart"
	if kind == "VOLUME":
		return "volume"
	if kind == "MACD":
		return "MACD %d/%d/%d" % [ind.fast, ind.slow, ind.sig]
	if kind == "RSI":
		return "RSI %d" % ind.period
	return kind

# collapse/expand button: right end of the header strip
func button_rect() -> Rect2:
	return Rect2(pane_rect.end.x - BTN_W - 4.0, pane_rect.position.y + 2.0, BTN_W, HEADER_H - 4.0)

func _draw() -> void:
	if view == null or data == null:
		return
	var r := pane_rect
	if r.size.x <= 0.0 or r.size.y <= 0.0:
		return
	# the chart pane is chrome only: a header strip over the top of the
	# graph area (the graph widgets draw underneath it); content panes
	# fill their whole rect with the pane background
	if kind == "chart":
		draw_rect(Rect2(r.position.x, r.position.y, r.size.x, HEADER_H), PANE_BG)
	else:
		draw_rect(r, PANE_BG)
	draw_line(Vector2(r.position.x, r.position.y), Vector2(r.end.x, r.position.y), SEP_COL)
	var font := ThemeDB.fallback_font
	draw_string(font, Vector2(r.position.x + 6.0, r.position.y + 13.0), title(),
		HORIZONTAL_ALIGNMENT_LEFT, r.size.x - BTN_W - 12.0, FONT_SIZE, LABEL_COL)
	var br := button_rect()
	draw_string(font, Vector2(br.position.x, br.position.y + 11.0), "-" if not collapsed else "+",
		HORIZONTAL_ALIGNMENT_CENTER, br.size.x, FONT_SIZE, LABEL_COL)
	if collapsed or kind == "chart":
		return
	if kind == "VOLUME":
		_draw_volume(r)
	elif kind == "MACD":
		_draw_macd(r)
	elif kind == "RSI":
		_draw_rsi(r)

# --- volume ---------------------------------------------------------------

func _draw_volume(r: Rect2) -> void:
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	var vmax := 0.0
	for s in data.series:
		if not s.visible or s.size() == 0:
			continue
		var last := mini(i1, s.size() - 1)
		for i in range(maxi(i0, 0), last + 1):
			vmax = maxf(vmax, s.candle_at(i).vol)
	if vmax <= 0.0:
		return
	var body := clampf(view.px_per_index * 0.7, 1.0, 30.0)
	var max_h := r.size.y - HEADER_H - TOP_PAD
	for s in data.series:
		if not s.visible or s.size() == 0:
			continue
		var last := mini(i1, s.size() - 1)
		if last < i0:
			continue
		for i in range(i0, last + 1):
			var c: Candle = s.candle_at(i)
			if c.vol <= 0:
				continue
			var x := view.index_to_x(i) + view.px_per_index / 2.0
			var h := maxf(c.vol / vmax * max_h, 1.0)
			var col: Color = s.col_pos if c.close >= c.open else s.col_neg
			draw_rect(Rect2(x - body / 2.0, r.end.y - h, body, h), col)
	draw_string(ThemeDB.fallback_font, Vector2(2.0, r.position.y + HEADER_H + 10.0), fmt_vol(vmax),
		HORIZONTAL_ALIGNMENT_RIGHT, ChartView.MARGIN_LEFT - 6.0, FONT_SIZE, LABEL_COL)

# --- MACD -----------------------------------------------------------------

# histogram (macd - signal) as bars around the zero line, macd line + signal
# line per series; symmetric scale on max |value| over the visible range
func _draw_macd(r: Rect2) -> void:
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	var body := clampf(view.px_per_index * 0.7, 1.0, 30.0)
	var zero_y := r.position.y + HEADER_H + (r.size.y - HEADER_H) / 2.0
	var amax := 0.0
	var per_series: Array = []
	for k in indicators.indicators.size():
		if indicators.indicators[k] != ind:
			continue
		for s in data.series:
			if not s.visible or s.size() == 0:
				continue
			var last := mini(i1, s.size() - 1)
			if last < i0:
				continue
			if not indicators._macd_ensure(ind, s, i0, last):
				continue
			var c: IndicatorWidget.IndCache = indicators._cache_for(ind, s)
			var hi := mini(i1, s.size() - 1)
			for i in range(i0, hi + 1):
				if i < c.vals_off or i - c.vals_off >= c.vals.size():
					continue
				amax = maxf(amax, absf(c.vals[i - c.vals_off]))
				amax = maxf(amax, absf(c.vals2[i - c.vals_off]))
			per_series.append([s, k, c])
	if amax <= 0.0 or per_series.is_empty():
		draw_line(Vector2(r.position.x, zero_y), Vector2(r.end.x, zero_y), Color(1, 1, 1, 0.15), 1.0)
		return
	var half := (r.size.y - HEADER_H) / 2.0 - 2.0
	draw_line(Vector2(r.position.x, zero_y), Vector2(r.end.x, zero_y), Color(1, 1, 1, 0.15), 1.0)
	for e in per_series:
		var s: PlotData.Series = e[0]
		var k: int = e[1]
		var c: IndicatorWidget.IndCache = e[2]
		var col := indicators.color_for(s, k)
		var last := mini(i1, s.size() - 1)
		var body_x := PackedVector2Array()
		var sig_pts := PackedVector2Array()
		for i in range(i0, last + 1):
			if i < c.vals_off or i - c.vals_off >= c.vals.size():
				continue
			var m: float = c.vals[i - c.vals_off]
			var sg: float = c.vals2[i - c.vals_off]
			if is_nan(m) or is_nan(sg):
				continue
			var x := view.index_to_x(i) + view.px_per_index / 2.0
			var hy := zero_y - (m - sg) / amax * half
			var hcol := Color(col, 0.35)
			draw_rect(Rect2(x - body / 2.0, mini(zero_y, hy), body, maxf(absf(hy - zero_y), 1.0)), hcol)
			body_x.append(Vector2(x, zero_y - m / amax * half))
			sig_pts.append(Vector2(x, zero_y - sg / amax * half))
		if body_x.size() >= 2:
			draw_polyline(body_x, col, 1.5)
		if sig_pts.size() >= 2:
			draw_polyline(sig_pts, col.lightened(0.5), 1.2)

# --- RSI ------------------------------------------------------------------

# 0..100 scale with 30/70 reference lines, one line per series
func _draw_rsi(r: Rect2) -> void:
	var ir := view.visible_index_range()
	var i0 := maxi(0, int(floor(ir.x)))
	var i1 := int(ceil(ir.y))
	var font := ThemeDB.fallback_font
	var dim := Color(1, 1, 1, 0.25)
	var body := r.size.y - HEADER_H
	for lvl: float in [70.0, 30.0]:
		var y := r.end.y - lvl / 100.0 * body
		var from := Vector2(r.position.x, y)
		var to := Vector2(r.end.x, y)
		var t := 0.0
		while t < 1.0:
			draw_line(from.lerp(to, t), from.lerp(to, mini(t + 0.04, 1.0)), dim, 1.0)
			t += 0.08
		draw_string(font, Vector2(2.0, y + 4.0), str(int(lvl)),
			HORIZONTAL_ALIGNMENT_RIGHT, ChartView.MARGIN_LEFT - 6.0, FONT_SIZE, LABEL_COL)
	for k in indicators.indicators.size():
		if indicators.indicators[k] != ind:
			continue
		for s in data.series:
			if not s.visible or s.size() == 0:
				continue
			var last := mini(i1, s.size() - 1)
			if last < i0:
				continue
			if not indicators._rsi_ensure(ind, s, i0, last):
				continue
			var c: IndicatorWidget.IndCache = indicators._cache_for(ind, s)
			var col := indicators.color_for(s, k)
			var pts := PackedVector2Array()
			for i in range(i0, last + 1):
				if i < c.vals_off or i - c.vals_off >= c.vals.size():
					continue
				var v: float = c.vals[i - c.vals_off]
				if is_nan(v):
					continue
				pts.append(Vector2(view.index_to_x(i) + view.px_per_index / 2.0,
					r.end.y - clampf(v, 0.0, 100.0) / 100.0 * body))
			if pts.size() >= 2:
				draw_polyline(pts, col, 1.5)
