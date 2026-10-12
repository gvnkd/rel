class_name OrderBookWidget
extends Control

const ROW_H := 18.0
const DEPTH := 14
const BASE_VOL := 200.0
const DECAY := 0.87
const COL_BID := Color(0.25, 0.8, 0.4)
const COL_ASK := Color(0.9, 0.32, 0.32)
const COL_BAR_BID := Color(0.25, 0.8, 0.4, 0.3)
const COL_BAR_ASK := Color(0.9, 0.32, 0.32, 0.3)
const COL_TEXT := Color(0.8, 0.83, 0.87)

var view: ChartView
var data: PlotData
var bids: Array[Vector2] = []
var asks: Array[Vector2] = []
var mid := 0.0

func _ready() -> void:
	mouse_filter = MOUSE_FILTER_IGNORE

func clear() -> void:
	bids.clear()
	asks.clear()
	queue_redraw()

func set_ladder(mid_price: float, step: float) -> void:
	var b: Array[Vector2] = []
	var a: Array[Vector2] = []
	for k in range(1, DEPTH + 1):
		var v := BASE_VOL * pow(DECAY, k - 1)
		b.append(Vector2(mid_price - step * k, v))
		a.append(Vector2(mid_price + step * k, v))
	set_book(b, a, mid_price)

func set_book(new_bids: Array[Vector2], new_asks: Array[Vector2], mid_price: float) -> void:
	mid = mid_price
	bids = new_bids.slice(0, DEPTH)
	asks = new_asks.slice(0, DEPTH)
	queue_redraw()

func _draw() -> void:
	if view == null:
		return
	var font := ThemeDB.fallback_font
	var r := Rect2(view.plot_rect.end.x, view.plot_rect.position.y, view.book_width, view.plot_rect.size.y)
	if bids.is_empty():
		draw_string(font, r.position + Vector2(8.0, 20.0), "no book", HORIZONTAL_ALIGNMENT_LEFT, r.size.x - 16.0, 12, Color(0.6, 0.62, 0.65))
		return
	var x := r.position.x + 4.0
	var w := r.size.x - 8.0
	var y := r.position.y + 8.0
	draw_string(font, Vector2(x, y + 10.0), "order book", HORIZONTAL_ALIGNMENT_LEFT, w, 12, Color(0.6, 0.62, 0.65))
	y += ROW_H + 4.0
	var max_vol := 1.0
	for lvl in bids + asks:
		max_vol = maxf(max_vol, lvl.y)
	var bar_x := x + 72.0
	var bar_w := w - 72.0
	for lvl in asks:
		_row(font, lvl, bar_x, bar_w, y, max_vol, COL_ASK, COL_BAR_ASK)
		y += ROW_H
	_mid_row(font, x, w, y)
	y += ROW_H
	for idx in range(bids.size() - 1, -1, -1):
		_row(font, bids[idx], bar_x, bar_w, y, max_vol, COL_BID, COL_BAR_BID)
		y += ROW_H

func _row(font: Font, lvl: Vector2, bar_x: float, bar_w: float, y: float, max_vol: float, col: Color, bar_col: Color) -> void:
	var bw := bar_w * lvl.y / max_vol
	draw_rect(Rect2(bar_x + bar_w - bw, y + 2.0, bw, ROW_H - 5.0), bar_col)
	draw_string(font, Vector2(bar_x - 70.0, y + 12.0), ChartView.fmt_price(lvl.x), HORIZONTAL_ALIGNMENT_LEFT, 68.0, 12, col)
	draw_string(font, Vector2(bar_x, y + 12.0), str(int(lvl.y)), HORIZONTAL_ALIGNMENT_RIGHT, bar_w - 2.0, 12, COL_TEXT)

func _mid_row(font: Font, x: float, w: float, y: float) -> void:
	draw_rect(Rect2(x, y + 1.0, w, ROW_H - 3.0), Color(0.9, 0.9, 0.35, 0.15))
	draw_string(font, Vector2(x, y + 12.0), ChartView.fmt_price(mid), HORIZONTAL_ALIGNMENT_LEFT, 70.0, 12, Color(0.9, 0.9, 0.35))
