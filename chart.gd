extends Node2D

const PRECISION := 5
const MULT := 10 ** PRECISION
const PRICE_FMT := "%0.5f : %d\n"
const OB_DEPTH := 50
const CANDLE_PERIOD_S := 60.0
const ORDER_INTERVAL_S := 0.05
const ORDERS_PER_TICK := 4

const PRICE_SCALE := 0.005
const CANDLE_CX := 600.0
const CANDLE_CY := 600.0
const CANDLE_W := 20.0
const SHADOW_W := 4.0
const CANDLE_SPACING := 22.0

var ob := OrderBookEngine.new()
var current := Candle.new()
var completed: Array[Candle] = []

var ob_x: int = 140
var ob_y: int = 250
var ob_w: int = 400
var ob_h: int = 500
var ob_spacing: int = 8

var paused := true
var time := 0.0
var t := 0.0

@onready var order_book_ui: OrderBookPanel = $OrderBook
@onready var plot_data: Node2D = $Plot/Data

func _ready() -> void:
	ob.traded.connect(_on_traded)
	plot_data.candles = completed
	ob.place_order(102190, 200)
	ob.place_order(102195, -200)
	_update_labels()

func _input(event: InputEvent) -> void:
	if event.is_action_pressed("pause"):
		paused = not paused

func _process(delta: float) -> void:
	if paused:
		return
	time += delta
	t += delta

	if time >= CANDLE_PERIOD_S:
		_roll_candle()

	if t >= ORDER_INTERVAL_S:
		t = 0.0
		var changed := false
		for i in ORDERS_PER_TICK:
			changed = _buy_or_sell() or changed
		if changed:
			ob.process_orders()
			_update_labels()
			queue_redraw()

func _draw() -> void:
	var bid_ps := ob.bid_prices()
	var ask_ps := ob.ask_prices()

	var max_vol := 1
	for p in bid_ps:
		max_vol = maxi(max_vol, ob.bids[p])
	for p in ask_ps:
		max_vol = maxi(max_vol, ob.asks[p])
	var vol_scale := float(ob_w) / float(max_vol)

	var center := Vector2(ob_x, ob_y + ob_h / 2)
	draw_line(Vector2(ob_x, center.y), Vector2(ob_x + ob_w, center.y), Color.GRAY)

	for i in mini(bid_ps.size(), OB_DEPTH):
		var p: Variant = bid_ps[i]
		var y := center.y + ob_spacing * (i + 1)
		draw_line(Vector2(ob_x, y), Vector2(ob_x + ob.bids[p] * vol_scale, y), Color.GREEN_YELLOW, 3.0)

	for i in mini(ask_ps.size(), OB_DEPTH):
		var p: Variant = ask_ps[i]
		var y := center.y - ob_spacing * (i + 1)
		draw_line(Vector2(ob_x, y), Vector2(ob_x + ob.asks[p] * vol_scale, y), Color.LIGHT_BLUE, 3.0)

	_draw_candles()

func _draw_candles() -> void:
	for i in completed.size():
		var c := completed[i]
		var x := CANDLE_CX + CANDLE_SPACING * i
		var o := c.open * PRICE_SCALE
		var h := c.high * PRICE_SCALE
		var l := c.low * PRICE_SCALE
		var cl := c.close * PRICE_SCALE
		draw_rect(Rect2(x, CANDLE_CY - maxf(o, cl), CANDLE_W, absf(o - cl)), Color.BURLYWOOD)
		draw_rect(Rect2(x + CANDLE_W / 2 - SHADOW_W / 2, CANDLE_CY - h, SHADOW_W, h - l), Color.TOMATO)

func _update_labels() -> void:
	var bid_text := ""
	for p in ob.bid_prices().slice(0, OB_DEPTH):
		bid_text += PRICE_FMT % [float(p) / MULT, ob.bids[p]]
	order_book_ui.set_bid_label(bid_text)

	var ask_text := ""
	var ask_slice := ob.ask_prices().slice(0, OB_DEPTH)
	ask_slice.reverse()
	for p in ask_slice:
		ask_text += PRICE_FMT % [float(p) / MULT, ob.asks[p]]
	order_book_ui.set_ask_label(ask_text)

	order_book_ui.set_ohlc(current.open, current.high, current.low, current.close)

func _roll_candle() -> void:
	if current.is_empty():
		time = 0.0
		return
	completed.append(current)
	var prev := current
	current = Candle.new()
	current.open = prev.close
	current.high = prev.close
	current.low = prev.close
	plot_data.queue_redraw()
	time = 0.0

func _on_traded(price: Variant, _vol: int) -> void:
	current.update(float(price) / MULT)

func _buy_or_sell() -> bool:
	var bs := randf()
	var vol := randi_range(1, 20)
	var pdelta := randi_range(0, 10)
	var bid: Variant = ob.best_bid()
	var ask: Variant = ob.best_ask()
	if bs > 0.75:
		ob.place_order(bid - pdelta if bid != null else ask - pdelta, -vol)
		return true
	if bs < 0.25:
		ob.place_order(ask + pdelta if ask != null else bid + pdelta, vol)
		return true
	return false
