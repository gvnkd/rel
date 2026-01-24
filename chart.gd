extends Node2D

# price: volume
var bids = {
	102190: 200,
	#102180: 200,
	#102170: 200,
	#102160: 200,
	#102150: 200,
}

var asks = {
	102195: 200,
	#102210: 200,
	#102220: 200,
	#102230: 200,
	#102240: 200,
}
var precision = 5
var mult = 10 ** precision

var ob_scale: float = 100.0
var ob_x: int = 140
var ob_y: int = 250
var ob_w: int = 400
var ob_h: int = 500
var ob_depth: int = 50
var ob_spacing: int = 8

var t: float = 0.0
var time: float = 0.0

var candle = [102195, 102295, 102095, 102155]
var candles = []

var paused = true

func _input(event: InputEvent) -> void:
	if event.is_action_pressed("pause"):
		paused = !paused

func get_bids():
	var bs = bids.keys()
	bs.sort()
	bs.reverse()
	#print("bids: ", bs)
	return bs

func get_asks():
	var a = asks.keys()
	a.sort()
	#print("asks: ", a)
	return a

func draw_candle():
	var scale = 0.005
	var cx = 600
	var w = 20
	var sw = 4
	var cy = 600
	var ndx = 0
	var x_spacing = 22
	for can in candles:
		var o = float(can[0]) * scale
		var h = float(can[1]) * scale
		var l = float(can[2]) * scale
		var c = float(can[3]) * scale
		var body = o - c
		var body_col = Color.BURLYWOOD
		
		#$OrderBook.set_ohlc(o, h, l, c)
		#print(o, " ", h, " ", l, " ", c)
		draw_rect(Rect2(cx+x_spacing*ndx,cy-o,w,abs(body)), body_col)
		draw_rect(Rect2(cx+w/2-sw/2+x_spacing*ndx,cy-o-h,sw,h-l), Color.TOMATO)
		ndx += 1

func _draw():
	var bks = get_bids()
	var ask = get_asks()
	# draw ob center line
	var obc_x = ob_x + ob_w/2
	var obc_y = ob_y + ob_h/2
	var p0 = Vector2(ob_x, obc_y)
	var p1 = Vector2(ob_x+ob_w, obc_y)
	draw_line(p0,p1,Color.GRAY,1.0)

	var ndx: int = 1	
	var max_vol: int = 1
	var vol_scale: float = 1.0
	# calc vol_scale
	var bid_text = ""
	for k in bks.slice(0, ob_depth, 1):
		var v = bids[k]
		bid_text += "%0.5f: %d\n" % [float(k)/mult, bids[k]]
		if v > max_vol:
			max_vol = v
			vol_scale = float(ob_w) / float(max_vol)

	#bks_slice.reverse()
	#for k in bks_slice:

	$OrderBook.set_bid_label(bid_text)
	
	var ask_text = ""
	var ask_slice = ask.slice(0, ob_depth, 1)
	for k in ask_slice:
		var v = asks[k]
		if v > max_vol:
			max_vol = v
			vol_scale = float(ob_w) / float(max_vol)

	ask_slice.reverse()
	for k in ask_slice:
		ask_text += "%0.5f : %d\n" % [float(k)/mult, asks[k]]

	$OrderBook.set_ask_label(ask_text)

	# draw bids
	for k in bks.slice(0, ob_depth, 1):
		var v = bids[k]
		p0 = Vector2(ob_x, obc_y + ob_spacing * ndx)
		p1 = Vector2(ob_x + v*vol_scale, obc_y + ob_spacing * ndx)
		draw_line(p0, p1, Color.GREEN_YELLOW, 3.0)
		ndx += 1

	# draw asck
	ndx = 1
	for k in ask.slice(0, ob_depth, 1):
		var v = asks[k]
		p0 = Vector2(ob_x, obc_y - ob_spacing * ndx)
		p1 = Vector2(ob_x + v*vol_scale, obc_y - ob_spacing * ndx)
		draw_line(p0, p1, Color.LIGHT_BLUE, 3.0)
		ndx += 1

	draw_candle()

func _process(delta: float) -> void:
	if paused:
		return

	time += delta
	t += delta
	process_orders()
	
	if candles.is_empty():
		candles.append(candle)
	
	if time > 60.0:
		var c = [0, 0, 0, 0]
		c = candle.duplicate(true)
		var open = c[3]
		#c[0] = candle[0]
		#c[1] = candle[1]
		#c[2] = candle[2]
		#c[3] = candle[3]
		c[0] = float(c[0]) / 100000
		c[1] = float(c[1]) / 100000
		c[2] = float(c[2]) / 100000
		c[3] = float(c[3]) / 100000
		$Plot/Data.candles.append(c)
		$Plot.queue_redraw()
		#candles.append(c)
		candle[0] = open
		candle[1] = open
		candle[2] = open
		time = 0.0
		
	if (t > 0.05):
		for x in range(1,5):
			var bid = 0
			var ask = 0
			if !bids.is_empty():
				var bid_prices = get_bids()
				bid = bid_prices[0]
			if !asks.is_empty():
				var ask_prices = get_asks()
				ask = ask_prices[0]

			if buy_or_sell(bid, ask):
				queue_redraw()
				t = 0
	
		#var p = randi_range(11 * mult, 13 * mult)
		#var v = randi_range(20, 500)
		#if p in asks.keys():
			#asks[p] += v
		#else:
			#asks[p] = v
#
		#p = randi_range(11 * mult, 13 * mult)
		#v = randi_range(20, 500)
		#if p in bids.keys():
			#bids[p] += v
		#else:
			#bids[p] = v
#
		#t = 0.0

# accepts current bid and ask prices
# and returns a volume of order
# when volume is negative value then place a sell order
# when volume is positive value then place a buy order
# when volume is zero then no order should be placed
func buy_or_sell(bid: int, ask: int) -> bool:
	var bs = randf()
	var vol = randi_range(1, 20)
	var pdelta = randi_range(50, 500)
	if bs > 0.75: # sell
		if bid == 0:
			place_order(ask - pdelta, -vol)
			return true
		place_order(bid - pdelta, -vol)
		return true
	if bs < 0.25: # buy
		if ask == 0:
			place_order(bid + pdelta, vol)
			return true
		place_order(ask + pdelta, vol)
		return true
	
	return false

func place_order(price: int, vol: int) -> void:
	if vol > 0: # buy order
		if price in bids.keys():
			bids[price] += vol
		else:
			bids[price] = vol
	if vol < 0: # sell order
		if price in asks.keys():
			asks[price] += -vol
		else:
			asks[price] = -vol

func update_candle(p: int):
	var o = candle[0]
	var h = candle[1]
	var l = candle[2]
	var c = candle[3]
	if o == 0:
		candle[0] = p

	if l > p or l == 0:
		#print("Updating Low: ", p)
		candle[2] = p
	if h < p or h == 0:
		candle[1] = p
		#print("Updating High: ", p)

	candle[3] = p

func process_orders():
	var bid_prices = get_bids()
	var ask_prices = get_asks()

	if bids.is_empty() or asks.is_empty():
		return

	#process bid orders
	var bids_del = []
	var asks_del = []

	var b = bid_prices[0]	
	var a = ask_prices[0]
	if b >= a:
		update_candle(b)
		var b_vol = bids[b]
		var a_vol = asks[a]
		if b_vol == a_vol:
			bids_del.append(b)
			asks_del.append(a)
		if b_vol > a_vol:
			asks_del.append(a)
			bids[b] = b_vol - a_vol
		else:
			bids_del.append(b)
			asks[a] = a_vol - b_vol

	#for p_bid in bid_prices:
		#var bid_vol = bids[p_bid]
		#for p_ask in ask_prices:
			#if p_bid >= p_ask:
				#var ask_vol = asks[p_ask]
				#if ask_vol > bid_vol:
					#asks[p_ask] -= bid_vol
					#bids[p_bid] = 0
					#bids_del.append(p_bid)
					## order complete
				#else:
					#bids[p_bid] -= ask_vol
					#asks[p_ask] = 0
					#asks_del.append(p_ask)
					#bid_vol -= ask_vol
					#if bid_vol <= 0:
						#bids_del.append(p_bid)
					## reduce bid vol and check next ask order
#
				#update_candle(p_bid)
			#else:
				#break

	for p in bids_del:
		bids.erase(p)

	for p in asks_del:
		asks.erase(p)
