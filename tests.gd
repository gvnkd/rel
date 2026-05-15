extends SceneTree

var tick_size: float = 0.0025
var min_spread: float = 0.50

var timestamp: int = 0
var time_delta: int = 10

var ticks: Array[Array] = [[-1, 1.15, 1]]


var order_book = {
	"bids": {
		1.10: 100,
		#1.25: 100,
		#1.50: 100,
	},
	"asks": {
		1.20: 100,
		#1.25: 20,
		#1.50: 30,
		#1.75: 40,
		#2.00: 50,
	},
}

func get_bids() -> Array:
	var bid_prices = order_book.bids.keys()
	bid_prices.sort()
	bid_prices.reverse()
	return bid_prices

func get_asks() -> Array:
	var ask_prices = order_book.asks.keys()
	ask_prices.sort()
	return ask_prices
	
func process_orders() -> void:
	var _bids = get_bids()
	var _asks = get_asks()
	print_debug("BIDs: ", _bids)
	print_debug("ASKs: ", _asks)

	if _bids.is_empty() or _asks.is_empty():
		print_debug("BIDs or ASKs prices is empty")
		return

	if _bids[0] < _asks[0]:
		print_debug("_bids[0] < _asks[0] No more matching orders")
		return

	var stop: bool = false
	var del_as: Array = []
	var del_bs: Array = []
	
	for bp in _bids:
		if stop:
			break

		var bv = order_book.bids[bp]
		if bv == 0:
			print_debug("BVol is zero. Skipping this order")
		elif bv < 0:
			print_debug("BVol is negative. This is WRONG")
			break

		print_debug("BID price: ", bp, " vol: ", bv)
		for ap in _asks:
			var av = order_book.asks[ap]
			if av == 0:
				print_debug("Skip empty volume for: ", ap)
				continue
			elif av <= 0:
				print_debug("Negative volume MUST not exists")
				return

			if bp < ap:
				print_debug("No more price for: ", bp)
				stop = true
				break

			if bv > av:
				print_debug("Buy ", av, " volume by ", ap)
				add_tick(float(ap), int(av))
				bv -= av
				order_book.asks[ap] -= av
				del_as.append(ap)
				order_book.bids[bp] = bv
			elif bv <= av:
				print_debug("Buy ", bv, " volume by ", ap)
				add_tick(float(ap), int(bv))
				order_book.asks[ap] -= bv
				order_book.bids[bp] -= av
				del_bs.append(bp)
				if bv == av:
					del_as.append(ap)

				break
				
			print_debug("ASK price: ", ap, " vol: ", av)

	for a in del_as:
		order_book.asks.erase(a)

	for b in del_bs:
		order_book.bids.erase(b)
		
	print("Order book: ", order_book)
	print("Ticks: ", ticks)

func add_tick(price: float, vol: int) -> void:
	print_debug(timestamp, " => New tick: ", price, " vol: ", vol)
	ticks.append([timestamp, price, vol])
	timestamp += 1

func place_order(price: float, vol: int) -> void:
	if vol > 0: # buy order
		if price in order_book.bids.keys():
			order_book.bids[price] += vol
		else:
			order_book.bids[price] = vol
	if vol < 0: # sell order
		if price in order_book.asks.keys():
			order_book.asks[price] += -vol
		else:
			order_book.asks[price] = -vol

func buy_or_sell(last_price: float) -> bool:
	var bs = randf() # buy or sell dice
	var vol = randi_range(1, 20)
	# how mach ticks from price
	var rand_ticks = randi_range(0, 10)
	var pdelta: float = tick_size * float(rand_ticks)
	if bs > 0.75: # sell
		var p: float = last_price - pdelta
		print_debug("Sell ", vol, " by ", p, " Last: ", last_price, " PDelta: ", pdelta)
		place_order(p, -vol)
		return true
	if bs <= 0.25: # buy
		var p: float = last_price + pdelta
		print_debug("Buy ", vol, " by ", p, " Last: ", last_price, " PDelta: ", pdelta)
		place_order(p, vol)
		return true
	
	return false

func _init() -> void:
	print("Ready")
	print(order_book)
	process_orders()
	var last_price = ticks[-1][1]
	print("========= Last price: ", last_price)
	#var get_new_order = false
	for n in range(0,1000):
		for x in range(0,20):
			buy_or_sell(last_price)
		process_orders()

	print(ticks)
	quit()
