extends SceneTree

const TICK_SIZE := 0.0025

var failures := 0
var ticks: Array = []

func check(cond: bool, name: String) -> void:
	if cond:
		print("PASS: ", name)
	else:
		failures += 1
		printerr("FAIL: ", name)

func new_book() -> OrderBookEngine:
	var ob := OrderBookEngine.new()
	ob.traded.connect(func(price, vol): ticks.append([price, vol]))
	return ob

func test_no_cross_no_trade() -> void:
	ticks.clear()
	var ob := new_book()
	ob.place_order(1.10, 100)
	ob.place_order(1.20, -100)
	ob.process_orders()
	check(ticks.is_empty(), "no trade when bid < ask")
	check(ob.bids == {1.10: 100} and ob.asks == {1.20: 100}, "book untouched")

func test_full_match() -> void:
	ticks.clear()
	var ob := new_book()
	ob.place_order(1.20, 100)
	ob.place_order(1.20, -100)
	ob.process_orders()
	check(ticks == [[1.20, 100]], "full match trades at ask price")
	check(ob.bids.is_empty() and ob.asks.is_empty(), "full match clears both sides")

func test_partial_bid_consumed() -> void:
	ticks.clear()
	var ob := new_book()
	ob.place_order(1.30, 50)
	ob.place_order(1.25, -100)
	ob.process_orders()
	check(ticks == [[1.25, 50]], "partial fill trades min volume")
	check(ob.bids.is_empty() and ob.asks == {1.25: 50}, "ask remainder stays on book")

func test_multi_level_sweep() -> void:
	ticks.clear()
	var ob := new_book()
	ob.place_order(1.30, 10)
	ob.place_order(1.20, 10)
	ob.place_order(1.15, -15)
	ob.process_orders()
	check(ticks == [[1.15, 10], [1.15, 5]], "sweep consumes both bid levels")
	check(ob.bids == {1.20: 5} and ob.asks.is_empty(), "bid remainder stays on book")

func test_sell_order_side() -> void:
	ticks.clear()
	var ob := new_book()
	ob.place_order(1.20, -30)
	check(ob.asks == {1.20: 30} and ob.bids.is_empty(), "negative vol lands in asks")
	ob.place_order(1.20, -10)
	check(ob.asks == {1.20: 40}, "sell adds to existing ask level")

func test_zero_vol_ignored() -> void:
	var ob := new_book()
	ob.place_order(1.20, 0)
	check(ob.bids.is_empty() and ob.asks.is_empty(), "zero vol is a no-op")

func test_random_sim_invariants() -> void:
	ticks.clear()
	var ob := new_book()
	ob.place_order(1.10, 100)
	ob.place_order(1.20, -100)
	ob.process_orders()
	var last_price := 1.15
	if not ticks.is_empty():
		last_price = ticks[-1][0]
	for n in 1000:
		for x in 20:
			_buy_or_sell(ob, last_price)
		ob.process_orders()
		if not ticks.is_empty():
			last_price = ticks[-1][0]
	var vols_ok := true
	for v in ob.bids.values() + ob.asks.values():
		vols_ok = vols_ok and v > 0
	check(vols_ok, "no non-positive volumes after sim")
	var spread_ok: bool = ob.bids.is_empty() or ob.asks.is_empty() or ob.best_bid() < ob.best_ask()
	check(spread_ok, "no crossed book after sim")
	check(ticks.size() > 0, "sim produced trades")

func _buy_or_sell(ob: OrderBookEngine, last_price: float) -> bool:
	var bs := randf()
	var vol := randi_range(1, 20)
	var pdelta := TICK_SIZE * float(randi_range(0, 10))
	if bs > 0.75:
		ob.place_order(last_price - pdelta, -vol)
		return true
	if bs <= 0.25:
		ob.place_order(last_price + pdelta, vol)
		return true
	return false

func _init() -> void:
	test_no_cross_no_trade()
	test_full_match()
	test_partial_bid_consumed()
	test_multi_level_sweep()
	test_sell_order_side()
	test_zero_vol_ignored()
	test_random_sim_invariants()
	if failures == 0:
		print("ALL TESTS PASSED")
	else:
		printerr(failures, " TEST(S) FAILED")
	quit(failures)
