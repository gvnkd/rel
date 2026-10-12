class_name OrderBookEngine
extends RefCounted

signal traded(price, vol)

var bids: Dictionary = {}
var asks: Dictionary = {}

func bid_prices() -> Array:
	var prices := bids.keys()
	prices.sort()
	prices.reverse()
	return prices

func ask_prices() -> Array:
	var prices := asks.keys()
	prices.sort()
	return prices

func best_bid() -> Variant:
	var prices := bid_prices()
	return prices[0] if not prices.is_empty() else null

func best_ask() -> Variant:
	var prices := ask_prices()
	return prices[0] if not prices.is_empty() else null

func place_order(price: Variant, vol: int) -> void:
	if vol == 0:
		return
	var side := bids if vol > 0 else asks
	side[price] = side.get(price, 0) + abs(vol)

func process_orders() -> void:
	var del_bids: Array = []
	var del_asks: Array = []
	var ask_ps := ask_prices()
	var ask_i := 0
	for bp in bid_prices():
		var bv: int = bids[bp]
		while ask_i < ask_ps.size() and bv > 0:
			var ap: Variant = ask_ps[ask_i]
			var av: int = asks.get(ap, 0)
			if av == 0:
				ask_i += 1
				continue
			if bp < ap:
				break
			var v: int = mini(bv, av)
			traded.emit(ap, v)
			bv -= v
			av -= v
			asks[ap] = av
			if av == 0:
				del_asks.append(ap)
				ask_i += 1
		if bv == 0:
			del_bids.append(bp)
		else:
			bids[bp] = bv
	for p in del_bids:
		bids.erase(p)
	for p in del_asks:
		asks.erase(p)
