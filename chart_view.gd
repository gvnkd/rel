class_name ChartView
extends RefCounted

const MARGIN_LEFT := 84.0
const MARGIN_BOTTOM := 26.0

var book_width := 180.0

var plot_rect := Rect2()

var px_per_index := 22.0
var px_per_price := 100000.0
var base_index := 0.0
var base_price := 0.0

func layout(size: Vector2) -> void:
	plot_rect = Rect2(MARGIN_LEFT, 0.0, maxf(size.x - MARGIN_LEFT - book_width, 0.0), maxf(size.y - MARGIN_BOTTOM, 0.0))

func index_to_x(i: float) -> float:
	return plot_rect.position.x + (i - base_index) * px_per_index

func x_to_index(x: float) -> float:
	return base_index + (x - plot_rect.position.x) / px_per_index

func price_to_y(p: float) -> float:
	return plot_rect.position.y + (base_price - p) * px_per_price

func y_to_price(y: float) -> float:
	return base_price - (y - plot_rect.position.y) / px_per_price

func visible_index_range() -> Vector2:
	return Vector2(x_to_index(plot_rect.position.x), x_to_index(plot_rect.end.x))

func visible_price_range() -> Vector2:
	return Vector2(y_to_price(plot_rect.end.y), base_price)

func zoom_at(pos: Vector2, factor: float) -> void:
	var i := x_to_index(pos.x)
	var p := y_to_price(pos.y)
	px_per_index = clampf(px_per_index * factor, 1.0, 400.0)
	px_per_price = clampf(px_per_price * factor, 1.0e-6, 1.0e8)
	base_index = i - (pos.x - plot_rect.position.x) / px_per_index
	base_price = p + (pos.y - plot_rect.position.y) / px_per_price

func zoom_x_at(pos: Vector2, factor: float) -> void:
	var i := x_to_index(pos.x)
	px_per_index = clampf(px_per_index * factor, 1.0, 400.0)
	base_index = i - (pos.x - plot_rect.position.x) / px_per_index

func scale_by(delta: Vector2, pos: Vector2) -> void:
	var i := x_to_index(pos.x)
	var p := y_to_price(pos.y)
	px_per_index = clampf(px_per_index * pow(1.01, delta.x), 1.0, 400.0)
	px_per_price = clampf(px_per_price * pow(1.01, -delta.y), 1.0e-6, 1.0e8)
	base_index = i - (pos.x - plot_rect.position.x) / px_per_index
	base_price = p + (pos.y - plot_rect.position.y) / px_per_price

func pan_by(delta: Vector2) -> void:
	base_index -= delta.x / px_per_index
	base_price += delta.y / px_per_price

func fit_prices(lo: float, hi: float) -> void:
	var span := maxf(hi - lo, 1.0e-9)
	px_per_price = plot_rect.size.y * 0.9 / span
	base_price = hi + span * 0.05

static func nice_step(raw: float) -> float:
	if raw <= 0.0:
		return 1.0
	var mag := pow(10.0, floor(log(raw) / log(10.0)))
	var n := raw / mag
	if n <= 1.0:
		return mag
	if n <= 2.0:
		return 2.0 * mag
	if n <= 2.5:
		return 2.5 * mag
	if n <= 5.0:
		return 5.0 * mag
	return 10.0 * mag

static func fmt_price(p: float) -> String:
	if absf(p) >= 1000.0:
		return "%.0f" % p
	if absf(p) >= 1.0:
		return "%.5f" % p
	return "%.8f" % p
