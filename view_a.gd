extends Sprite2D

var t = 0.0

var radius: float = 1.0
var scale_factor: float = 100.0
var pointsA: Array[Vector3] = []
var pointsB: Array[Vector3] = []
var distX = 256.0
var linkAB = [0,0]

func _ready() -> void:
	for a in Vector3(0.0, TAU, TAU/5):
		var zA = radius * sin(a)
		var yA = radius * cos(a)
		var xA = 0.0
		var zB = radius * sin(a)
		var yB = radius * cos(a)
		var xB = 0.0
		pointsA.push_back(Vector3(xA,yA,zA).normalized())
		pointsB.push_back(Vector3(xB,yB,zB).normalized())

func _draw() -> void:
	var x0 = 256.0;
	var y0 = 256.0;
	for p in pointsA:
		var x = p.x * scale_factor
		var y = p.y * scale_factor
		draw_line(Vector2(x0, y0), Vector2(x0 + x, y0 + y), Color.ROSY_BROWN)

	for p in pointsB:
		var x = p.x * scale_factor
		var y = p.y * scale_factor
		draw_line(Vector2(x0+distX, y0), Vector2(x0 + x + distX, y0 + y), Color.BURLYWOOD)

	var pA = pointsA[linkAB[0]] * scale_factor
	var pB = pointsB[linkAB[1]] * scale_factor
	var from = Vector2(x0 + pA.x, y0 + pA.y)
	var to = Vector2(x0 + pB.x + distX, y0 + pB.y)
	draw_line(from, to, Color.TOMATO)
	#print(pA.distance_to(pB))
	#for a in Vector3(0.0, TAU, TAU/5):
		#var aa = a + t
		#var x = r * sin(a)
		#var y = r * cos(a)
		#var xx = x * cos(aa) - y * sin(aa)
		#var yy = x * sin(aa) + y * cos(aa)
		#draw_line(Vector2(x0, y0), Vector2(x0 + x, y0 + y), Color.GREEN, 5.0, true)
		#draw_line(Vector2(x0, y0), Vector2(x0 + xx, y0 + yy), Color.RED, 5.0, true)

func _process(delta: float) -> void:
	var d = delta*1
	t += delta
	var i: int = 0
	while i < pointsA.size():
		var pA = pointsA[i]
		var vA = pA.rotated(Vector3(1,0,0), d)
		var pB = pointsB[i]
		var vB = pB.rotated(Vector3(-1,0,0), d)
		pointsA[i] = vA
		pointsB[i] = vB
		i += 1

	if t > 2:
		var iA = linkAB[0]
		var iB = linkAB[1]
		iA = (iA + 1) % 5
		iB = (iB + 1) % 5
		linkAB[0] = iA
		linkAB[1] = iB
		t = 0
	var hud = get_node("../HUD")
	var dist = pointsA[linkAB[0]].distance_to(pointsB[linkAB[1]])
	hud.set_distance(dist)
	queue_redraw()
