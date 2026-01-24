extends Control

func set_distance(dist: float) -> void:
	$VBoxContainer/HBoxContainer/distance.text = "%3.3f" % dist
