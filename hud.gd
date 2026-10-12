extends Control

@onready var distance_label: Label = $VBoxContainer/HBoxContainer/distance

func set_distance(dist: float) -> void:
	distance_label.text = "%3.3f" % dist
