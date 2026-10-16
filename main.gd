class_name MainTabs
extends TabContainer

@onready var chart: Control = $Chart

var ml_page: MlPage

func _ready() -> void:
	# node name has a space — $MLTrainer would not resolve
	ml_page = get_node("ML Trainer") as MlPage
	ml_page.equity_overlay.connect(_on_equity_overlay)

# route the ML page's equity overlay to the chart tab and bring it up
func _on_equity_overlay(tl_name: String, path: String) -> void:
	current_tab = 0
	chart._on_equity_overlay(tl_name, path)
