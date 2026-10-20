class_name MainTabs
extends VBoxContainer

func _ready() -> void:
	var bar: HBoxContainer = $MenuBar
	var chart = $Tabs/Chart

	var b_sim := _tool_button(bar, "⚙", "show/hide simulation panel (F1)")
	b_sim.toggle_mode = true
	b_sim.button_pressed = chart.sim_window.visible
	b_sim.toggled.connect(func(on: bool) -> void: chart.sim_window.visible = on)
	chart.sim_window.visibility_changed.connect(
		func() -> void: b_sim.set_pressed_no_signal(chart.sim_window.visible))

	var b_series := _tool_button(bar, "☰", "show/hide series list")
	b_series.toggle_mode = true
	b_series.button_pressed = chart.series_window.visible
	b_series.toggled.connect(func(on: bool) -> void: chart.series_window.visible = on)
	chart.series_window.visibility_changed.connect(
		func() -> void: b_series.set_pressed_no_signal(chart.series_window.visible))

	var b_load := _tool_button(bar, "⇩", "load candles from a CSV or a binary .rbin export")
	b_load.pressed.connect(chart.series_panel.open_load_dialog)

	var spacer := Control.new()
	spacer.size_flags_horizontal = SIZE_EXPAND_FILL
	bar.add_child(spacer)

	var b_exit := _tool_button(bar, "⏻", "exit")
	b_exit.pressed.connect(func() -> void: get_tree().quit())

func _tool_button(bar: Container, text: String, tip: String) -> Button:
	var b := Button.new()
	b.text = text
	b.tooltip_text = tip
	b.focus_mode = Control.FOCUS_NONE
	bar.add_child(b)
	return b
