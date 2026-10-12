class_name OrderBookPanel
extends Control

func set_bid_label(text: String) -> void:
	$HBoxContainer/VBoxContainer/BidLabel.text = text

func set_ask_label(text: String) -> void:
	$HBoxContainer/VBoxContainer/AskLabel.text = text

func set_ohlc(o: float, h: float, l: float, c: float) -> void:
	$HBoxContainer/VBoxContainer2/Open.text = "%0.5f" % o
	$HBoxContainer/VBoxContainer2/High.text = "%0.5f" % h
	$HBoxContainer/VBoxContainer2/Low.text = "%0.5f" % l
	$HBoxContainer/VBoxContainer2/Close.text = "%0.5f" % c
