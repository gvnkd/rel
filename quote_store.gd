class_name QuoteStore
extends RefCounted

# Binary candle store ("RELQ"): fixed-size records over a base 1-minute
# series, with lazily aggregated per-timeframe siblings. Real-quotes CSVs
# are converted once (cached under user://quotecache) and series read
# through a sliding window — the chart never materializes all candles.

const MAGIC := 0x514C4552  # "RELQ" little-endian
const VERSION := 1
const HEADER_SIZE := 64
const REC_SIZE := 48  # time u64 + o/h/l/c f64 + vol u64
const CHUNK := 65536  # records processed per awaited step

const CACHE_DIR := "user://quotecache"

# timeframe -> minutes per bucket; sim chunk counts reuse the same keys
const TFS: Array[String] = ["1m", "5m", "1h", "2h", "D", "W"]
const TF_MINUTES := { "1m": 1, "5m": 5, "1h": 60, "2h": 120, "D": 1440, "W": 10080 }

var path := ""
var count := 0
var tf_minutes := 1
var decimals := -1
var has_time := true
var min_low := 0.0
var max_high := 0.0

var _f: FileAccess = null

# ---- paths -------------------------------------------------------------

static func cache_path(csv_path: String, tf: String = "1m") -> String:
	var key := csv_path.md5_text()
	var suffix := "" if tf == "1m" else "." + tf
	return "%s/%s%s.rbin" % [CACHE_DIR, key, suffix]

static func sniff_csv(csv_path: String) -> String:
	# "quotes" = leading "YYYY.MM.DD HH:MM" timestamp column; anything else
	# is treated as the sim candles format (open,high,low,close,vol)
	var f := FileAccess.open(csv_path, FileAccess.READ)
	if f == null:
		return "error"
	var first := f.get_line().strip_edges()
	f.close()
	if first.is_empty():
		return "error"
	var head := first.get_slice(",", 0)
	if head.length() >= 16 and head[4] == "." and head[7] == "." and head[10] == " ":
		return "quotes"
	return "sim"

# parses "YYYY.MM.DD HH:MM" to unix minutes; -1 on garbage
static func parse_time_min(s: String) -> int:
	if s.length() < 16:
		return -1
	var y := s.substr(0, 4).to_int()
	var mo := s.substr(5, 2).to_int()
	var d := s.substr(8, 2).to_int()
	var h := s.substr(11, 2).to_int()
	var mi := s.substr(14, 2).to_int()
	if y < 1970 or mo < 1 or mo > 12 or d < 1 or d > 31:
		return -1
	# days since epoch via civil calendar (no time库 needed)
	var yy := y - (1 if mo <= 2 else 0)
	var era := floori((yy if yy >= 0 else yy - 399) / 400.0)
	var yoe := yy - era * 400
	var mp := (mo + 9) % 12
	var doy := floori((153 * mp + 2) / 5.0) + d - 1
	var doe := yoe * 365 + floori(yoe / 4.0) - floori(yoe / 100.0) + doy
	var days := era * 146097 + doe - 719468
	return days * 1440 + h * 60 + mi

# Monday-aligned week bucket for unix minutes (epoch is a Thursday):
# Mondays sit at t ≡ 4*1440 (mod 10080); `%` is truncated here, so shift
# into the non-negative range before reducing
static func week_bucket(t: int) -> int:
	var x := t - 4 * 1440
	x = x - (x % 10080 + 10080) % 10080
	return x + 4 * 1440

static func bucket_start(t: int, tf: String) -> int:
	if tf == "W":
		return week_bucket(t)
	return t - t % int(TF_MINUTES[tf])

# ---- header ------------------------------------------------------------

static func _write_header(f: FileAccess, p_tf_minutes: int, p_has_time: bool, p_decimals: int, p_count: int, p_lo: float, p_hi: float) -> void:
	var h := PackedByteArray()
	h.resize(HEADER_SIZE)
	h.encode_u32(0, MAGIC)
	h.encode_u32(4, VERSION)
	h.encode_u32(8, 1 if p_has_time else 0)
	h.encode_u32(12, p_tf_minutes)
	h.encode_u64(16, p_count)
	h.encode_double(24, p_lo)
	h.encode_double(32, p_hi)
	h.encode_s32(40, p_decimals)
	f.store_buffer(h)

func _read_header(f: FileAccess) -> bool:
	if f.get_length() < HEADER_SIZE:
		return false
	f.seek(0)
	var h := f.get_buffer(HEADER_SIZE)
	if h.decode_u32(0) != MAGIC or h.decode_u32(4) != VERSION:
		return false
	has_time = h.decode_u32(8) != 0
	tf_minutes = h.decode_u32(12)
	count = int(h.decode_u64(16))
	min_low = h.decode_double(24)
	max_high = h.decode_double(32)
	decimals = h.decode_s32(40)
	return true

# ---- open / window reads -----------------------------------------------

func open(p_path: String) -> bool:
	if _f != null:
		_f.close()
	_f = FileAccess.open(p_path, FileAccess.READ)
	if _f == null:
		return false
	if not _read_header(_f):
		_f.close()
		_f = null
		return false
	path = p_path
	return true

func close() -> void:
	if _f != null:
		_f.close()
		_f = null

func _rec_at(idx: int) -> Dictionary:
	_f.seek(HEADER_SIZE + idx * REC_SIZE)
	var b := _f.get_buffer(REC_SIZE)
	return {
		"t": b.decode_u64(0), "o": b.decode_double(8), "h": b.decode_double(16),
		"l": b.decode_double(24), "c": b.decode_double(32), "v": b.decode_u64(40),
	}

# fills `out` (Array[Candle]) with records [a, b); returns absolute offset
func read_range(a: int, b: int, out: Array[Candle]) -> int:
	out.clear()
	if _f == null or a >= count:
		return a
	b = mini(b, count)
	_f.seek(HEADER_SIZE + a * REC_SIZE)
	var want := (b - a) * REC_SIZE
	var buf := _f.get_buffer(want)
	if buf.size() < want:
		buf.resize(want)
	for off in range(0, want, REC_SIZE):
		var c := Candle.new()
		c.open = buf.decode_double(off + 8)
		c.high = buf.decode_double(off + 16)
		c.low = buf.decode_double(off + 24)
		c.close = buf.decode_double(off + 32)
		c.vol = int(buf.decode_u64(off + 40))
		out.append(c)
	return a

# ---- csv conversion (async, chunked) -----------------------------------

# converts a real-quotes csv to the base 1m rbin; skips when the cache is
# fresh (mtime newer than the csv). `progress` gets 0..1 callbacks.
func convert_csv_async(csv_path: String, progress: Callable = Callable()) -> Error:
	DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(CACHE_DIR))
	var out := cache_path(csv_path)
	if FileAccess.file_exists(out):
		var src := FileAccess.open(csv_path, FileAccess.READ)
		var fresh := FileAccess.get_modified_time(out) >= FileAccess.get_modified_time(csv_path)
		if src != null:
			src.close()
		if fresh:
			return OK
	var f := FileAccess.open(csv_path, FileAccess.READ)
	if f == null:
		return ERR_CANT_OPEN
	var w := FileAccess.open(out, FileAccess.WRITE)
	if w == null:
		f.close()
		return ERR_CANT_OPEN
	var dec := 5
	_write_header(w, 1, true, dec, 0, 0.0, 0.0)
	var size := f.get_length()
	var n := 0
	var lo := INF
	var hi := -INF
	var dec_seen := false
	while not f.eof_reached():
		var line := f.get_line().strip_edges()
		if line.is_empty():
			continue
		var parts := line.split(",")
		if parts.size() < 6:
			continue
		var t := parse_time_min(parts[0])
		if t < 0:
			continue
		if not dec_seen:
			var dot := parts[1].find(".")
			dec = parts[1].length() - dot - 1 if dot >= 0 else 0
			dec_seen = true
		var rec := PackedByteArray()
		rec.resize(REC_SIZE)
		rec.encode_u64(0, t)
		rec.encode_double(8, parts[1].to_float())
		rec.encode_double(16, parts[2].to_float())
		rec.encode_double(24, parts[3].to_float())
		rec.encode_double(32, parts[4].to_float())
		rec.encode_u64(40, maxi(parts[5].to_int(), 0))
		w.store_buffer(rec)
		lo = minf(lo, parts[3].to_float())
		hi = maxf(hi, parts[2].to_float())
		n += 1
		if n % CHUNK == 0:
			if progress.is_valid():
				progress.call(minf(f.get_position() / maxf(size, 1), 0.99))
			await Engine.get_main_loop().process_frame
	_write_header_at(w, 1, true, dec, n, lo, hi)
	w.close()
	f.close()
	if progress.is_valid():
		progress.call(1.0)
	return OK

func _write_header_at(f: FileAccess, p_tf: int, p_has_time: bool, p_dec: int, p_count: int, p_lo: float, p_hi: float) -> void:
	var pos := f.get_position()
	f.seek(0)
	_write_header(f, p_tf, p_has_time, p_dec, p_count, p_lo, p_hi)
	f.seek(pos)

# ---- timeframe aggregation (async, chunked) ----------------------------

# builds the aggregated rbin for `tf` from the nearest available parent
# (5m from 1m, 1h from 5m, 2h from 1h, D from 2h, W from D); skips when the
# cached file is fresh relative to the parent
func ensure_tf_async(csv_path: String, tf: String, progress: Callable = Callable()) -> Error:
	if tf == "1m":
		return OK
	var out := cache_path(csv_path, tf)
	var parent_tf := _parent_tf(tf)
	var parent_path := cache_path(csv_path, parent_tf)
	if FileAccess.file_exists(out) and FileAccess.file_exists(parent_path):
		if FileAccess.get_modified_time(out) >= FileAccess.get_modified_time(parent_path):
			return OK
	if not FileAccess.file_exists(parent_path):
		return ERR_FILE_NOT_FOUND
	var pf := FileAccess.open(parent_path, FileAccess.READ)
	if pf == null:
		return ERR_CANT_OPEN
	var ph := _peek_header(pf)
	pf.close()
	if ph.is_empty():
		return ERR_PARSE_ERROR
	var w := FileAccess.open(out, FileAccess.WRITE)
	if w == null:
		return ERR_CANT_OPEN
	_write_header(w, int(TF_MINUTES[tf]), true, ph["dec"], 0, 0.0, 0.0)
	var src := QuoteStore.new()
	if not src.open(parent_path):
		w.close()
		return ERR_CANT_OPEN
	var n: int = ph["count"]
	var lo := INF
	var hi := -INF
	var out_n := 0
	var cur_bucket := -1
	var co := 0.0
	var ch := -INF
	var cl := INF
	var cc := 0.0
	var cv := 0
	var i := 0
	while i < n:
		var upto := mini(i + CHUNK, n)
		var recs: Array = []
		src.read_range_raw(i, upto, recs)
		for r in recs:
			var bt := bucket_start(int(r["t"]), tf)
			if bt != cur_bucket and cur_bucket >= 0:
				_store_agg(w, cur_bucket, co, ch, cl, cc, cv)
				out_n += 1
				lo = minf(lo, cl)
				hi = maxf(hi, ch)
				ch = -INF
				cl = INF
				cv = 0
			if bt != cur_bucket:
				cur_bucket = bt
				co = r["o"]
				ch = r["h"]
				cl = r["l"]
				cc = r["c"]
			else:
				ch = maxf(ch, r["h"])
				cl = minf(cl, r["l"])
				cc = r["c"]
			cv += int(r["v"])
		i = upto
		if progress.is_valid():
			progress.call(minf(float(i) / maxf(n, 1), 0.99) * 0.95)
		await Engine.get_main_loop().process_frame
	if cur_bucket >= 0:
		_store_agg(w, cur_bucket, co, ch, cl, cc, cv)
		out_n += 1
		lo = minf(lo, cl)
		hi = maxf(hi, ch)
	_write_header_at(w, int(TF_MINUTES[tf]), true, ph["dec"], out_n, lo, hi)
	w.close()
	src.close()
	if progress.is_valid():
		progress.call(1.0)
	return OK

static func _parent_tf(tf: String) -> String:
	match tf:
		"5m": return "1m"
		"1h": return "5m"
		"2h": return "1h"
		"D": return "2h"
		"W": return "D"
	return "1m"

static func _peek_header(f: FileAccess) -> Dictionary:
	if f.get_length() < HEADER_SIZE:
		return {}
	f.seek(0)
	var h := f.get_buffer(HEADER_SIZE)
	if h.decode_u32(0) != MAGIC:
		return {}
	return {
		"tf": h.decode_u32(12), "count": int(h.decode_u64(16)),
		"lo": h.decode_double(24), "hi": h.decode_double(32), "dec": h.decode_s32(40),
	}

func read_range_raw(a: int, b: int, out: Array) -> void:
	out.clear()
	if _f == null or a >= count:
		return
	b = mini(b, count)
	_f.seek(HEADER_SIZE + a * REC_SIZE)
	var want := (b - a) * REC_SIZE
	var buf := _f.get_buffer(want)
	if buf.size() < want:
		buf.resize(want)
	for off in range(0, want, REC_SIZE):
		out.append({
			"t": buf.decode_u64(off), "o": buf.decode_double(off + 8),
			"h": buf.decode_double(off + 16), "l": buf.decode_double(off + 24),
			"c": buf.decode_double(off + 32), "v": buf.decode_u64(off + 40),
		})

static func _store_agg(w: FileAccess, t: int, o: float, h: float, l: float, c: float, v: int) -> void:
	var rec := PackedByteArray()
	rec.resize(REC_SIZE)
	rec.encode_u64(0, t)
	rec.encode_double(8, o)
	rec.encode_double(16, h)
	rec.encode_double(24, l)
	rec.encode_double(32, c)
	rec.encode_u64(40, v)
	w.store_buffer(rec)
