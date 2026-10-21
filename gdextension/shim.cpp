#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/ref_counted.hpp>
#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/variant/packed_float64_array.hpp>
#include <godot_cpp/variant/packed_int32_array.hpp>
#include <godot_cpp/variant/string.hpp>

#include <cstdlib>

extern "C" int sim_generate(const char *params, const char *out_path);
extern "C" int sim_continue(const char *params, const char *out_path);
extern "C" double sim_progress(void);
extern "C" void sim_cancel(void);
extern "C" int sim_ml_train(const char *params);
extern "C" double sim_ml_progress(void);
extern "C" char *sim_ml_sessions(void);
extern "C" void sim_ml_cancel(void);
extern "C" int sim_dtw_scan(const char *params);
extern "C" double sim_dtw_progress(void);
extern "C" char *sim_dtw_sessions(void);
extern "C" void sim_dtw_cancel(void);
extern "C" char *sim_dtw_path(const char *params);
extern "C" int sim_strat_optimize(const char *params);
extern "C" double sim_strat_progress(void);
extern "C" char *sim_strat_sessions(void);
extern "C" void sim_strat_cancel(void);
extern "C" int sim_strat_dsl(const char *params);
extern "C" int sim_fft_forecast(const double *closes, int count, int start, int end, int predict, int waves, double edge_w, double *out);
extern "C" int sim_fft_best(const double *closes, int count, int predict, int rmse_pct, double *out);
extern "C" int sim_perm_entropy(const double *closes, int count, int window, int m, double *out);
extern "C" int sim_cecp(const double *closes, int count, int window, int m, double *out);
extern "C" int sim_markov(const int *states, int count, int window, int surpwin, double lam_hi, double lam_lo, double s_ref, double k, double *out);
extern "C" int sim_sma(const double *closes, int count, int period, double *out);
extern "C" int sim_macd(const double *closes, int count, int fast, int slow, int sig, double *out);
extern "C" int sim_rsi(const double *closes, int count, int period, double *out);
extern "C" int sim_bb(const double *closes, int count, int period, double mult, double *out);
extern "C" void hs_init(int *argc, char **argv[]);
extern "C" void hs_exit(void);

__attribute__((constructor)) static void simffi_init(void) {
	// "+RTS -N -I0 -RTS": all CPU capabilities (otherwise every forkIO
	// batch runs single-core) and NO idle GC — after a multi-GB scan the
	// idle collector would major-GC the retained heap continuously,
	// pinning a core at 100% while the editor sits idle
	int argc = 5;
	char *argv[] = { (char *)"rel-sim", (char *)"+RTS", (char *)"-N", (char *)"-I0", (char *)"-RTS", nullptr };
	char **argv_ptr = argv;
	hs_init(&argc, &argv_ptr);
}

__attribute__((destructor)) static void simffi_exit(void) {
	hs_exit();
}

class SimNative : public godot::RefCounted {
	GDCLASS(SimNative, godot::RefCounted);

protected:
	static void _bind_methods() {
		godot::ClassDB::bind_method(godot::D_METHOD("generate", "params", "out_path"), &SimNative::generate);
		godot::ClassDB::bind_method(godot::D_METHOD("resume", "params", "out_path"), &SimNative::resume);
		godot::ClassDB::bind_method(godot::D_METHOD("progress"), &SimNative::progress);
		godot::ClassDB::bind_method(godot::D_METHOD("cancel"), &SimNative::cancel);
		godot::ClassDB::bind_method(godot::D_METHOD("fft_forecast", "closes", "start", "end", "predict", "waves", "edge_w"), &SimNative::fft_forecast);
		godot::ClassDB::bind_method(godot::D_METHOD("perm_entropy", "closes", "window", "m"), &SimNative::perm_entropy);
		godot::ClassDB::bind_method(godot::D_METHOD("cecp", "closes", "window", "m"), &SimNative::cecp);
		godot::ClassDB::bind_method(godot::D_METHOD("sma", "closes", "period"), &SimNative::sma);
		godot::ClassDB::bind_method(godot::D_METHOD("macd", "closes", "fast", "slow", "sig"), &SimNative::macd);
		godot::ClassDB::bind_method(godot::D_METHOD("rsi", "closes", "period"), &SimNative::rsi);
		godot::ClassDB::bind_method(godot::D_METHOD("bb", "closes", "period", "mult"), &SimNative::bb);
		godot::ClassDB::bind_method(godot::D_METHOD("markov", "states", "window", "surpwin", "lam_hi", "lam_lo", "s_ref", "k"), &SimNative::markov);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_train", "params"), &SimNative::ml_train);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_progress"), &SimNative::ml_progress);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_sessions"), &SimNative::ml_sessions);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_cancel"), &SimNative::ml_cancel);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_optimize", "params"), &SimNative::strat_optimize);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_progress"), &SimNative::strat_progress);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_sessions"), &SimNative::strat_sessions);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_cancel"), &SimNative::strat_cancel);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_backtest_dsl", "params"), &SimNative::strat_backtest_dsl);
		godot::ClassDB::bind_method(godot::D_METHOD("dtw_scan", "params"), &SimNative::dtw_scan);
		godot::ClassDB::bind_method(godot::D_METHOD("dtw_progress"), &SimNative::dtw_progress);
		godot::ClassDB::bind_method(godot::D_METHOD("dtw_sessions"), &SimNative::dtw_sessions);
		godot::ClassDB::bind_method(godot::D_METHOD("dtw_cancel"), &SimNative::dtw_cancel);
		godot::ClassDB::bind_method(godot::D_METHOD("dtw_path", "params"), &SimNative::dtw_path);
	}

public:
	int generate(const godot::String &params, const godot::String &out_path) {
		godot::CharString ps = params.utf8();
		godot::CharString op = out_path.utf8();
		return sim_generate(ps.ptr(), op.ptr());
	}

	int resume(const godot::String &params, const godot::String &out_path) {
		godot::CharString ps = params.utf8();
		godot::CharString op = out_path.utf8();
		return sim_continue(ps.ptr(), op.ptr());
	}

	double progress() {
		return sim_progress();
	}

	void cancel() {
		sim_cancel();
	}

	// ML training: starts a batch of parallel sessions, blocks until done
	// (call on a worker thread), writes per-session results into out dir
	int ml_train(const godot::String &params) {
		godot::CharString ps = params.utf8();
		return sim_ml_train(ps.ptr());
	}

	double ml_progress() {
		return sim_ml_progress();
	}

	// live session table as JSON; the C string is copied then freed
	godot::String ml_sessions() {
		char *s = sim_ml_sessions();
		godot::String r(s);
		::free(s);
		return r;
	}

	void ml_cancel() {
		sim_ml_cancel();
	}

	// DTW correlation scan: starts a batch of parallel window-length
	// sessions, blocks until done (call on a worker thread), writes
	// per-session results into out dir
	int dtw_scan(const godot::String &params) {
		godot::CharString ps = params.utf8();
		return sim_dtw_scan(ps.ptr());
	}

	double dtw_progress() {
		return sim_dtw_progress();
	}

	// live session table as JSON; the C string is copied then freed
	godot::String dtw_sessions() {
		char *s = sim_dtw_sessions();
		godot::String r(s);
		::free(s);
		return r;
	}

	void dtw_cancel() {
		sim_dtw_cancel();
	}

	// single-window drill-down: warping path + z-normalized windows as JSON;
	// the C string is copied then freed
	godot::String dtw_path(const godot::String &params) {
		godot::CharString ps = params.utf8();
		char *s = sim_dtw_path(ps.ptr());
		godot::String r(s);
		::free(s);
		return r;
	}

	// strategy optimizer: starts a batch of parallel sessions, blocks until
	// done (call on a worker thread), writes per-session results into out dir
	int strat_optimize(const godot::String &params) {
		godot::CharString ps = params.utf8();
		return sim_strat_optimize(ps.ptr());
	}

	double strat_progress() {
		return sim_strat_progress();
	}

	// live session table as JSON; the C string is copied then freed
	godot::String strat_sessions() {
		char *s = sim_strat_sessions();
		godot::String r(s);
		::free(s);
		return r;
	}

	void strat_cancel() {
		sim_strat_cancel();
	}

	// hand-authored DSL strategy backtest (fast, synchronous); writes the
	// optimizer's artifact layout with seed 0 into the out dir
	int strat_backtest_dsl(const godot::String &params) {
		godot::CharString ps = params.utf8();
		return sim_strat_dsl(ps.ptr());
	}

	godot::PackedFloat64Array fft_forecast(const godot::PackedFloat64Array &closes, int64_t start, int64_t end, int64_t predict, int64_t waves, double edge_w) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || predict < 1 || predict > 1000000)
			return out;
		out.resize((int)(closes.size() + predict));
		int got = sim_fft_forecast(closes.ptr(), (int)closes.size(), (int)start, (int)end, (int)predict, (int)waves, edge_w, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	godot::PackedFloat64Array perm_entropy(const godot::PackedFloat64Array &closes, int64_t window, int64_t m) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || window < 2 || m < 2)
			return out;
		out.resize((int)closes.size());
		int got = sim_perm_entropy(closes.ptr(), (int)closes.size(), (int)window, (int)m, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	// rolling Complexity-Entropy Causality Plane: (H, C) interleaved per candle
	godot::PackedFloat64Array cecp(const godot::PackedFloat64Array &closes, int64_t window, int64_t m) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || window < 2 || m < 2)
			return out;
		out.resize(2 * (int)closes.size());
		int got = sim_cecp(closes.ptr(), (int)closes.size(), (int)window, (int)m, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	// simple moving average: one double per candle, NaN warm-up
	godot::PackedFloat64Array sma(const godot::PackedFloat64Array &closes, int64_t period) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || period < 1)
			return out;
		out.resize((int)closes.size());
		int got = sim_sma(closes.ptr(), (int)closes.size(), (int)period, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	// MACD: (macd, signal, hist) interleaved per candle, NaN warm-up
	godot::PackedFloat64Array macd(const godot::PackedFloat64Array &closes, int64_t fast, int64_t slow, int64_t sig) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || fast < 1 || slow < 1 || sig < 1)
			return out;
		out.resize(3 * (int)closes.size());
		int got = sim_macd(closes.ptr(), (int)closes.size(), (int)fast, (int)slow, (int)sig, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	// Wilder's RSI: one double per candle, NaN for i < period
	godot::PackedFloat64Array rsi(const godot::PackedFloat64Array &closes, int64_t period) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || period < 1)
			return out;
		out.resize((int)closes.size());
		int got = sim_rsi(closes.ptr(), (int)closes.size(), (int)period, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	// Bollinger bands: (mid, upper, lower) interleaved per candle, NaN warm-up
	godot::PackedFloat64Array bb(const godot::PackedFloat64Array &closes, int64_t period, double mult) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || period < 1)
			return out;
		out.resize(3 * (int)closes.size());
		int got = sim_bb(closes.ptr(), (int)closes.size(), (int)period, mult, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	// Bull-Sideways-Bear probabilities + adaptive Markov matrix over
	// PRE-CLASSIFIED states (one per slice bar, 0 Bu / 1 SW / 2 Be, index
	// 0 a dummy): 15 doubles per bar — [bsbp x3, transition matrix
	// row-major x9, surprise, rolling surprise, lambda]. The caller
	// classifies the whole loaded slice so the states are exact for any
	// subslice; only the count/surprise chain restarts at the slice start
	godot::PackedFloat64Array markov(const godot::PackedInt32Array &states, int64_t window, int64_t surpwin, double lam_hi, double lam_lo, double s_ref, double k) {
		godot::PackedFloat64Array out;
		if (states.is_empty() || window < 1 || surpwin < 1)
			return out;
		out.resize(15 * (int)states.size());
		int got = sim_markov(states.ptr(), (int)states.size(), (int)window, (int)surpwin, lam_hi, lam_lo, s_ref, k, out.ptrw());
		out.resize(got < 0 ? 0 : got);
		return out;
	}

	godot::PackedFloat64Array fft_best(const godot::PackedFloat64Array &closes, int64_t predict, int64_t rmse_pct) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || predict < 1 || predict > 100000)
			return out;
		out.resize(3);
		if (sim_fft_best(closes.ptr(), (int)closes.size(), (int)predict, (int)rmse_pct, out.ptrw()) == 1)
			return out;
		out.clear();
		return out;
	}
};

extern "C" {

GDExtensionBool GDE_EXPORT sim_native_init(
		GDExtensionInterfaceGetProcAddress get_proc_address,
		GDExtensionClassLibraryPtr library,
		GDExtensionInitialization *initialization) {
	godot::GDExtensionBinding::InitObject init_obj(get_proc_address, library, initialization);

	init_obj.register_initializer([](godot::ModuleInitializationLevel level) {
		if (level == godot::MODULE_INITIALIZATION_LEVEL_SCENE) {
			godot::ClassDB::register_class<SimNative>();
		godot::ClassDB::bind_method(godot::D_METHOD("fft_best", "closes", "predict", "rmse_pct"), &SimNative::fft_best);
	}
	});
	init_obj.set_minimum_library_initialization_level(godot::MODULE_INITIALIZATION_LEVEL_SCENE);

	return init_obj.init();
}
}
