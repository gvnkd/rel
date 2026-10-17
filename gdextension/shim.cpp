#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/ref_counted.hpp>
#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/variant/packed_float64_array.hpp>
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
extern "C" int sim_strat_optimize(const char *params);
extern "C" double sim_strat_progress(void);
extern "C" char *sim_strat_sessions(void);
extern "C" void sim_strat_cancel(void);
extern "C" int sim_strat_dsl(const char *params);
extern "C" int sim_fft_forecast(const double *closes, int count, int start, int end, int predict, int waves, double edge_w, double *out);
extern "C" int sim_fft_best(const double *closes, int count, int predict, int rmse_pct, double *out);
extern "C" int sim_perm_entropy(const double *closes, int count, int window, int m, double *out);
extern "C" int sim_cecp(const double *closes, int count, int window, int m, double *out);
extern "C" int sim_markov(const double *closes, int count, int window, int surpwin, double thresh, double lam_hi, double lam_lo, double s_ref, double k, double *out);
extern "C" void hs_init(int *argc, char **argv[]);
extern "C" void hs_exit(void);

__attribute__((constructor)) static void simffi_init(void) {
	int argc = 0;
	char *argv[] = { nullptr };
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
		godot::ClassDB::bind_method(godot::D_METHOD("markov", "closes", "window", "surpwin", "thresh", "lam_hi", "lam_lo", "s_ref", "k"), &SimNative::markov);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_train", "params"), &SimNative::ml_train);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_progress"), &SimNative::ml_progress);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_sessions"), &SimNative::ml_sessions);
		godot::ClassDB::bind_method(godot::D_METHOD("ml_cancel"), &SimNative::ml_cancel);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_optimize", "params"), &SimNative::strat_optimize);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_progress"), &SimNative::strat_progress);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_sessions"), &SimNative::strat_sessions);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_cancel"), &SimNative::strat_cancel);
		godot::ClassDB::bind_method(godot::D_METHOD("strat_backtest_dsl", "params"), &SimNative::strat_backtest_dsl);
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

	// Bull-Sideways-Bear probabilities + adaptive Markov matrix: 15 doubles
	// per bar — [bsbp x3, transition matrix row-major x9, surprise, rolling
	// surprise, lambda]
	godot::PackedFloat64Array markov(const godot::PackedFloat64Array &closes, int64_t window, int64_t surpwin, double thresh, double lam_hi, double lam_lo, double s_ref, double k) {
		godot::PackedFloat64Array out;
		if (closes.is_empty() || window < 1 || surpwin < 1)
			return out;
		out.resize(15 * (int)closes.size());
		int got = sim_markov(closes.ptr(), (int)closes.size(), (int)window, (int)surpwin, thresh, lam_hi, lam_lo, s_ref, k, out.ptrw());
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
