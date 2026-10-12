#include <godot_cpp/godot.hpp>
#include <godot_cpp/classes/ref_counted.hpp>
#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/variant/string.hpp>

extern "C" int sim_generate(const char *params, const char *out_path);
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
	}

public:
	int generate(const godot::String &params, const godot::String &out_path) {
		godot::CharString ps = params.utf8();
		godot::CharString op = out_path.utf8();
		return sim_generate(ps.ptr(), op.ptr());
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
		}
	});
	init_obj.set_minimum_library_initialization_level(godot::MODULE_INITIALIZATION_LEVEL_SCENE);

	return init_obj.init();
}
}
