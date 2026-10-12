{
  lib,
  stdenv,
  godot-cpp,
  sim,
  gmp,
  libffi,
  autoPatchelfHook,
}:

stdenv.mkDerivation {
  pname = "sim-native";
  version = "0.1.0";

  src = ../gdextension;

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [
    godot-cpp
    sim
    gmp
    libffi
  ];

  buildPhase = ''
    SIM_LIB=$(dirname $(find ${sim}/lib -name 'libsimffi.so' | head -1))
    GODOT_CPP_LIB=$(find ${godot-cpp}/lib -name 'libgodot-cpp.*.a' | head -1)
    $CXX -std=c++17 -fPIC -shared \
      -I${godot-cpp}/include \
      -I${godot-cpp}/include/godot_cpp \
      -I${godot-cpp}/include/gdextension \
      -o libsimnative.linuxbsd.editor.x86_64.so \
      shim.cpp \
      "$GODOT_CPP_LIB" \
      -L"$SIM_LIB" -lsimffi \
      -Wl,-rpath,$SIM_LIB
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/sim_native/bin
    cat > $out/share/sim_native/sim_native.gdextension <<'EOF'
    [configuration]

    entry_symbol = "sim_native_init"
    compatibility_minimum = "4.5"
    reloadable = true

    [libraries]

    linux.debug.x86_64 = "bin/libsimnative.linuxbsd.editor.x86_64.so"
    linux.release.x86_64 = "bin/libsimnative.linuxbsd.editor.x86_64.so"
    EOF
    cp libsimnative.linuxbsd.editor.x86_64.so $out/share/sim_native/bin/
    runHook postInstall
  '';

  meta = {
    description = "GDExtension exposing the rel-sim Haskell library to Redot";
    platforms = lib.platforms.linux;
  };
}
