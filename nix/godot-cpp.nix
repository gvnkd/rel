{
  lib,
  stdenv,
  fetchFromGitHub,
  python3,
  scons,
}:

stdenv.mkDerivation {
  pname = "godot-cpp";
  version = "4.5-stable";

  src = fetchFromGitHub {
    owner = "godotengine";
    repo = "godot-cpp";
    rev = "godot-4.5-stable";
    hash = "sha256-TfDqd5lfwCFz4IZxVwQi2hhKZTVbCfaJ0GCeScElAx0=";
  };

  nativeBuildInputs = [
    python3
    scons
  ];

  enableParallelBuilding = true;

  sconsFlags = [
    "platform=linux"
    "target=editor"
    "arch=x86_64"
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib $out/include
    cp bin/libgodot-cpp.*.a $out/lib/
    cp -r include/godot_cpp $out/include/
    cp -r gen/include/godot_cpp $out/include/
    cp -r gdextension $out/include/
    runHook postInstall
  '';

  meta = {
    description = "C++ bindings for the Godot/Redot GDExtension API";
    homepage = "https://github.com/godotengine/godot-cpp";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
