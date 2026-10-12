{
  lib,
  stdenv,
  fetchFromGitHub,
  installShellFiles,
  perl,
  pkg-config,
  scons,
  wayland-scanner,
  alsa-lib,
  dbus,
  embree,
  enet,
  fontconfig,
  freetype,
  glib,
  glslang,
  graphite2,
  harfbuzz,
  icu,
  libdecor,
  libGL,
  libjpeg_turbo,
  libpulseaudio,
  libtheora,
  libwebp,
  libx11,
  libxcursor,
  libxext,
  libxfixes,
  libxi,
  libxinerama,
  libxkbcommon,
  libxrandr,
  libxrender,
  mbedtls,
  miniupnpc,
  openxr-loader,
  pcre2,
  recastnavigation,
  sdl3,
  speechd-minimal,
  udev,
  vulkan-loader,
  wayland,
  wslay,
  zstd,
  version ? "26.2",
  rev ? "redot-26.2-stable",
  hash ? "sha256-G8BKrgNwOMjuOxrL57VncZqMHqxl0/am3XjdaJjBOgw=",
  extraSconsFlags ? [ ],
}:

let
  harfbuzz-icu = harfbuzz.override { withIcu = true; };
  mkSconsFlags = lib.mapAttrsToList (
    k: v: if builtins.isBool v then "${k}=${builtins.toJSON v}" else "${k}=${v}"
  );
in
stdenv.mkDerivation (finalAttrs: {
  pname = "redot";
  inherit version;

  src = fetchFromGitHub {
    owner = "Redot-Engine";
    repo = "redot-engine";
    inherit rev hash;
  };

  nativeBuildInputs = [
    installShellFiles
    perl
    pkg-config
    scons
    wayland-scanner
  ];

  buildInputs = [
    alsa-lib
    dbus
    embree
    enet
    fontconfig
    freetype
    glib
    glslang
    graphite2
    harfbuzz-icu
    icu
    libdecor
    libGL
    libjpeg_turbo
    libpulseaudio
    libtheora
    libwebp
    libx11
    libxcursor
    libxext
    libxfixes
    libxi
    libxinerama
    libxkbcommon
    libxrandr
    libxrender
    mbedtls
    miniupnpc
    openxr-loader
    pcre2
    recastnavigation
    sdl3
    speechd-minimal
    udev
    vulkan-loader
    wayland
    wslay
    zstd
  ];

  env.BUILD_NAME = "nix";

  patches = [ ./patches/linuxbsd-fix-glslang-libs.patch ];

  postPatch = ''
    # stop scons from hiding NIX_CFLAGS_COMPILE and friends
    perl -pi -e '{ $r += s:(env = Environment\(.*):\1\nenv["ENV"] = os.environ: } END { exit ($r != 1) }' SConstruct

    # prefer system libraries over bundled thirdparty copies
    perl -pi -e '{ $r |= s:(opts.Add\(BoolVariable\("builtin_.*, )True(\)\)):\1False\2: } END { exit ($r != 1) }' SConstruct

    substituteInPlace thirdparty/glad/egl.c \
      --replace-fail \
      'static const char *NAMES[] = {"libEGL.so.1", "libEGL.so"}' \
      'static const char *NAMES[] = {"${lib.getLib libGL}/lib/libEGL.so"}'

    substituteInPlace thirdparty/glad/gl.c \
      --replace-fail \
      'static const char *NAMES[] = {"libGLESv2.so.2", "libGLESv2.so"}' \
      'static const char *NAMES[] = {"${lib.getLib libGL}/lib/libGLESv2.so"}'

    substituteInPlace thirdparty/glad/gl{,x}.c \
      --replace-fail '"libGL.so.1"' '"${lib.getLib libGL}/lib/libGL.so"'

    substituteInPlace thirdparty/volk/volk.c \
      --replace-fail \
      'dlopen("libvulkan.so.1"' \
      'dlopen("${lib.getLib vulkan-loader}/lib/libvulkan.so"'
  '';

  sconsFlags = mkSconsFlags {
    precision = "single";
    production = true;
    platform = "linuxbsd";
    target = "editor";
    debug_symbols = false;

    alsa = true;
    dbus = true;
    fontconfig = true;
    pulseaudio = true;
    speechd = true;
    touch = true;
    udev = true;
    wayland = true;
    x11 = true;

    module_mono_enabled = false;

    # aliasing bugs exist with hardening+LTO
    ccflags = "-fno-strict-aliasing";
    linkflags = "-Wl,--build-id";

    # libraries not packaged in nixpkgs
    builtin_msdfgen = true;
    builtin_rvo2_2d = true;
    builtin_rvo2_3d = true;
    builtin_xatlas = true;
    builtin_clipper2 = true;

    redirect_build_objects = false;

    use_sowrap = false;
  } ++ extraSconsFlags;

  enableParallelBuilding = true;

  strictDeps = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/libexec
    find bin -maxdepth 1 -type f -name 'redot.*' -exec cp {} $out/libexec/redot \;
    ln -s ../libexec/redot $out/bin/redot

    installManPage misc/dist/linux/redot.6

    mkdir -p $out/share/applications $out/share/icons/hicolor/scalable/apps
    cp misc/dist/linux/org.redotengine.Redot.desktop $out/share/applications/
    substituteInPlace $out/share/applications/org.redotengine.Redot.desktop \
      --replace-fail "Exec=redot" "Exec=$out/bin/redot"
    cp icon.svg $out/share/icons/hicolor/scalable/apps/redot.svg
    cp icon.png $out/share/icons/redot.png

    runHook postInstall
  '';

  meta = {
    description = "Redot Engine — community-driven fork of the Godot game engine";
    homepage = "https://redotengine.org";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "redot";
  };
})
