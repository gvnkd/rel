# rel — project memories

Order-book market sim. Redot (Godot fork) 26.2 = renderer + UI only; **Haskell (`sim/`) is the single source of truth for all business logic**.

## Architecture

- `sim/` — Haskell package `rel-sim`: `Sim.Engine` (order book: `Side`/`Order`/`Book`/`Trade`, volumes positive by construction via `mkVol`), `Sim.Gen` (seeded splitmix generator; `refPrice own other` = opposite-side-best-first — callers pass own side first), `Sim.Cli`/`Sim.Csv` (CSV: open,high,low,close,vol — vol = sum of trade volumes), `SimFFI` (foreign-library, C ABI `sim_generate`). QuickCheck suite runs in the nix build (buildPhase).
- `gdextension/shim.cpp` — godot-cpp 4.5 shim exposing `SimNative.generate(params, out_path)` to GDScript; `hs_init`/`hs_exit` in ELF constructors.
- `sim_panel.gd` — params form + timeline rows (CheckBox show/hide + per-row × remove, in a ScrollContainer) inside `tool_window.gd` (`SimWindow` in chart.tscn): drag by title bar (press via bar `gui_input`, motion via window `_input` — no bubbling), ×/F1 show/hide; Generate writes `user://simdata/<name>.csv` via SimNative. `chart.gd` skips pan/zoom when the pointer is over SimWindow or while it's dragging.
- `chart.gd` / `chart.tscn` — chart scene: root Control owns `ChartView` (shared index↔x / price↔y transform, pan/zoom, nice grid steps) + `PlotData` (series list, playback head). Widgets (full-rect Controls, all draw in screen space from ChartView, no Camera2D/node scaling): `grid_widget.gd` (grid + axis labels in margins), `series_widget.gd` (modes candles/bars/line/dots, key `M`), `indicator_widget.gd` (SMA, key `I`), `order_book_widget.gd` (bids/asks ladder via `set_ladder`, synthetic placeholder data from last candle until sim exports book), `cursor_widget.gd` (OHLC at head), `cross_widget.gd` (crosshair + axis tags, follows mouse). Keys: space/=`-` playback, wheel zoom at cursor (X only), RMB drag scales X/Y, drag pan detaches follow. Rendering draws every candle inside the visible index window (bounded by screen width, ~1.4k candles/frame max) — no replay-head cap; `view.follow` auto-scrolls the tail to the right edge while playing, re-attaches on new timeline.
- `nix/` — ALL nix code lives here (`redot.nix`, `sim.nix`, `godot-cpp.nix`, `gdextension.nix`, `patches/`). Root has only `flake.nix`.
- Deleted and gone: GDScript order-book engine, `tests.gd`, order-book UI. Do not reintroduce logic into GDScript.

## Commands

- `nix build .#redot` / `.#sim` / `.#sim-extension` — packages
- `nix run .#link-sim-extension` — copy `.so` + `.gdextension` into `addons/sim_native/` (required after rebuild; artifacts gitignored)
- `nix flake check` — sim-generate + scene-loads checks
- Headless: `./result/bin/redot --headless --path . --import` (needed once for global class cache), then `-s script.gd` or `--quit-after N chart.tscn`
- CLI: `sim --candles 100000 --orders-per-candle 1000` → CSV stdout (~14s for 100M orders)
- Redot version: 26.2-stable built from source, `rev` overridable in `nix/redot.nix`

## Nix gotchas (all cost a build cycle)

- **Nix builds from the git tree.** Untracked/intent-to-add files are invisible → `can't find source for ...`. Always `git add` before `nix build` / `nix flake check`. `flake.nix` itself must be tracked to evaluate.
- `nix build .#sim-extension` retargets the `./result` symlink away from redot — re-run `nix build .#redot` (cached) or use the store path directly.
- Redot editor scans **every `.gdextension` under `res://`** — never keep template `.gdextension` files in source dirs (it creates `extension_list.cfg` entries). Template lives in `nix/gdextension.nix`.
- Haskell foreign-library on nixpkgs: needs `-fPIC` on the **library stanza**, plus `splitmix` override with `--ghc-options=-fPIC` (`nix/sim.nix`). `options: standalone` does NOT work (nix GHC's static RTS is non-PIC) — link RTS dynamically and fix rpaths with `autoPatchelfHook` in the consumer.
- GHC RTS does not self-init when dlopen'ed — `hs_init` via `__attribute__((constructor))` in the shim, else `newBoundTask: RTS is not initialised`.
- Cabal rejects module + dir name clash (`Sim.Types` file + `Sim.Types/` dir) — internal module is flat `Sim.Internal`.
- `cp` from nix store into project: `rm -f` target first + `chmod u+w` (store perms are read-only).

## GDScript/Redot gotchas

- `:=` inference fails on Variant chains (Dictionary values, untyped node vars) — annotate the var type explicitly.
- Inner classes: access via `const PlotDataScript := preload(...)` then `PlotDataScript.Series.new()`, not via node instance.
- In `SceneTree -s` scripts, `@onready` vars are not set in `_initialize()` — `await process_frame` after `add_child` first.
- `SimNative` GDExtension class only exists after the extension is linked and project imported; `sim_panel.gd` guards with `ClassDB.class_exists`.

## State / prefs

- Master branch; commits only when Sergey asks (he says "commit"/"coomit"), push only when asked.
- As of 2026-10-12: commits through `d17d4cc` are local, NOT pushed.
- Perf target: 100k candles × 1k orders ≈ 14s. Regressions matter.
