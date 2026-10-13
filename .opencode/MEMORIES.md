# rel — project memories

Order-book market sim. Redot (Godot fork) 26.2 = renderer + UI only; **Haskell (`sim/`) is the single source of truth for all business logic**.

## Architecture

- `sim/` — Haskell package `rel-sim`: `Sim.Engine` (order book: `Side`/`Order`/`Book`/`Trade`, volumes positive by construction via `mkVol`), `Sim.Gen` (seeded splitmix generator; `refPrice own other` = opposite-side-best-first — callers pass own side first), `Sim.TickGen` (tick export, duplicated roll helpers — do NOT share with Sim.Gen, exporting them costs ~40% runSim perf), `Sim.Cli`/`Sim.Csv` (CSV: open,high,low,close,vol — vol = sum of trade volumes), `SimFFI` (foreign-library, C ABI `sim_generate`). QuickCheck suite runs in the nix build (buildPhase).
- tick modes: `--mode tick-trades|tick-steps` (+ `--book-depth N`, default 50 = old GDScript `ob_depth`) writes `<out>.ticks.csv` sidecar next to the candles csv: `c,price,vol` + book-depth `(bp,bv,ap,av)` quads padded with `0.00000,0`; `vol=0` rows are book-only (marker = last row of every candle, exact end-of-candle book). UI: sim panel mode dropdown; chart aggregates ticks to candles and plays the per-candle book snapshot at the replay head (`Series.books`, `OrderBookWidget.set_book`); plain csvs keep the synthetic ladder.
- anti-spike options (2026-10-12): `--price-ref opposite|mid` + `--clamp-rest on|off` (UI: "price ref" dropdown + "clamp rest" checkbox in sim_panel.gd). Root cause of ±0.4–0.5 spikes: leftover volume rests up to `tick-range` beyond the OPPOSITE touch, so bid/ask stacks do independent random walks and the spread between them never closes (processOrders only matches `bp >= ap`); alternating orders then print at opposite ends of the gap. Clamp = price may not rest > tick-range beyond own side's best; mid = anchor at bid/ask mid. Both kill spikes (max move 0.59 → ≤0.005); baseline (opposite/off) is bit-identical to pre-change output.
- async UI generation (2026-10-13): `sim_panel.gd` runs `SimNative.generate` on a GDScript `Thread` (GHC auto-binds the foreign pthread on the `foreign export` entry); `_process` polls `progress()` (0..1, per-candle, hits 1.0 during the post-loop CSV write while thread still alive) and `wait_to_finish()` wires the timeline back. Generate button toggles to Cancel → `cancel()`. Engine: `SimHooks` (per-candle progress + cancel poll) in `Sim.Internal`; hooked drivers in SEPARATE modules `Sim.GenH`/`Sim.TickGenH` (full copies, same rationale as Gen↔TickGen). FFI: `sim_progress`/`sim_cancel` exports over NOINLINE IORefs; cancel throws `SimCancelled`, `sim_generate` returns 3 and removes csv/.meta/ticks sidecar. `-threaded` REQUIRED on the foreign-library (non-threaded RTS + worker in `base`'s unsafe `write()` → main-thread `sim_progress` dies with `schedule: re-entered unsafely`).
- tick-mode scale gotchas (2026-10-13): the ticks sidecar is ~2.5MB per 1k candles at 500 ord/candle tick-trades, depth 50 — a 30k-candle run writes a 7.3GB sidecar (minutes of "99%" write-out). Progress reserves the last percent: hook reports done/(total+1), (1,1) only after all files written — without this the UI shows 100% for the whole write-out. Fixed O(n²) tick accumulation: `ts ++ reverse ticks` per candle and `reverse ticks ++ acc` per order both copied growing lists; now chunked (`reverse ticks : tss`, final `concat (reverse tss)`) in TickGen AND TickGenH, output identical. CLI (`app/Main.hs`) uses `runSim` only — tick paths are reachable ONLY via FFI/UI and the QuickCheck suite; CLI timings say nothing about tick-mode perf.
- streaming tick export (2026-10-13): the FFI/UI tick path no longer materializes ticks — `runSimTicksH :: SimHooks -> (Tick -> IO ()) -> SimParams -> IO [Candle]` emits rows through a sink (`Sim.Csv.tickRow`/`tickHeader` exported); SimFFI opens the sidecar with 1MB BlockBuffering and sinks rows during generation. 30k×500 tick-trades: RSS flat 176MB (was multi-GB + swap), output BYTE-IDENTICAL to list-based, cancel mid-stream removes the partial sidecar. Emit order per candle: order-step ticks forward, then end-of-candle marker (list-based export reverses the same batch — verified identical). Row sink is `Sim.Csv.tickRowB` (bytestring Builder): bytestring 0.12 REMOVED doubleFixed, and prices are p/10^scale exactly, so the INTEGER is formatted directly (intDec + zero-padded fraction) — byte-identical to printf "%.5f", ~13x faster (107MB/s vs 8MB/s; 7.7GB sidecar in 100s). Never pkill -f with a pattern that appears in your own command line — zsh -c cmdline matches and the shell kills itself.
- perf pitfall measured 2026-10-13: adding the IO driver (or any large binding) to `Sim.Gen` itself starves GHC's shared simplifier budget and de-specializes the untouched `runSim` (11.7s → 15-16s, chain: runOrders→mkPriceFun→lambda inlining). Duplicating machinery inside the module doesn't help; separate module restores 11.7s bit-identical. `{-# INLINE runOrders #-}` alone gets 12.7s — insufficient.
- book-drain deadlock: when BOTH sides empty, refPrice returns Nothing → market freezes forever (mid mode hit it at seed 1). Fixed via `gsAnchor` (last trade price, init = start mid) used as final refPrice fallback; Gen.TickGen both update it, logic duplicated as usual.
- perf: 100k×1k ≈ 11.7s baseline; clamp/mid modes ≈ 15.8–16s (+35%, opt-in only — per-order Maybe-chain overhead, profiling not worth it). Perf trick: `mkPriceFun` resolves ref-mode/clamp ONCE per run into a specialized `PriceFun` closure — per-order flag checks cost ~40%.
- `gdextension/shim.cpp` — godot-cpp 4.5 shim exposing `SimNative.generate(params, out_path)` to GDScript; `hs_init`/`hs_exit` in ELF constructors.
- `sim_panel.gd` — params form + timeline rows (CheckBox show/hide + per-row × remove, in a ScrollContainer) inside `tool_window.gd` (`SimWindow` in chart.tscn): drag by title bar (press via bar `gui_input`, motion via window `_input` — no bubbling), ×/F1 show/hide; Generate writes `user://simdata/<name>.csv` via SimNative. `chart.gd` skips pan/zoom when the pointer is over SimWindow or while it's dragging.
- `chart.gd` / `chart.tscn` — chart scene: root Control owns `ChartView` (shared index↔x / price↔y transform, pan/zoom, nice grid steps) + `PlotData` (series list, playback head). Widgets (full-rect Controls, all draw in screen space from ChartView, no Camera2D/node scaling): `grid_widget.gd` (grid + axis labels in margins), `series_widget.gd` (modes candles/bars/line/dots, key `M`), `indicator_widget.gd` (SMA, key `I`), `order_book_widget.gd` (bids/asks ladder via `set_book(bids, asks, mid)` from per-candle snapshots; `set_ladder` = synthetic fallback), `cursor_widget.gd` (OHLC at head), `cross_widget.gd` (crosshair + axis tags, follows mouse). Keys: space/=`-` playback, wheel zoom at cursor (X only), RMB drag scales X/Y, drag pan detaches follow. Rendering draws every candle inside the visible index window (bounded by screen width, ~1.4k candles/frame max) — no replay-head cap; `view.follow` auto-scrolls the tail to the right edge while playing, re-attaches on new timeline.
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
