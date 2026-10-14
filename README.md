# rel

Order-book market simulator. The Haskell package `rel-sim` (`sim/`) is the
single source of truth for all business logic; Redot (Godot fork) is the
renderer and UI only. One run produces OHLCV candles (and optionally a
per-tick book-export sidecar) from a seeded order flow.

## Model

Each step rolls an order with probability given by the buy/sell thresholds.
Its price is anchored at the bid/ask mid (falling back to the opposite
side's best, then the last trade price, when a side is empty), offset by a
random distance in `[0, tick-range]`. Orders rest on the book; whenever the
best bid >= best ask the book matches at the ask price, trade by trade.
Volumes are positive by construction; nothing prevents prices from drifting
arbitrarily far except the behaviors below.

## Parameters

All parameters are CLI flags (`sim --flag value`) and key=value pairs for
the FFI/UI path (`"flag=value;..."`). Defaults reproduce legacy runs
bit-identically.

### Flow

- `--seed N` (42) — RNG seed. Fully determines the run; same params + seed
  = byte-identical output.
- `--candles N` (1000) — number of candles. Scales runtime ~linearly.
- `--orders-per-candle lo,hi` (500,500) — uniform roll of order attempts
  per candle. Higher = smoother candles, more trades per candle, longer
  runs. `lo==hi` draws no RNG (deterministic count).

### Order sizing

- `--vol lo,hi` (1,20) — uniform order volume. Larger volumes drain book
  levels faster → more trades per placement, fatter candle volume.
- `--start-vol V` (200) — volume of the two initial book orders.

### Direction

- `--buy-thresh D` (0.25) — roll below this ⇒ a buy order.
- `--sell-thresh D` (0.75) — roll above this ⇒ a sell order. Between the
  two thresholds nothing is placed. Widening the gap (e.g. 0.4/0.6) makes
  the book idle longer → fewer trades, more book-only steps in tick mode.
  Moving both thresholds up/down tilts the drift: persistent sell excess
  walks the mid down, buy excess walks it up.

### Price formation

- `--start-bid P`, `--start-ask P` (102190, 102195) — initial book, in
  ticks (see `--price-scale`). Sets the opening price and the initial
  half-spread (`(ask-bid)/2` ticks).
- `--tick-range N` (10) — max offset from the reference price, in ticks
  (arithmetic mode) or basis points (percent mode). The volatility knob:
  larger = wider candle ranges and deeper book stacks away from the mid.
- `--price-scale N` (100000) — ticks per price unit; output prices are
  `ticks / scale` with 5 decimals. Changing it rescales the output without
  changing the dynamics (tick counts stay the same).
- `--price-offset ticks|percent` (ticks) —
  - `ticks`: arithmetic offset — order rests at `ref ± d` ticks. Constant
    absolute volatility: at price 1.0 a 10-tick move is 0.01%, at price
    0.001 it is 100%. The mid can walk to and below zero.
  - `percent`: proportional offset — order rests at `ref · (1 ± d/10000)`
    (d basis points). Volatility scales with price, so relative candle
    sizes stay roughly constant across the price range. The ask is floored
    at 1 tick: this mode cannot quote a non-positive price regardless of
    `--tick-range`.

### Price guards (agent behaviour)

- `--clamp-rest on|off` (off) — an order may not rest further than
  tick-range beyond its own side's best. Keeps the book compact, prevents
  runaway stacks far from the touch; costs a little per-order perf (the
  heftier ~+40% mid-anchoring overhead is unconditional, not from clamp).
- `--zero-floor on|off` (off) — agents refuse to place an order that would
  rest at a non-positive price (a seller won't pay to sell). Removes
  negative prints without distorting quotes above zero; near zero the flow
  becomes one-sided (bids keep arriving, asks dry up) and the market tends
  to bounce off the floor rather than die. Without it, arithmetic offsets
  with a large `--tick-range` can print negative prices.

### Output

- `--mode candles|tick-trades|tick-steps` (candles) —
  - `candles`: only the OHLCV csv (`open,high,low,close,vol`; vol = sum of
    trade volumes).
  - `tick-trades`: additionally writes `<out>.ticks.csv` — one row per
    trade plus an end-of-candle book snapshot marker.
  - `tick-steps`: additionally one row per placed order that did not
    trade. Sidecar size scales with orders × `--book-depth` (depth 50 at
    500 ord/candle ≈ 2.5 MB per 1k candles).
- `--book-depth N` (50) — book levels per side recorded in tick rows;
  affects sidecar width only, not the simulation.

## Building / running

- `nix build .#sim` — CLI (`result/bin/sim`)
- `nix build .#sim` + `nix flake check` — QuickCheck suite runs in the build
- `nix run .#link-sim-extension` — copy the FFI library into the Redot
  project (required after rebuilds; then restart the editor fully)
