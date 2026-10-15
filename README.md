# rel

Order-book market simulator. The Haskell package `rel-sim` (`sim/`) is the
single source of truth for all business logic; Redot (Godot fork) is the
renderer and UI only. One run produces OHLCV candles (and optionally a
per-tick book-export sidecar) from a seeded order flow — or, in sinus
mode, from a closed-form harmonic signal with a known ground truth.

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

### Sinus generator

`--sinus on|off` (off) replaces the order-book flow with a pure sinus
generator (`Sim.Sinus`): no book, no RNG — every candle is a closed-form
function of its absolute index, so a run (or a resume) is reproducible
trivially. Output is always the plain OHLCV csv; tick modes don't apply
(a stale sidecar from an earlier same-name tick run is removed). Purpose:
a ground-truth signal for validating indicators — FFT must recover the
exact ladder frequencies, KS must read a clean cyclic series.

- `--sinus-period N` (100) — wave-0 period in candles.
- `--sinus-amp N` (500) — wave-0 amplitude in basis points of the start
  mid. Clamped to a quarter of the base price, so the composite (whose
  total swing is bounded by 2×amp) stays strictly positive; a 1-tick
  floor backs that up.
- `--sinus-waves N` (1) — harmonic count. Wave k has period
  `max 2 (period/(k+1))` and amplitude `amp·0.5^k`, phases aligned: a
  classic decaying harmonic ladder. Waves shorter than 8 candles fall
  under the FFT indicator's hard low-pass — that boundary is deliberate.
- `--sinus-vol-base N` (1000), `--sinus-vol-amp N` (400) — volumes follow
  the same ladder a quarter cycle ahead (phase π/2, so volume peaks mid
  price-move), floored at 1.

Candle shape: opens chain closes; high/low = body ± a small fixed wick
(`amp/20`, at least 1 tick) so wick-consuming indicators see non-trivial
values. Resuming a sinus timeline continues the phase seamlessly (the
first new candle's open chains the last written close). The UI exposes
the same as a `sinus` checkbox plus five fields in the sim panel.

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

## Chart indicators

Indicators live in the right-click chart menu (`add SMA` / `add FFT` /
`add KS`; per-indicator period/params, color, delete). The math runs in
Haskell behind the FFI (`Sim.Fft`, `Sim.Entropy`), the widget only draws.
The cursor tableau (top right) appends each enabled indicator's value at
the hovered candle (or the replay head): `KS 0.871`, `SMA 1.02195`, etc.
Values are blank during warm-up / outside the indicator's range.

- **SMA** — simple moving average of closes over `period` candles,
  drawn on the price scale.
- **FFT** — Fourier fit + forecast over a draggable window. Two dashed
  vertical handle lines (`s:` / `e:`) mark the window; drag them to move
  the edges, or enable `lock` to drag the whole range at a fixed width.
  The solid light-colored curve inside the window is the ridge-LS fit of
  the top spectral bins (frequencies snapped to exact DFT bins of the
  window, low-passed at period ≥ 8 candles so per-candle noise is never
  extrapolated); the dashed curve continues it for `predict` candles,
  anchored at the fit's last point. `waves` caps the harmonic count
  (0 = auto). `edge w` endpoint-weights the fit (linear ramps over
  ~n/128 candles at both edges): boundary leverage is `2k·w0/Σw`, so
  with a rich basis (many waves) even moderate values pin smooth
  endpoint deviations — on real data the last-candle fit error drops
  ~4x at `edge w` 100, tightening the fit/prediction junction. A sharp
  single-candle spike at the edge is unmatchable by any smooth basis;
  heavy weights there backfire (Gibbs overshoot) — keep `edge w` low
  for those. `opt` runs a walk-forward search over (window, waves) for
  the given `predict` length, scoring each candidate with the hybrid
  objective `w·relRMSE + (1−w)·(1−direction accuracy)` where `w` is the
  `opt rmse %` spin; the optimized window is placed so its prediction
  overlaps the last 50 candles of real data, and the achieved score is
  shown in the HUD, e.g. `FFT [1912..2950]+64x24 (0.031)` — read it as
  in-sample, no forward guarantee.
- **KS** — rolling permutation entropy (Bandt–Pompe) as a Kolmogorov-
  Sinai proxy, normalized to 0..1 and drawn over the full plot height
  (1 = noise-like, 0 = trending). `period` is the sliding window,
  `m` (2–7, default 5) the ordinal-pattern length; NaN until the window
  fills. Caveat: short windows cannot saturate — e.g. period 100 with
  m=5 tops out around 0.87 (finite-sample, not a bug); use m=3 or a
  wider window to reach ~1 on noise.

### FFT internals (developer notes)

The pipeline in `Sim.Fft` (`sim/src/Sim/Fft.hs`):

1. **Spectrum** — iterative in-place unboxed FFT of the demeaned window,
   zero-padded to the next power of two.
2. **Bin selection** — top-k magnitudes non-maximum-suppressed within a
   mainlobe (±ceil(m/n) padded bins) so one strong wave cannot starve
   weaker harmonics of the picks, snapped to exact window DFT bins
   (`f = q/n`), hard low-pass at period ≥ 8 candles. Snapped columns are
   exactly orthogonal over the window, so the solve provably cannot
   produce runaway coefficients; the low-pass stops per-candle noise
   from being extrapolated.
3. **Refinement** — the top two bins are re-estimated by ternary
   maximization of the single-component LS energy within one window bin
   of the zero-padded peak (the snap grid is too coarse when the period
   does not divide the window: the fit drifts out of phase toward the
   edges). The objective is the intercept-adjusted explained energy
   `bᵀG⁻¹b`, not the raw correlation `c²+s²` — off-bin columns are
   neither orthogonal nor equal-norm, and the raw peak is displaced by
   up to ~0.1 bin at low frequencies. Refined candidates replace the
   snapped set only when their in-window SSE wins by a 10% margin, so
   near-bin refinements that merely absorb harmonic leakage never beat
   an exact orthogonal bin.
4. **Fit** — weighted ridge least squares with an intercept column via
   closed-form Gram matrices (Dirichlet kernel + ramp sums for
   `edge w`; Jacobi eigendecomposition with near-null eigen-direction
   truncation as belt-and-braces). The intercept matters: demeaning
   alone leaves the window mean outside the cos/sin span, and at few
   cycles per window the no-intercept fit compromises toward that
   leftover DC instead of the true frequency.
5. **Synthesis** — the fit and forecast are generated by the rotation
   recurrence (4 multiplies per point per wave, no per-point trig).
6. **Search** (`opt`) — walk-forward grid over (window, waves), anchors
   stepped by `predict`, scored on the mandatory 50-candle real-data
   overlap with the hybrid objective.

Everything is O(window) per wave plus O(k²) small-matrix work; a 100k
window at 50 waves takes ~130 ms. On the sinus ground truth the fit
tracks within the tick-quantization floor (~0.5 tick) for any window
phase, fractional cycles included; before refinement the edge error
reached the full amplitude. Tests: QuickCheck suite (runs in the
nix build, fails it on any property failure) brute-force-verifies the
Gram/correlations against direct design-matrix builds and locks
fractional-cycle recovery on quantized sinus sums.

## Building / running

- `nix build .#sim` — CLI (`result/bin/sim`)
- `nix build .#sim` + `nix flake check` — QuickCheck suite runs in the build
- `nix run .#link-sim-extension` — copy the FFI library into the Redot
  project (required after rebuilds; then restart the editor fully)
