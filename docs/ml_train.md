# ML Trainer — methods and targets

Offline supervised trainer for next-candle action prediction. The Haskell
package `rel-sim` (`Sim.ML.*`) is the single source of truth; the GDScript
UI (`ml_panel.gd`, "ML Trainer" tab) is a thin control/visualization layer
over the FFI.

## Pipeline

```
data (csv | rbin) → features + labels → split/normalize → mini-batch SGD
                → per-epoch val-Calmar model selection → test evaluation
                → metrics json + equity curve csv
```

## Data sources

| input | notes |
|---|---|
| sim candles csv | `open,high,low,close,vol` (sim panel output) |
| real-quotes csv | leading `YYYY.MM.DD HH:MM` timestamp column, sniffed per row |
| `.rbin` RELQ | real-quotes binary store (chart export) |
| `.rbin` RELS | sim full export (chart export; books/levels blocks unread) |

All inputs become `Bar` (double OHLCV); the ML path never touches integer
`Price`/scale.

## Features (tabular windows, per candle)

| feature | default window | meaning |
|---|---|---|
| lag returns | lags 1, 5, 20 | `close[i]/close[i-lag] − 1` |
| realized vol | 20 | std of lag-1 returns |
| range position | 20 | where close sits in the `[low, high]` envelope, 0..1 |
| SMA distance | 20 | `close/SMA − 1` |
| volume relative | 20 | `vol/SMA(vol) − 1` |
| permutation entropy (KS) | off (kswin=0) | rolling normalized PE in 0..1 |

Warm-up rows (window not filled) are empty and excluded from training.
Feature lags use only past/current candles — no future leakage by
construction.

## Targets (labels)

The target is the **next candle's direction**: `ret = close[i+1]/close[i] − 1`

| class | condition | policy position |
|---|---|---|
| 0 down | `ret < −labeleps` | short (−1) |
| 1 flat | `|ret| ≤ labeleps` (default 1e-5) | flat (0) |
| 2 up | `ret > labeleps` | long (+1) |

The predicted class maps directly to a position held over candle i → i+1;
this is the "possible action" the model outputs.

## Model

Feedforward MLP: `featureSize → hidden… → 3`, tanh hidden layers,
softmax output. Xavier-uniform init from a splitmix stream (one seed per
session). Hand-rolled forward/backward (no external DL deps); gradients
validated against finite differences in the QuickCheck suite.

## Optimization

- **Loss:** cross-entropy on the 3-class softmax.
- **Optimizer:** mini-batch SGD (default batch 256), full sweep of the
  train segment per epoch; the sweep order is shuffled each epoch with a
  splitmix stream seeded per session — fully deterministic per seed.
- **Hyperparameters:** learning rate `lr` (default 0.05), epochs
  (default 50), hidden sizes (`hidden`, default `16`).

## Segments and normalization

Chronological split: train (default 60%) → validation (20%) → test (rest,
min 10 samples). Features are standardized with the **train segment's**
mean/std only, then applied to all three segments.

## Reward, evaluation, model selection

- **Training reward** is plain next-candle PnL: `position · ret − fee · |Δposition|`
  (fee default 0.0002). No drawdown term in the loss — degenerate-policy
  detection is deferred until one actually appears.
- **Model selection:** after each epoch the model is ranked on the
  validation segment by **Calmar**; the best epoch's weights are kept and
  evaluated once on the held-out test segment.
- **Metrics** (test segment, best-on-val model):
  - `pnl` — total return of the policy walk
  - `maxDD` — max peak-to-trough drawdown of the equity curve
  - `calmar` — `pnl / maxDD` (falls back to `pnl` when `maxDD ≈ 0`)
  - `accuracy` — fraction of correct next-move class predictions
  - `trades` — position changes
- The equity curve is exported as `equity_<seed>.csv` (candles csv with
  `o=h=l=c=equity`) so the chart can overlay it directly.

## Parallel sessions

`sim_ml_train` forks N sessions (`sessions`, default 4) over the RTS
(`-threaded`): one seed per session (`seed…seed+N−1`), shared preprocessed
dataset, live JSON table via `sim_ml_sessions`, cancellation via
`sim_ml_cancel`. Sessions are independent model selection runs — the
spread across seeds is itself a signal (a single lucky seed ≠ edge).

## Parameter reference (FFI `key=value`)

| key | default | meaning |
|---|---|---|
| data | — (required) | csv or .rbin path |
| out | — (required) | results directory |
| sessions / seed | 4 / 1 | parallel session count / base seed |
| epochs / lr / batch | 50 / 0.05 / 256 | optimizer |
| hidden | `16` | hidden layer sizes, comma list |
| trainpct / valpct | 0.6 / 0.2 | split fractions |
| fee | 0.0002 | per position change |
| lags | `1,5,20` | return lag features |
| volwin / rangewin / smawin / volsma | 20 | feature windows |
| kswin / ksm | 0 / 5 | permutation entropy window / pattern length |
| labeleps | 1e-5 | flat-class threshold |

## Current limits / roadmap

- Tabular windows only — the protocol (`Bar` stream → features → action)
  stays unchanged when sequence models (streaming hidden state) land;
  that is also the point where GPU training (accelerate/CUDA/ROCm)
  becomes worthwhile.
- No drawdown penalty in the reward (Calmar is selection-only); add if
  degenerate always-in-market policies start winning PnL.
- No walk-forward retraining yet; the test segment is evaluated once.
