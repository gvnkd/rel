# DTW Correlation Scanner — methods and targets

Subsequence-DTW search for cross-series shape correlation: slide a window
over series A and, for every window, find the best-matching part of series
B — invariant to any affine Y-deformation (scale/offset) and to X-deformation
(time warp) within a band. The Haskell package `rel-sim` (`Sim.Dtw`,
`Sim.DtwIndex`, `Sim.Run`) is the single source of truth; the GDScript UI
(`dtw_panel.gd`, "DTW" tab) and the `dtw` CLI are thin layers over the
FFI/CLI paths.

## Pipeline

```
A (csv | rbin) ── streaming closes ──► Prefix A (sums, sums²)
B (csv | rbin) ── streaming closes ──► Prefix B ──► fingerprint index per (win)
        │
        ▼  per stepped A window, parallel chunks across all cores
z-normalize window → PAA fingerprint word
        → Hamming-≤2 neighbor probes in B's index (≤ maxcand candidates)
        → banded free-end DTW verification per candidate
        → best (position in B, per-step distance)
        ▼
position-line csv + summary json per (tfA, tfB, win) session
```

## What the matcher answers

For each A-window `A[i, i+win)`:

- **where** in B the closest shape lives (`j` = start index in B candles),
- **how close** (`dist` = mean squared z-distance per warping-path step —
  comparable across window lengths),
- guaranteed **monotone, non-shuffling** alignment: the warping path only
  steps `(+1,0)/(0,+1)/(+1,+1)`, so a match can never map A's past onto
  B's future.

Y-deformation is factored out exactly: both the window and each candidate
B-segment are z-normalized before distance is computed, so `B ≈ a·A + b`
matches at `dist ≈ 0` for any `a > 0, b`. X-deformation is absorbed by the
Sakoe–Chiba band: the path may drift at most `band` candles off the
diagonal.

## The distance, precisely

Banded subsequence DTW with a **free end**: the query is consumed fully,
the match in B may end anywhere within the band of the diagonal. Reference
normalization uses mean/std over the `win`-length window at the candidate
start (UCR convention) — normalizing over the whole `win + 2·band` segment
would mix match and trailing content and inflate distances.

`dist = (Σ (z_q − z_r)² along path) / pathLength` — per-step mean squared
error. Two independent z-normalized windows score ≈ 2.0; identical shapes
≈ 0; real cross-series finds on derived signals go down to ~1e-4.

## Candidate recall (the fingerprint index)

Verifying every B position with DTW is O(nB·win) per window — infeasible at
30M-row series. Instead each window is summarized once:

1. **PAA**: the z-normalized window is cut into `paa` segments (default 8);
   segment means are O(1) via prefix sums.
2. **Quantization**: each segment mean is digitized through equiprobable
   gaussian breakpoints (`card` levels, default 8); digits pack into one
   Word64 (`card^paa ≤ 2^24`).
3. **Index**: a dense counting-sort index over B (`offsets + positions`
   vectors, ~15 bytes/position) maps word → positions.
4. **Probes**: the query word plus all words within Hamming distance 2
   (single-digit deviation ≤ 1) are looked up; candidates are verified in
   probe-priority order, capped at `maxcand` (default 64) per window.

Guarantee: the scan **never reports a distance below** the brute-force
optimum; for genuinely close matches (small dist) the index recalls the
exact optimum. Arbitrary noise optima are not fingerprint-recoverable —
that is the designed speed/recall trade-off, tuned by `maxcand`.

Fingerprint recall is meaningful for **autocorrelated** series (prices,
random walks). White noise windows quantize unstably under warp; and random
walks self-similarly correlate, so the global best of a scan isn't
necessarily "the" relationship — read the position line, not just the best.

Degenerate flat regions: zero-variance windows z-normalize to all-zeros and
match everywhere at dist 0 (self-comparison of sim data shows ~2–5% of
windows aligning to a non-identity position at cap 64; `maxcand=512`
recovers ~all). Real quotes data does not exhibit this at these windows.

## Parameters

| param | default | meaning |
|---|---|---|
| `windows` | `64,128,256,512` | window lengths in A candles; one parallel session each. Each must be divisible by `paa` |
| `band` | `win/4` | Sakoe–Chiba band: max time drift in candles (both sides) |
| `step` | `win/4` | A-window slide step |
| `maxcand` | 64 | candidate verifications cap per window |
| `paa` | 8 | fingerprint segments per window |
| `card` | 8 | quantization levels per segment (3..8) |

Constraints enforced per session (violations → session status `short`):
`win mod paa == 0`, `card^paa ≤ 2^24`, `len(A) ≥ win`, `len(B) ≥ win + 2·band`.

## Inputs

| input | notes |
|---|---|
| sim candles csv | `open,high,low,close,vol` |
| real-quotes csv | leading `YYYY.MM.DD HH:MM` timestamp column, sniffed per row |
| `.rbin` RELQ / RELS | binary stores (chart exports); fixed 48B records |

The DTW loader (`Sim.ML.Data.loadCloses`) accepts **any** prices — unlike
the ML `Bar` parser it does not gate on positivity, so modulated/negative
series (e.g. offset signals) are read in full. The csv path streams from
the handle in 8MB chunks with a hand-rolled ByteString→Double parser; a
31M-row file loads in ~1GB instead of ~20GB.

## Timeframe grids

The panel's grid mode materializes every timeframe of two chart series —
store-backed series reuse their per-TF rbin caches (missing aggregates are
built down the `ensure_tf_async`/`_aggregate_tf_async` chains), sim series
are written as chunked temp csvs — and fires **one** FFI batch over all
non-empty tfA×tfB×window combinations (36 pairs × windows, all parallel).
TF labels follow the chart's `QuoteStore.TFS`: `1m, 5m, 1h, 2h, D, W`.

## Outputs

Per session (`<out>/dtw_<tfA>_<tfB>_<win>.{json,csv,bin}`):

- **json**: `{tfA, tfB, win, band, windows, found, best: {i, j, dist}, csv,
  dataa, datab}` — the data paths are stamped in after the scan so
  drill-downs work when a run is reloaded later.
- **csv** (position line): `i,j,dist` per stepped A window that matched
  (`j = −1` rows are omitted).
- **bin** (RELM v1): `u32 magic, u32 version, u32 win, u32 band, u32 step,
  u64 count`, then `count × 16B` records `(i u32, j i32, dist f64)` —
  ~2.5× smaller than the csv and parse-free; the UI prefers it and falls
  back to the csv. A past run reopens via the panel's "Load results…"
  (pick any `dtw_*.json`).

The panel's "chart" button converts the matches into a candles csv of the
**lead/lag offset `j − i`** (one row per stepped A window, gaps carried)
and loads it as a regular chart timeline: a flat line = stable correlation
at a constant offset, drift = relative speed change, jumps = regime
switches.

## Visualization (DTW page plots)

Each session row's "plot" button loads the matches into the page's
`dtw_plots.gd` widget with four modes:

| mode | view | reading |
|---|---|---|
| scatter | (i, j) points colored by dist, diagonal reference | diagonal clusters = stable sync; arcs = lead/lag change; clouds = noise |
| dist | per-step distance per A window (min/max envelope per pixel) | where in A the correlation is strong; ~2 = none, <0.1 = strong |
| drill | two z-normalized windows overlaid + warping-path connectors | what a match looks like and where it stretches; LMB-click scatter/dist to open (FFI `sim_dtw_path` recomputes the exact path, ~ms) |
| pairs | correspondence wiring: A index range on top, B on bottom, connector per matched window | the "wiring diagram" of the warp over a range; RMB-click twice to select |

Dense sessions downsample to ≤60k points (scatter) / one min-max envelope
column per pixel (dist), so 300k-window scans stay interactive.

## CLI

```
dtw --dataa data/EURUSDM1.csv --datab data/signal_mod_offset_samp10.csv \
    --out res --windows 64,128,256,512 \
    [--band N] [--step N] [--maxcand N] [--paa N] [--card N] \
    [--tfs 1m,5m,1h,2h,D,W]        # grid mode: comma lists in dataa/datab
```

Multicore is on by default (`-with-rtsopts=-N`); pass `+RTS -N16 -RTS` etc.
to override. Progress prints per session as `tfA/tfB w<win> <pct>%`.

## FFI

`sim_dtw_scan(params)` (blocking; run on a worker thread) with
`sim_dtw_progress` / `sim_dtw_sessions` (live JSON table with
`tfA/tfB/win/done/windows/best/status`) / `sim_dtw_cancel`. Params are the
table above as `key=value;...`; grid mode via `tfs=...` with comma-aligned
`dataa=`/`datab=` lists (empty entry skips the timeframe). Return codes:
0 ok, 1 bad params, 2 data/IO failure, 3 cancelled.

## Performance

Measured on EURUSDM1.csv (4.7M rows) × signal_mod_offset_samp10.csv
(31.4M rows), 4 window sessions, 32 cores:

- wall ~7 min for all four windows (win 256 alone: 3m20s at 1148% CPU),
- peak RSS ~3GB (loader ~1GB live + prefixes + per-session indexes),
- best dist 1.3e-4 at win 64 — the planted EURUSD↔signal relation is
  recovered at the correct positions.

Fingerprint pruning keeps the scan sub-quadratic: candidates reject at the
word-lookup stage; only plausible matches pay for DTW (≤ 64 verifications
× `win·(win+2·band)` cells per window).

## Gotchas

- `best = 0` on identical inputs is correct, not a bug (exact self-match).
- A session reporting `best 0.000000` with `found = 0` means "no result"
  (sentinel), not a perfect match — check the json's `found`.
- Cross-TF sessions (A 1m vs B 1D) have band/window semantics in each
  series' own candle units; the scan typically shows them as weak — that is
  informative, not broken.
- `B.spanEnd` leaks under stream loops (bytestring internals, ~45MB per 8MB
  chunk) — the loader uses `elemIndexEnd`; do not reintroduce spanEnd in
  chunked readers.
- The GHC RTS must be started with `-N` for any forkIO batch to use more
  than one core (the shim passes `+RTS -N -RTS` to `hs_init`).
