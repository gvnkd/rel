# Strategy DSL

Hand-authored strategies for the Strategy tab's "Backtest DSL" button.
Rules are evaluated **per candle**, top to bottom: the first rule whose
condition holds sets the position for that candle; if no rule matches,
the position is flat (0). The strategy is level-based and stateless —
it does not remember anything between candles.

Positions: `1` = long, `-1` = short, `0` = flat.

## Grammar

```
rule    := cond "->" pos                  one rule per line
cond    := or
or      := and ("|" and)*                 | is lowest
and     := not ("&" not)*                 & binds tighter
not     := "!" not | atom
atom    := "(" cond ")" | cmp | xup/xdn
cmp     := ind rel ind                    rel: >  >=  <  <=
xup/xdn := "xup(" ind "," ind ")"         cross up / down
ind     := "close" | "sma(" int ")"
```dsl

Indicators: `close` (the candle close), `sma(N)` (simple moving average,
`N` candles).

## Events vs levels

`xup`/`xdn` are **one-candle events**: they hold only on the candle where
the cross happens. To *hold* a position, use a level comparison — it
holds for every candle while it is true.

```dsl
xup(close, sma(20)) -> 1
```dsl

is long for exactly one candle after each up-cross (mostly flat — a
pitfall, not a strategy).

## Example 1 — dual-SMA trend ride

Long when the fast average is above the slow one, short when below.
Always in the market.

```dsl
sma(10)>sma(50) -> 1
sma(10)<sma(50) -> -1
```dsl

## Example 2 — trend with higher-timeframe confirmation

Both legs of the trend must agree, otherwise no rule matches and the
strategy sits flat. Trades less, pays less fee.

```dsl
sma(20)>sma(50) & sma(50)>sma(200) -> 1
sma(20)<sma(50) & sma(50)<sma(200) -> -1
```dsl

## Example 3 — pullback entry with trend gate

In an uptrend (`close` above its long average), buy while the fast pair
is below the slow pair (a pullback); mirror short in a downtrend. When
price and the fast pair disagree with the gate, no rule matches → flat.

```dsl
close>sma(100) & sma(10)<sma(30) -> 1
close<sma(100) & sma(10)>sma(30) -> -1
```dsl

## Notes

- Every `sma(N)` needs `N-1` candles of warm-up: the first usable candle
  is the slowest SMA's period, results before that are flat.
- The backtest fills at candle close and charges `fee` per position
  change; a strategy that flips every candle bleeds fees.
- Precedence binds tighter left to right within the same operator;
  parenthesize when in doubt.
