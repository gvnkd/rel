{-# LANGUAGE StrictData #-}

-- order-book level lifetimes for the OBVOL indicator: one segment per
-- continuous existence of a (side, price) level — open when volume first
-- rests at the price, closed when fully consumed. The book never cancels
-- resting volume (Sim.Engine), so disappearance == full consumption at
-- the top of the book. The tick drivers observe both events directly,
-- which the top-N snapshot serialization cannot express: a resting level
-- that leaves the snapshot window is still in the book.
--
-- Each lifetime also accumulates its volume-change events (rests bump,
-- partial fills reduce; the absolute level volume after each change) so
-- the UI can render brightness/width dynamics; events flush with the
-- closing segment, keeping each lifetime's records contiguous in the
-- export (ev0/evn are record indices).
module Sim.Lvl
  ( LvlSeg (..)
  , LvlEv (..)
  , LvlTr (..)
  , emptyTr
  , trFromBook
  , trRest
  , trConsume
  , trCloseAll
  , trTotalEv
  ) where

import qualified Data.Map.Strict as Map
import Sim.Internal

data LvlEv = LvlEv
  { leC    :: !Int
  , leSide :: !Side
  , lePx   :: !Price
  , leVol  :: !Int
  }
  deriving (Eq, Show)

data LvlSeg = LvlSeg
  { lvC0   :: !Int
  , lvC1   :: !Int
  , lvSide :: !Side
  , lvPx   :: !Price
  , lvPeak :: !Int
  , lvEv0  :: !Int
  , lvEvN  :: !Int
  }
  deriving (Eq, Show)

-- open level: current/peak volume, placement candle, and the buffered
-- volume-change events since opening (most recent head). Event indices
-- are assigned at CLOSE time — while levels are open their events
-- interleave in time, so a lifetime's contiguous export range is only
-- known when it flushes
data Lvl = Lvl
  { lvCur   :: !Int
  , lvPeak' :: !Int
  , lvStart :: !Int
  , lvBuf   :: [LvlEv]
  }

data LvlTr = LvlTr
  { trBids   :: !(Map.Map Price Lvl)
  , trAsks   :: !(Map.Map Price Lvl)
  , trNextEv :: !Int
  }

emptyTr :: LvlTr
emptyTr = LvlTr Map.empty Map.empty 0

trTotalEv :: LvlTr -> Int
trTotalEv = trNextEv

-- seed open lifetimes from a book snapshot (resume): every level present
-- at ci starts a fresh segment there — the original placement candle and
-- peak are lost across a restart, so the resumed segment under-reports
-- both (documented; straight runs are exact). evBase continues the
-- event indexing across the restart
trFromBook :: Int -> Int -> Book -> LvlTr
trFromBook evBase ci b = LvlTr
  (Map.map (\v -> Lvl (unVol v) (unVol v) ci []) (bookBids b))
  (Map.map (\v -> Lvl (unVol v) (unVol v) ci []) (bookAsks b))
  evBase

-- volume v rested at (side, p): opens a segment at a level that was
-- empty, otherwise bumps the current/peak volume of the open one. Always
-- buffers an event — the volume changed
trRest :: Int -> Side -> Price -> Int -> LvlTr -> LvlTr
trRest ci side p v tr = case side of
  Bid -> tr { trBids = go (trBids tr) }
  Ask -> tr { trAsks = go (trAsks tr) }
  where
    go levels = case Map.lookup p levels of
      Nothing ->
        Map.insert p (Lvl v v ci [LvlEv ci side p v]) levels
      Just (Lvl c pk st buf) ->
        let c' = c + v
        in Map.insert p (Lvl c' (max pk c') st (LvlEv ci side p c' : buf)) levels

-- a top-of-book match consumes v from BOTH the best bid and the best ask
-- level (Sim.Engine.matchStep takes min bv av from each). Partial
-- consumption buffers an event; full consumption closes with a segment
-- paired with the lifetime's buffered events (the drop to 0 is implicit
-- in the segment end) and takes the next contiguous event range.
-- Consumption at a price with no open segment is ignored (defensive; the
-- tick drivers seed from the same event stream)
trConsume :: Int -> Price -> Price -> Int -> LvlTr -> (LvlTr, [(LvlSeg, [LvlEv])])
trConsume ci bp ap v tr =
  let (bs, p1, n1) = eat (trNextEv tr) Bid bp (trBids tr)
      (as', p2, n2) = eat (trNextEv tr + n1) Ask ap (trAsks tr)
  in (tr { trBids = bs, trAsks = as', trNextEv = trNextEv tr + n1 + n2 }, p1 ++ p2)
  where
    eat n0 side p levels = case Map.lookup p levels of
      Nothing     -> (levels, [], 0)
      Just (Lvl c pk st buf)
        | c <= v    -> ( Map.delete p levels
                       , [(LvlSeg st ci side p pk n0 (length buf), reverse buf)]
                       , length buf
                       )
        | otherwise ->
            let c' = c - v
            in ( Map.insert p (Lvl c' (max pk c') st (LvlEv ci side p c' : buf)) levels
               , []
               , 0
               )

-- close every open segment (end of run), assigning contiguous event
-- ranges in flush order
trCloseAll :: Int -> LvlTr -> [(LvlSeg, [LvlEv])]
trCloseAll ci tr = go (trNextEv tr)
  ( [ (Bid, p, x) | (p, x) <- Map.toAscList (trBids tr) ]
      ++ [ (Ask, p, x) | (p, x) <- Map.toAscList (trAsks tr) ]
  )
  where
    go _ [] = []
    go n ((side, p, Lvl _ pk st buf) : rest) =
      ( LvlSeg st ci side p pk n (length buf)
      , reverse buf
      ) : go (n + length buf) rest
