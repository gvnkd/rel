{-# LANGUAGE StrictData #-}

-- feedforward MLP for the offline trainer: tanh hidden layers, softmax
-- output with cross-entropy loss. Everything is pure; SGD is a plain
-- fold over the gradient of the full batch. Weights are flat unboxed
-- vectors in row-major layout: w ! (i * out + j) is input i -> output j.
module Sim.ML.Model
  ( Layer (..)
  , Model (..)
  , LayerGrad (..)
  , initModel
  , layerSizes
  , forward
  , predict
  , predictClass
  , gradOne
  , sgdStep
  , loss
  ) where

import qualified Data.Vector.Unboxed as VU
import System.Random.SplitMix (SMGen, nextWord64)

data Layer = Layer
  { lIn  :: !Int
  , lOut :: !Int
  , lW   :: !(VU.Vector Double)
  , lB   :: !(VU.Vector Double)
  } deriving (Eq, Show)

data Model = Model
  { mLayers :: [Layer]
  } deriving (Eq, Show)

-- per-layer gradient, same layout as the layer weights
data LayerGrad = LayerGrad
  { lgW :: !(VU.Vector Double)
  , lgB :: !(VU.Vector Double)
  } deriving (Show)

-- Xavier-uniform init from a splitmix generator; `sizes` is
-- input : hidden... : output (output is the class count)
initModel :: SMGen -> [Int] -> Model
initModel g0 sizes = Model (go g0 (zip sizes (drop 1 sizes)))
  where
    go _ [] = []
    go g ((i, o) : rest) =
      let (ws, g')  = rolls g (i * o) (range i o)
          (bs, g'') = rolls g' o (range i o)
      in Layer i o (VU.fromList ws) (VU.fromList bs) : go g'' rest
    range i o = sqrt (6 / fromIntegral (i + o))
    rolls g 0 _ = ([], g)
    rolls g k r =
      let (u, g1) = rollDouble g
          (xs, g2) = rolls g1 (k - 1) r
      in (2 * r * u - r : xs, g2)

layerSizes :: Model -> [Int]
layerSizes m = case mLayers m of
  [] -> []
  ls -> map lIn ls ++ [lOut (last ls)]

-- activations per layer, head = input vector
forward :: Model -> VU.Vector Double -> [VU.Vector Double]
forward m x = go x (mLayers m)
  where
    go a [] = [a]
    go a (l : ls) =
      let z = affine l a
          a' = if null ls then softmax z else VU.map tanh z
      in a : go a' ls

-- softmax class probabilities
predict :: Model -> VU.Vector Double -> VU.Vector Double
predict m x = case forward m x of
  []      -> VU.empty
  acts    -> last acts

predictClass :: Model -> VU.Vector Double -> Int
predictClass m x = VU.maxIndex (predict m x)

-- cross-entropy loss (natural log) of the batch
loss :: Model -> [(VU.Vector Double, Int)] -> Double
loss m samples =
  let n = max 1 (length samples)
      one (x, y) =
        let ps = predict m x
        in if y >= 0 && y < VU.length ps then negate (log (max 1e-12 (ps VU.! y))) else 0
  in sum (map one samples) / fromIntegral n

-- analytic gradient of the loss for one sample: softmax + cross-entropy
-- gives dlogits = probs - onehot; hidden layers backprop through tanh.
-- walks layers output-ward to input-ward; `inp` is the activation feeding
-- the current layer (= its gradOne-input), also the output of the layer
-- below, so (1 - inp^2) is that layer's tanh'
gradOne :: Model -> VU.Vector Double -> Int -> [LayerGrad]
gradOne m x y =
  let acts = forward m x
      probs = last acts
      dOut = VU.imap (\j p -> p - (if j == y then 1 else 0)) probs
      go _ [] _ = []
      go da (l : ls) (inp : inps) =
        let gw = VU.generate (lIn l * lOut l) $ \k ->
                   let (i, j) = k `divMod` lOut l
                   in (if i < VU.length inp then inp VU.! i else 0) * (da VU.! j)
            gb = da
            dprev = VU.generate (lIn l) $ \i ->
              sum [ (lW l VU.! (i * lOut l + j)) * (da VU.! j)
                  | j <- [0 .. lOut l - 1] ]
            dzPrev
              | null ls   = VU.empty
              | otherwise = VU.imap (\i v -> v * (1 - let a = if i < VU.length inp then inp VU.! i else 0 in a * a)) dprev
        in LayerGrad gw gb : go dzPrev ls inps
  -- grads come out output-layer-first; return them aligned with mLayers
  in reverse (go dOut (reverse (mLayers m)) (reverse (init acts)))

-- one full-batch SGD step: gradients summed over the batch, averaged,
-- then subtracted with the learning rate
sgdStep :: Double -> Model -> [(VU.Vector Double, Int)] -> Model
sgdStep _ m [] = m
sgdStep lr m samples =
  let gradss = map (\(x, y) -> gradOne m x y) samples
      combined = foldl1 (zipWith addGrads) gradss
      n = length samples
      scaled = map (scaleGrad (1 / fromIntegral n)) combined
      layers' = zipWith apply (mLayers m) scaled
  in Model layers'
  where
    addGrads a b = LayerGrad (VU.zipWith (+) (lgW a) (lgW b)) (VU.zipWith (+) (lgB a) (lgB b))
    scaleGrad f g = LayerGrad (VU.map (* f) (lgW g)) (VU.map (* f) (lgB g))
    apply l g = Layer (lIn l) (lOut l)
      (VU.zipWith (\w dw -> w - lr * dw) (lW l) (lgW g))
      (VU.zipWith (\b db -> b - lr * db) (lB l) (lgB g))

-- layer output: out_j = sum_i x_i * w(i,j) + b_j
affine :: Layer -> VU.Vector Double -> VU.Vector Double
affine l x = VU.generate (lOut l) $ \j ->
  (if j < VU.length (lB l) then lB l VU.! j else 0)
    + sum [ (if i < VU.length x then x VU.! i else 0) * (lW l VU.! (i * lOut l + j))
          | i <- [0 .. lIn l - 1] ]

softmax :: VU.Vector Double -> VU.Vector Double
softmax z
  | VU.null z = VU.empty
  | otherwise =
      let m = VU.maximum z
          es = VU.map (\v -> exp (v - m)) z
          s = VU.sum es
      in VU.map (/ s) es

rollDouble :: SMGen -> (Double, SMGen)
rollDouble g =
  let (w, g') = nextWord64 g
  in (fromIntegral w / 2 ^ (64 :: Int), g')
