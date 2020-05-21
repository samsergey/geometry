{-# language DeriveFunctor #-}
module Decorations where

import Base
import Data.Monoid
import Data.Maybe
import Control.Monad

------------------------------------------------------------

data LabelSettings = LabelSettings
  { getLabel :: Last String
  , getLabelCorner :: Last (Int, Int)
  , getLabelOffset :: Last XY
  , getLabelPosition :: Last CN
  , getLabelAngle :: Last Angular} deriving (Show)


instance Semigroup LabelSettings where
  l1 <> l2 = LabelSettings
    { getLabel = getLabel l1 <> getLabel l2
    , getLabelCorner = getLabelCorner l1 <> getLabelCorner l2
    , getLabelOffset = getLabelOffset l1 <> getLabelOffset l2
    , getLabelPosition = getLabelPosition l1 <> getLabelPosition l2
    , getLabelAngle = getLabelAngle l1 <> getLabelAngle l2 }

instance Monoid LabelSettings where
  mempty = LabelSettings mempty mempty mempty mempty mempty

getLabelOption op = fromJust . getMaybeLabelOption op
getMaybeLabelOption op = getLast . op . (labelDefaults <> labelSettings)

------------------------------------------------------------

data Style = Style
  { getStroke :: Last String
  , getFill :: Last String
  , getDashing :: Last String
  , getStrokeWidth :: Last String } deriving (Show)

instance Semigroup Style where
  l1 <> l2 = Style
    { getStroke = getStroke l1 <> getStroke l2
    , getFill = getFill l1 <> getFill l2
    , getDashing = getDashing l1 <> getDashing l2
    , getStrokeWidth = getStrokeWidth l1 <> getStrokeWidth l2}

instance Monoid Style where
  mempty = Style mempty mempty mempty mempty

getStyleOption op
  = getLast . op . (styleDefaults <> style)
  
------------------------------------------------------------
type Options = (LabelSettings, Style)

newtype Decorated a = Decorated (Options, a)
  deriving Functor

instance Applicative Decorated where
  pure p = Decorated (mempty, p)
  (<*>) = ap

instance Monad Decorated where
  Decorated (d, x) >>= f =
    let Decorated (d', y) = f x
    in Decorated (d <> d', y)

fromDecorated (Decorated (_, x)) = x


class Decor a where
  options :: a -> Options
  options p = (labelDefaults p, styleDefaults p)

  setOptions :: Options -> a -> a
  setOptions _ = id

  labelDefaults :: a -> LabelSettings
  labelDefaults _ = mempty

  styleDefaults :: a -> Style
  styleDefaults _ = mempty


setLabel :: Decor a => LabelSettings -> a -> a
setLabel lb = setOptions (lb, mempty)

setStyle :: Decor a => Style -> a -> a
setStyle s = setOptions (mempty, s)
  
labelSettings :: Decor a => a -> LabelSettings
labelSettings = fst . options

style :: Decor a => a -> Style
style = snd . options

labelText :: Decor a => a -> Maybe String
labelText = getMaybeLabelOption getLabel

labelPosition :: Decor a => a -> CN
labelPosition = getLabelOption getLabelPosition
  
labelOffset :: Decor a => a -> XY
labelOffset = getLabelOption getLabelOffset
  
labelCorner :: Decor a => a -> (Int, Int)
labelCorner f = let (x, y) = labelOffset f
                  in (signum (round x), signum (round y))

labelAngle :: Decor a => a -> Angular
labelAngle = getLabelOption getLabelAngle

label :: Decor a => String -> a -> Decorated a
label s d = setLabel o (pure d)
   where o = (labelSettings d) { getLabel = pure s }

loffs :: Decor a => XY -> a -> Decorated a
loffs o f = setLabel ld (pure f)
   where ld = (labelSettings f) { getLabelOffset = pure o}

lpos :: (Affine p, Decor a) => p -> a -> Decorated a
lpos x f = setLabel ld (pure f)
   where ld = (labelSettings f) { getLabelPosition = pure (cmp x) }

lparam :: (Curve a, Decor a) => Double -> a -> Decorated a
lparam x f = lpos (f .@ x) f
  
------------------------------------------------------------
  
instance Decor (Decorated a) where
  options (Decorated (o, _)) = o
  setOptions o' f = Decorated (o', id) <*> f

instance Show a => Show (Decorated a) where
  show f = l <> show (fromDecorated f)
    where l = fromMaybe mempty $ (<> ":") <$> labelText f

instance Eq a => Eq (Decorated a) where
  d1 == d2 = fromDecorated d1 == fromDecorated d2

instance Affine a => Affine (Decorated a) where
  cmp = cmp . fromDecorated
  fromCN = pure . fromCN

instance Trans a => Trans (Decorated a) where
  transform t = fmap (transform t)

instance Curve a => Curve (Decorated a) where
  param = param . fromDecorated
  locus = locus . fromDecorated
  tangent = tangent . fromDecorated
  isContaining = isContaining . fromDecorated
  isEnclosing = isEnclosing . fromDecorated
  distanceTo = distanceTo . fromDecorated


instance Figure a => Figure (Decorated a) where
  isTrivial = isTrivial . fromDecorated
  refPoint = refPoint . fromDecorated

------------------------------------------------------------

stroke :: Decor a => String -> a -> Decorated a
stroke s f = setStyle ld (pure f)
  where ld = (style f) { getStroke = pure s}

white :: Decor a => a -> Decorated a
white = stroke "white"

fill :: Decor a => String -> a -> Decorated a
fill s f = setStyle ld (pure f)
  where ld = (style f) { getFill = pure s}

width :: Decor a => String -> a -> Decorated a
width s f = setStyle ld (pure f)
  where ld = (style f) { getStrokeWidth = pure s}

thin :: Decor a => a -> Decorated a
thin = width "1"

dashed :: Decor a => a -> Decorated a
dashed f = setStyle ld (pure f)
  where ld = (style f) { getDashing = pure "5,5"}

dotted :: Decor a => a -> Decorated a
dotted f = setStyle ld (pure f)
  where ld = (style f) { getDashing = pure "2,3"}
