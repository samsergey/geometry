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
  , getStrokeWidth :: Last String
  , isVisible :: Last Bool } deriving (Show)

instance Semigroup Style where
  l1 <> l2 = Style
    { getStroke = getStroke l1 <> getStroke l2
    , getFill = getFill l1 <> getFill l2
    , getDashing = getDashing l1 <> getDashing l2
    , getStrokeWidth = getStrokeWidth l1 <> getStrokeWidth l2
    , isVisible = isVisible l1 <> isVisible l2}

instance Monoid Style where
  mempty = Style mempty mempty mempty mempty mempty

getStyleOption op
  = getLast . op . (styleDefaults <> style)
  
------------------------------------------------------------

type Options = (LabelSettings, Style)

class Decorated a where
  options :: a -> Options
  options p = (labelDefaults p, styleDefaults p)

  setOptions :: Options -> a -> a
  setOptions _ = id

  labelDefaults :: a -> LabelSettings
  labelDefaults _ = mempty

  styleDefaults :: a -> Style
  styleDefaults _ = mempty


setLabel :: Decorated a => LabelSettings -> a -> a
setLabel lb = setOptions (lb, mempty)

setStyle :: Decorated a => Style -> a -> a
setStyle s = setOptions (mempty, s)
  
labelSettings :: Decorated a => a -> LabelSettings
labelSettings = fst . options

style :: Decorated a => a -> Style
style = snd . options

labelText :: Decorated a => a -> Maybe String
labelText = getMaybeLabelOption getLabel

labelPosition :: Decorated a => a -> CN
labelPosition = getLabelOption getLabelPosition
  
labelOffset :: Decorated a => a -> XY
labelOffset = getLabelOption getLabelOffset
  
labelCorner :: Decorated a => a -> (Int, Int)
labelCorner f = let (x, y) = labelOffset f
                  in (signum (round x), signum (round y))

labelAngle :: Decorated a => a -> Angular
labelAngle = getLabelOption getLabelAngle

-- invisible :: Decorated a => a -> a
-- invisible f = setStyle s f
--   where s = (style f) { isVisible = pure False }

label :: Decorated a => String -> a -> a
label s d = setLabel o d
   where o = (labelSettings d) { getLabel = pure s }

-- loffs :: Decorated a => XY -> a -> Decoration a
-- loffs o f = setLabel ld f
--    where ld = (labelSettings f) { getLabelOffset = pure o}

-- lpos :: (Affine p, Decorated a) => p -> a -> Decoration a
-- lpos x f = setLabel ld f
--    where ld = (labelSettings f) { getLabelPosition = pure (cmp x) }

-- lparam :: (Curve a, Figure a) => Double -> a -> a
-- lparam x f = setLabel ld f
--   where ld = (labelSettings f) { getLabelPosition = pure (f .@ x) }
  
------------------------------------------------------------
  
newtype Decoration a = Decoration (Options, a)
  deriving Functor

instance Applicative Decoration where
  pure p = Decoration (mempty, p)
  (<*>) = ap

instance Monad Decoration where
  Decoration (d, x) >>= f =
    let Decoration (d', y) = f x
    in Decoration (d <> d', y)


fromDecoration (Decoration (_, x)) = x


instance Decorated (Decoration a) where
  options (Decoration (o, _)) = o
  setOptions o' f = Decoration (o', id) <*> f


instance Show a => Show (Decoration a) where
  show f = l <> show (fromDecoration f)
    where l = fromMaybe mempty $ (<> ":") <$> labelText f


instance Eq a => Eq (Decoration a) where
  d1 == d2 = fromDecoration d1 == fromDecoration d2


instance Affine a => Affine (Decoration a) where
  cmp = cmp . fromDecoration
  fromCN = pure . fromCN


instance Trans a => Trans (Decoration a) where
  transform t = liftM (transform t)


instance Curve a => Curve (Decoration a) where
  param = param . fromDecoration
  locus = locus . fromDecoration
  tangent = tangent . fromDecoration
  isContaining = isContaining . fromDecoration
  isEnclosing = isEnclosing . fromDecoration
  distanceTo = distanceTo . fromDecoration


instance Figure a => Figure (Decoration a) where
  isTrivial = isTrivial . fromDecoration
  refPoint = refPoint . fromDecoration
