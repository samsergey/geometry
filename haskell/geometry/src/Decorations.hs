{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
module Decorations where

import Base
import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.String( IsString(..) )

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
  asCmp = pure . asCmp

instance Trans a => Trans (Decorated a) where
  transform t = fmap (transform t)

instance Curve a => Curve (Decorated a) where
  param = param . fromDecorated
  project = project . fromDecorated
  tangent = tangent . fromDecorated
  isContaining = isContaining . fromDecorated
  isEnclosing = isEnclosing . fromDecorated
  distanceTo = distanceTo . fromDecorated


instance Figure a => Figure (Decorated a) where
  isTrivial = isTrivial . fromDecorated
  refPoint = refPoint . fromDecorated

------------------------------------------------------------
newtype Decorator a = Decorator (a -> Decorated a)

instance Semigroup (Decorator a) where
  Decorator a <> Decorator b = Decorator (a >=> b)
  
instance Monoid (Decorator a) where
  mempty = Decorator $ \a -> Decorated (mempty, a)

instance Decor a => IsString (Decorator a) where
  fromString = label

infixl 5 #:
(#:) :: Decor a => a -> Decorator a -> Decorated a
a #: (Decorator d) = d a

stroke :: Decor a => String -> Decorator a
stroke s = Decorator $ \f ->
  let ld = (style f) { getStroke = pure s}
  in setStyle ld (pure f)

white :: Decor a => Decorator a
white = stroke "white"

fill :: Decor a => String -> Decorator a
fill s = Decorator $ \f ->
  let ld = (style f) { getFill = pure s}
  in setStyle ld (pure f)

width :: Decor a => String -> Decorator a
width s = Decorator $ \f ->
  let ld = (style f) { getStrokeWidth = pure s}
  in setStyle ld (pure f)

thin :: Decor a => Decorator a
thin = width "1"

dashed :: Decor a => Decorator a
dashed = Decorator $ \f ->
  let ld = (style f) { getDashing = pure "5,5"}
  in setStyle ld (pure f)

dotted :: Decor a => Decorator a
dotted = Decorator $ \f ->
  let ld = (style f) { getDashing = pure "2,3"}
  in setStyle ld (pure f)

label :: Decor a => String -> Decorator a
label s = Decorator $ \d -> 
   let o = (labelSettings d) { getLabel = pure s }
   in setLabel o (pure d)

loffs :: Decor a => XY -> Decorator a
loffs o = Decorator $ \f -> 
   let ld = (labelSettings f) { getLabelOffset = pure o}
   in setLabel ld (pure f)

lpos :: (Affine p, Decor a) => p -> Decorator a
lpos x = Decorator $ \f ->
   let ld = (labelSettings f) { getLabelPosition = pure (cmp x) }
   in setLabel ld (pure f)

lparam :: (Curve a, Decor a) => Double -> Decorator a
lparam x = Decorator $ \f -> f #: lpos (f @-> x)
  
