{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
module Decorations
  ( -- * Classes
    Decor (..)
    -- * Records and Types
  , Options, Labeling (..), Style (..)
  , Decorated (..), fromDecorated
  -- * Decorators
  , (#:)
  , stroke, white, fill
  , width, thin, dotted, dashed
  , label, loffs, lpos, lparam
  -- * Functions
  , labelText, labelPosition, labelOffset, labelCorner
  )
where

import Base
import Data.Monoid
import Data.Maybe
import Control.Monad
import Data.String( IsString(..) )


-- | The alias for a tuple of decoration records
type Options = (Labeling, Style)

-- | Record representing labeling parameters
data Labeling = Labeling
  { getLabel :: Last String
  , getLabelCorner :: Last (Int, Int)
  , getLabelOffset :: Last XY
  , getLabelPosition :: Last CN
  , getLabelAngle :: Last Angular} deriving (Show)


instance Semigroup Labeling where
  l1 <> l2 = Labeling
    { getLabel = getLabel l1 <> getLabel l2
    , getLabelCorner = getLabelCorner l1 <> getLabelCorner l2
    , getLabelOffset = getLabelOffset l1 <> getLabelOffset l2
    , getLabelPosition = getLabelPosition l1 <> getLabelPosition l2
    , getLabelAngle = getLabelAngle l1 <> getLabelAngle l2 }

instance Monoid Labeling where
  mempty = Labeling mempty mempty mempty mempty mempty

getLabelOption op = fromJust . getMaybeLabelOption op
getMaybeLabelOption op = getLast . op . (labelDefaults <> labelSettings)


-- | Record representing styling parameters
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
  
-- | The transparent decoration wrapper for geometric objects.
-- Inherits all properties of embedded object.
newtype Decorated a = Decorated (Options, a)
  deriving Functor

-- | The selector for the embedded object.
fromDecorated (Decorated (_, x)) = x

instance Applicative Decorated where
  pure p = Decorated (mempty, p)
  (<*>) = ap

instance Monad Decorated where
  Decorated (d, x) >>= f =
    let Decorated (d', y) = f x
    in Decorated (d <> d', y)


-- | The class of objects which could have decorations.
class Decor a where
  -- | Get the decoration data
  options :: a -> Options
  options p = (labelDefaults p, styleDefaults p)

  -- | Set the decoration data
  setOptions :: Options -> a -> a
  setOptions _ = id

  -- | Default labeling settings
  labelDefaults :: a -> Labeling
  labelDefaults _ = mempty

  -- | Default styling settings
  styleDefaults :: a -> Style
  styleDefaults _ = mempty

setLabeling :: Decor a => Labeling -> a -> a
setLabeling lb = setOptions (lb, mempty)

setStyle :: Decor a => Style -> a -> a
setStyle s = setOptions (mempty, s)

-- | Returns labeling settings from a decorated object.
labelSettings :: Decor a => a -> Labeling
labelSettings = fst . options

-- | Returns styling settings from a decorated object.
style :: Decor a => a -> Style
style = snd . options

-- | Returns label text of a decorated object.
labelText :: Decor a => a -> Maybe String
labelText = getMaybeLabelOption getLabel

-- | Returns label position for a decorated object.
labelPosition :: Decor a => a -> CN
labelPosition = getLabelOption getLabelPosition

-- | Returns label offset against the label position.  
labelOffset :: Decor a => a -> XY
labelOffset = getLabelOption getLabelOffset

-- | Returns label corner used as an anchor of a text in SVG.  
labelCorner :: Decor a => a -> (Int, Int)
labelCorner f = let (x, y) = labelOffset f
                  in (signum (round x), signum (round y))

-- | Returns label rotation angle.  
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
-- | A wrapped decoration function with monoidal properties,
-- corresponding to decoration options.
newtype Decorator a = Decorator (a -> Decorated a)

instance Semigroup (Decorator a) where
  Decorator a <> Decorator b = Decorator (a >=> b)
  
instance Monoid (Decorator a) where
  mempty = Decorator $ \a -> Decorated (mempty, a)

instance Decor a => IsString (Decorator a) where
  fromString = label

infixl 5 #:
-- | The infix operator for decorator apprication.
--
-- >>> aPoint # at (4, 5) #: label "A"
-- A:<Point (4.0, 5.0)>
--
-- >>> segment (4,5) (6,9) #: "s" <> dotted <> white
-- s:<Segment (4.0,5.0) (6.0,9.0)>
--
(#:) :: Decor a => a -> Decorator a -> Decorated a
a #: (Decorator d) = d a

-- | The stroke color decorator.
stroke :: Decor a => String -> Decorator a
stroke s = Decorator $ \f ->
  let ld = (style f) { getStroke = pure s}
  in setStyle ld (pure f)

-- | The decorator for white lines.
white :: Decor a => Decorator a
white = stroke "white"

-- | The fill color decorator.
fill :: Decor a => String -> Decorator a
fill s = Decorator $ \f ->
  let ld = (style f) { getFill = pure s}
  in setStyle ld (pure f)

-- | The stroke-width decorator.
width :: Decor a => String -> Decorator a
width s = Decorator $ \f ->
  let ld = (style f) { getStrokeWidth = pure s}
  in setStyle ld (pure f)

-- | The decorator for thin lines.
thin :: Decor a => Decorator a
thin = width "1"

-- | The decorator for dashed lines.
dashed :: Decor a => Decorator a
dashed = Decorator $ \f ->
  let ld = (style f) { getDashing = pure "5,5"}
  in setStyle ld (pure f)

-- | The decorator for dotted lines.
dotted :: Decor a => Decorator a
dotted = Decorator $ \f ->
  let ld = (style f) { getDashing = pure "2,3"}
  in setStyle ld (pure f)

-- | The decorator for labeling objects. Could be used as overloaded string.
--
-- >>> aPoint # at (4, 5) #: "A"
-- A:<Point (4, 5)>
--
label :: Decor a => String -> Decorator a
label s = Decorator $ \d -> 
   let o = (labelSettings d) { getLabel = pure s }
   in setLabeling o (pure d)

-- | The decorator for label offset.
loffs :: Decor a => XY -> Decorator a
loffs o = Decorator $ \f -> 
   let ld = (labelSettings f) { getLabelOffset = pure o}
   in setLabeling ld (pure f)

-- | The decorator for label position.
lpos :: (Affine p, Decor a) => p -> Decorator a
lpos x = Decorator $ \f ->
   let ld = (labelSettings f) { getLabelPosition = pure (cmp x) }
   in setLabeling ld (pure f)

-- | The decorator for setting label on a curve at a given parameter value.
lparam :: (Curve a, Decor a) => Double -> Decorator a
lparam x = Decorator $ \f -> f #: lpos (f @-> x)
  
