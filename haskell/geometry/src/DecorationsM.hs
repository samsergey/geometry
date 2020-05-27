{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
module DecorationsM where

import Data.Monoid

import Base

data Option = Stroke String
            | Fill String
            | Thickness Double
            | MultyStroke Int
            | Dashing String
            | LabelText String
            | LabelPosition CN
            | LabelOffset CN
            | LabelAngle Angular
            | SegmentMark Int
  deriving (Show)

newtype Options = Options {getOptions :: (Dual [Option]) }
  deriving (Semigroup, Monoid, Show)

mkOptions = Options . Dual


-- | The class of objects which could have decorations.
class Decor a where
  -- | Get the decoration data
  options :: a -> Options
  options p = defaultOptions p

  -- | Set the decoration data
  setOptions :: Options -> a -> a
  setOptions _ = id

  -- | Default labeling settings
  defaultOptions :: a -> Options
  defaultOptions _ = mempty

find :: Decor a => (Option -> Maybe b) -> a -> Maybe b
find p d = getFirst . foldMap (First . p) . getDual . getOptions
           $ options d

getFill :: Decor a => a -> Maybe String
getFill = find $ \case {Fill x -> Just x; _ -> Nothing }
--getStroke = find $ \case {Stroke x -> Just x; _ -> Nothing }
--getThickness = find $ \case {Thickness x -> Just x; _ -> Nothing }
--getMultyStroke = find $ \case {MultyStroke x -> Just x; _ -> Nothing }
--getDashing = find $ \case {Dashing x -> Just x; _ -> Nothing }
--getLabelText = find $ \case {LabelText x -> Just x; _ -> Nothing }
--getLabelPosition = find $ \case {LabelPosition x -> Just x; _ -> Nothing }
--getLabelOffset = find $ \case {LabelOffset x -> Just x; _ -> Nothing }
--getLabelAngle = find $ \case {LabelAngle x -> Just x; _ -> Nothing }
--getSegmentMark = find $ \case {SegmentMark x -> Just x; _ -> Nothing }
