{- Ploterific.Plot.Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ploterific.Plot.Types where

-- Remote
import Options.Generic
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite as VL

-- Local


-- Basic
newtype Color = Color { unColor :: T.Text } deriving (Read, Show)
newtype Feature = Feature { unFeature :: T.Text } deriving (Read, Show)
newtype Facet = Facet { unFacet :: T.Text } deriving (Read, Show)
newtype FacetNum = FacetNum { unFacetNum :: Int } deriving (Read, Show)
newtype Delimiter = Delimiter { unDelimiter :: Char } deriving (Read, Show)
newtype Input = Input { unInput :: String } deriving (Read, Show)
newtype Output = Output { unOutput :: String } deriving (Read, Show)
newtype Height = Height { unHeight :: Double } deriving (Read, Show)
newtype Width = Width { unWidth :: Double } deriving (Read, Show)

-- Advanced
data Opts = Opts { _color :: Maybe Color
                 , _features :: [Feature]
                 , _facet :: Maybe Facet
                 , _facetNum :: Maybe FacetNum
                 , _delimiter :: Delimiter
                 , _input :: Maybe Input
                 , _output :: Maybe Output
                 , _mark :: VL.Mark
                 , _height :: Maybe Height
                 , _width :: Maybe Width
                 }

deriving instance Read VL.Mark
deriving instance Eq VL.Measurement
