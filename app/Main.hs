{- ploterific
Gregory W. Schwartz

Blah's the blah in the blah.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Options.Generic
import Text.Read (readMaybe)
import qualified Data.Text as T

-- Local
import Ploterific.Program.Options
import Ploterific.Plot.Plot
import Ploterific.Plot.Types

main :: IO ()
main = do
  options <- getRecord "ploterific, Gregory W. Schwartz.\
                       \ Quick plotting from the command line."

  let opts = Opts { _color = fmap (Color . T.pack) . unHelpful . color $ options
                  , _features = fmap (Feature . T.pack) . unHelpful . feature $ options
                  , _facet = fmap (Facet . T.pack) . unHelpful . facet $ options
                  , _facetNum = fmap FacetNum . unHelpful . facetNum $ options
                  , _delimiter = Delimiter . fromMaybe ',' . unHelpful . delimiter $ options
                  , _input = fmap Input . unHelpful . input $ options
                  , _output = fmap Output . unHelpful . output $ options
                  , _mark = fromMaybe (error "Cannot parse mark")
                          . readMaybe
                          . unHelpful
                          . mark
                          $ options
                  , _height = fmap Height . unHelpful . height $ options
                  , _width = fmap Width . unHelpful . width $ options
                  }

  runReaderT plot opts

  return ()
