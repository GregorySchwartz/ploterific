{- Ploterific.Program.Options
Gregory W. Schwartz

Options for the command line program.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ploterific.Program.Options where

-- Remote
import Options.Generic
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite as VL

-- Local
import Ploterific.Plot.Types

-- | Command line arguments
data Options
    = Options { input :: Maybe String <?> "([STDIN] | PATH) The path for the input data in tabular format, with row observations and column features."
              , output :: Maybe String <?> "([STDOUT] | PATH) HTML output path."
              , feature :: [String] <?> "(COLUMN) A list of columns to use in the plot in the format --feature col1 --feature col2 etc. for axes X, Y, etc. To force a measurement, add `:` followed by `N`, `O`, `Q`, or `T` for nominal, ordinal, quantitiative, or temporal measurements, respectively. Full list of measurements located at https://vega.github.io/vega-lite/docs/type.html."
              , color :: Maybe String <?> "(NOTHING | COLUMN) The column containing a feature to use for colors. Add measurement according to --feature if needed."
              , measurement :: [String] <?> "(Nothing | MEASUREMENT) Specify the level of measurement for each --feature in order. For instance, --feature col1 --feature col2 --measurement quantitative --measurement nominal. "
              , mark :: String <?> "(MARK) The mark type for the plot. Common types are Circle, Bar, Boxplot, and Line. See https://hackage.haskell.org/package/hvega-0.11.0.1/docs/Graphics-Vega-VegaLite.html#t:Mark for a list."
              , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the table."
              , height :: Maybe Double <?> "(Nothing | DOUBLE) The height of the plot."
              , width :: Maybe Double <?> "(Nothing | Double) The width of the plot."
              } deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "input"       = Just 'i'
    short "output"      = Just 'o'
    short "feature"     = Just 'f'
    short "color"       = Just 'c'
    short "mark"        = Just 'm'
    short "delimiter"   = Just 'd'
    short "height"      = Just 'h'
    short "width"       = Just 'w'
    short x             = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers
