{- Ploterific.Plot.Plot
Gregory W. Schwartz

Collects the functions pertaining to the plotting of figures.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ploterific.Plot.Plot
  ( plot
  ) where

-- Remote
import Control.Monad.Reader (ReaderT (..), asks, liftIO)
import Data.Bool (bool)
import Data.Char (ord)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv.Streaming as CSVStream
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Read as T
import qualified Graphics.Vega.VegaLite as VL
import qualified Graphics.Vega.VegaLite.Theme as VL

-- Local
import Ploterific.Plot.Types

-- | Split the measurement of a feature.
splitColMeasure :: T.Text -> Either T.Text (T.Text, VL.Measurement)
splitColMeasure feature = splitOrNot . T.breakOnEnd ":" $ feature
  where
    splitOrNot ("", _) = Left feature
    splitOrNot x = Right $ trueSplit x
    trueSplit = L.over L._1 (T.dropEnd 1) . L.over L._2 toMeasurement
    toMeasurement "N" = VL.Nominal
    toMeasurement "O" = VL.Ordinal
    toMeasurement "Q" = VL.Quantitative
    toMeasurement "T" = VL.Temporal

-- | Get the column name even with the measurement.
getColName :: T.Text -> T.Text
getColName = either id fst . splitColMeasure

-- | Get the measurement.
getColMeasurement :: T.Text -> Maybe VL.Measurement
getColMeasurement = either (const Nothing) (Just . snd) . splitColMeasure

-- | Parse rows of a CSV.
loadCsv :: Delimiter -> BL.ByteString -> [Map.Map T.Text T.Text]
loadCsv (Delimiter d) =
  either error (F.toList . snd)
    . CSVStream.decodeByNameWith ( CSV.defaultDecodeOptions
                                 { CSV.decDelimiter = fromIntegral (ord d) }
                                 )


-- | Convert rows to data columns.
rowsToDataColumns :: Maybe Color
                  -> [Feature]
                  -> [Map.Map T.Text T.Text]
                  -> [VL.DataColumn]
                  -> VL.Data
rowsToDataColumns color fs rows =
  VL.dataFromColumns []
    . maybe
        id
        (\ (Color c)
        -> VL.dataColumn
            (getColName c)
            (numOrString (textToString c) c rows)
        )
        color
    . foldl'
        (\ acc (Feature x)
        -> VL.dataColumn
            (getColName x)
            (numOrString (textToNumbers x) x rows)
         . acc
        )
        id
        fs
  where
    numOrString :: ([Map.Map T.Text T.Text] -> VL.DataValues) -> T.Text -> ([Map.Map T.Text T.Text] -> VL.DataValues)
    numOrString def x =
      maybe
          def
          (bool (textToString x) (textToNumbers x) . (== VL.Quantitative))
        . getColMeasurement
        $ x
    textToNumbers x = VL.Numbers
                    . fmap ( maybe
                              (error "Can not parse number in column")
                              (either error fst . T.double)
                           . Map.lookup (getColName x)
                           )
    textToString x = VL.Strings . fmap (fromMaybe "" . Map.lookup (getColName x))

-- | Get the encoding.
enc :: Maybe Color -> [Feature] -> [VL.EncodingSpec] -> VL.PropertySpec
enc color fs =
  VL.encoding
    . maybe
        id
        (\ (Color c)
        -> VL.color ( [VL.MName . getColName $ c]
                   <> maybe [] (\x -> [VL.MmType x]) (getColMeasurement c)
                    )
        )
        color
    . VL.tooltips
        ( fmap (\ (Feature !f)
               -> [ VL.TName . getColName $ f ]
                 <> maybe [] (\x -> [VL.TmType x]) (getColMeasurement f)
               )
          fs
       <> [ maybe
              []
              (\ (Color c)
              -> [VL.TName . getColName $ c]
              <> maybe [] (\x -> [VL.TmType x]) (getColMeasurement c)
              )
              color
          ]
        )
    . foldl'
        (\ acc (!p, Feature !f)
        -> VL.position
            p
            ( [VL.PName . getColName $ f]
             <> maybe [] (\x -> [VL.PmType x]) (getColMeasurement f)
            )
         . acc
        )
        id
        (zip pos fs)
  where
    pos = [VL.X, VL.Y]

-- | Render plot.
plot :: ReaderT Opts IO ()
plot = do
  input' <- asks (maybe BL.getContents (BL.readFile . unInput) . _input)
  output' <- asks (maybe TL.putStrLn (TL.writeFile . unOutput) . _output)
  height' <- asks _height
  width' <- asks _width
  mark' <- asks _mark
  color' <- asks _color
  features' <- asks _features
  delimiter' <- asks _delimiter

  contents <- liftIO input'

  let dataSet =
        rowsToDataColumns color' features' . loadCsv delimiter' $ contents
      p       = VL.toVegaLite
              $   [ dataSet []
                  , enc color' features' []
                  , VL.mark mark' []
                  , VL.theme VL.defaultConfig []
                  , VL.selection . VL.select "view" VL.Interval [VL.BindScales]
                  $ []
                  ]
                 <> maybe [] (\(Height x) -> [VL.height x]) height'
                 <> maybe [] (\(Width x) -> [VL.width x]) width'

  liftIO . output' $ VL.toHtml p

  return ()
