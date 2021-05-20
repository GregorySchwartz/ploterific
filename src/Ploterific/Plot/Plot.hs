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
import Data.Colour.Palette.BrewerSet ( brewerSet, ColorCat (..) )
import Data.Colour.Palette.Harmony (colorRamp)
import Data.Colour.SRGB (sRGB24show)
import Data.Either (rights)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import GHC.Natural (intToNatural)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv.Streaming as CSVStream
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
                  -> Maybe Facet
                  -> [Feature]
                  -> [Map.Map T.Text T.Text]
                  -> [VL.DataColumn]
                  -> VL.Data
rowsToDataColumns color facet fs rows =
  VL.dataFromColumns []
    . maybe
        id
        (\ (Color x)
        -> VL.dataColumn
            (getColName x)
            (numOrString (textToString x) x rows)
        )
        color
    . maybe
        id
        (\ (Facet x)
        -> VL.dataColumn
            (getColName x)
            (numOrString (textToString x) x rows)
        )
        facet
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
                    . fmap fst
                    . rights
                    . fmap ( maybe
                              ( error
                              $ "Column not in table: " <> show (getColName x))
                              T.double
                           . Map.lookup (getColName x)
                           )
    textToString x = VL.Strings . fmap (fromMaybe "" . Map.lookup (getColName x))

-- | Get color encoding for color ramps.
labelColorScale :: [ColorLabel] -> VL.MarkChannel
labelColorScale cs = VL.MScale [ VL.SDomain (VL.DStrings labels)
                               , VL.SRange (VL.RStrings colors)
                               ]
  where
    labels =
      Set.toAscList . Set.fromList . fmap unColorLabel $ cs
    colors =
      fmap (T.pack . sRGB24show) . colorRamp (length labels) . brewerSet Set1 $ 9

-- | Get the encoding.
enc :: Maybe (Color, [ColorLabel])
    -> [Feature]
    -> [VL.EncodingSpec]
    -> VL.PropertySpec
enc colorInfo fs =
  VL.encoding
    . maybe
        id
        (\ (Color c, ls)
        -> VL.color ( [VL.MName . getColName $ c, labelColorScale ls]
                   <> maybe [] (\x -> [VL.MmType x]) (getColMeasurement c)
                    )
        )
        colorInfo
    . VL.tooltips
        ( fmap (\ (Feature !f)
               -> [ VL.TName . getColName $ f ]
                 <> maybe [] (\x -> [VL.TmType x]) (getColMeasurement f)
               )
          fs
       <> [ maybe
              []
              (\ (Color c, _)
              -> [VL.TName . getColName $ c]
              <> maybe [] (\x -> [VL.TmType x]) (getColMeasurement c)
              )
              colorInfo
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
  facet' <- asks _facet
  facetNum' <- asks _facetNum
  delimiter' <- asks _delimiter
  defaultTheme' <- asks _defaultTheme

  contents <- liftIO input'

  let rows = loadCsv delimiter' contents
      dataSet =
        rowsToDataColumns color' facet' features' $ rows
      colorLabels =
        fmap
          (\ (Color c)
          -> fmap (ColorLabel . fromMaybe "" . Map.lookup (getColName c)) rows
          )
          color'
      colorInfo = (,) <$> color' <*> colorLabels
      facetSpec (Facet x) = [ VL.facetFlow
                            $ [VL.FName . getColName $ x]
                           <> maybe [] (\y -> [VL.FmType y]) (getColMeasurement x)
                            ]
      plotSpec = [ enc colorInfo features' []
                 , VL.mark mark' []
                 , VL.selection . VL.select "view" VL.Interval [VL.BindScales]
                 $ []
                 ]
      p       = VL.toVegaLite
              $   [ dataSet []
                  ]
                 <> bool plotSpec [VL.specification . VL.asSpec $ plotSpec] (isJust facet')
                 <> maybe [] (\(Height x) -> [VL.height x]) height'
                 <> maybe [] (\(Width x) -> [VL.width x]) width'
                 <> maybe [] facetSpec facet'
                 <> maybe [] (\(FacetNum x) -> [VL.columns $ intToNatural x]) facetNum'
                 <> bool [VL.theme VL.defaultConfig []] [] (unDefaultTheme defaultTheme')

  liftIO . output' $ VL.toHtml p

  return ()
