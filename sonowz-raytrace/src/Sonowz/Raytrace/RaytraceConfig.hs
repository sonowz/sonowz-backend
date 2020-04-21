module Sonowz.Raytrace.RaytraceConfig
  ( Config(..)
  , ConfigResult(..)
  , jsonToConfig
  )
where

import Relude
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

-- Internal JSON decode format
data RTConfig = RTConfig
  { pixelWidth   :: Int
  , pixelHeight  :: Int
  , jittering    :: Bool
  , areaLight    :: Bool
  , antiAliasing :: Bool
  , dofToggle    :: Bool
  , dofAperture  :: Float
  , dofFocus     :: Int
  , sceneNo      :: Int
  } deriving (Eq, Show, Read)

instance FromJSON RTConfig where
  parseJSON = withObject "RTConfig" parseRTConfig

newtype Config = Config Text deriving (Eq, Show, Read) via Text

data ConfigResult =
    ConfigSuccess Config
  | DecodeFail Text
  deriving (Eq, Show, Read)

jsonToConfig :: LByteString -> ConfigResult
jsonToConfig _json = case eitherDecode' _json of
  Left  errormsg -> DecodeFail (toText errormsg)
  Right rtConfig -> ConfigSuccess $ createConfig rtConfig

parseRTConfig :: Aeson.Object -> Aeson.Parser RTConfig
parseRTConfig obj = do
  pixelWidth   <- parseItemRange "pixelWidth" 2 500 obj
  pixelHeight  <- parseItemRange "pixelHeight" 2 500 obj
  jittering    <- parseItem "jittering" obj
  areaLight    <- parseItem "areaLight" obj
  antiAliasing <- parseItem "antiAliasing" obj
  dofToggle    <- parseItem "dofToggle" obj
  dofAperture  <- parseItemRange "dofAperture" 1.0 50.0 obj
  dofFocus     <- parseItemRange "dofFocus" 1 600 obj
  sceneNo      <- parseItemRange "sceneNo" 1 3 obj
  return RTConfig { .. }

parseItemRange :: (FromJSON a, Show a, Ord a) => Text -> a -> a -> Aeson.Object -> Aeson.Parser a
parseItemRange name minValue maxValue obj = do
  value <- obj .: name
  case range name minValue maxValue value of
    Right value'   -> return value'
    Left  errormsg -> fail (toString errormsg)

parseItem :: FromJSON a => Text -> Aeson.Object -> Aeson.Parser a
parseItem = flip (.:)

range :: (Show a, Ord a) => Text -> a -> a -> a -> Either Text a
range name minValue maxValue value = if (minValue <= value) && (value <= maxValue)
  then Right value
  else Left $ name <> " must be in range [" <> show minValue <> ", " <> show maxValue <> "]."

createConfig :: RTConfig -> Config
createConfig conf =
  let
    def opt param enable = mconcat
      [ "\n"
      , if enable then "" else "//"
      , "#define " <> opt <> " "
      , if show param == "()" then "" else show param
      ]
  in
    Config $ mconcat
      [ "#pragma once\n"
      , def "PIXEL_WIDTH"  (pixelWidth conf)  True
      , def "PIXEL_HEIGHT" (pixelHeight conf) True
      , "\n//#define RT_DEBUG"
      , "\n#define RT_MULTITHREAD 4"
      , "\n//#define RT_WINDOWS"
      , def "RT_JITTERING"    ()                 (jittering conf)
      , def "RT_AREA_LIGHT"   ()                 (areaLight conf)
      , def "RT_ANTIALIASING" ()                 (antiAliasing conf)
      , def "RT_DOF"          ()                 (dofToggle conf)
      , def "RT_DOF_APERTURE" (dofAperture conf) (dofToggle conf)
      , def "RT_DOF_FOCUS"    (dofFocus conf)    (dofToggle conf)
      , def "RT_SCENE_NO"     (sceneNo conf)     True
      , "\n"
      ]
