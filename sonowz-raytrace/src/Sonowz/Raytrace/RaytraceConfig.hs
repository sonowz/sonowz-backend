module Sonowz.Raytrace.RaytraceConfig
  ( Config (..),
    ConfigResult (..),
    jsonToConfig,
  )
where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Relude

-- Internal JSON decode format
data RTConfig = RTConfig
  { width :: Int,
    height :: Int,
    jittering :: Bool,
    areaLight :: Bool,
    antiAliasing :: Bool,
    toggle :: Bool,
    aperture :: Float,
    focus :: Int,
    sceneNo :: Int
  }
  deriving (Eq, Show, Read)

instance FromJSON RTConfig where
  parseJSON = withObject "RTConfig" parseRTConfig

newtype Config = Config Text deriving (Eq, Show, Read) via Text

data ConfigResult
  = ConfigSuccess Config
  | DecodeFail Text
  deriving (Eq, Show, Read)

jsonToConfig :: LByteString -> ConfigResult
jsonToConfig _json = case eitherDecode' _json of
  Left errormsg -> DecodeFail (toText errormsg)
  Right rtConfig -> ConfigSuccess $ createConfig rtConfig

parseRTConfig :: Aeson.Object -> Aeson.Parser RTConfig
parseRTConfig obj = do
  width <- parseItemRange "pixelWidth" 2 500 obj
  height <- parseItemRange "pixelHeight" 2 500 obj
  jittering <- parseItem "jittering" obj
  areaLight <- parseItem "areaLight" obj
  antiAliasing <- parseItem "antiAliasing" obj
  toggle <- parseItem "dofToggle" obj
  aperture <- parseItemRange "dofAperture" 1.0 50.0 obj
  focus <- parseItemRange "dofFocus" 1 600 obj
  sceneNo <- parseItemRange "sceneNo" 1 3 obj
  return RTConfig {..}

parseItemRange :: (FromJSON a, Show a, Ord a) => Key -> a -> a -> Aeson.Object -> Aeson.Parser a
parseItemRange name minValue maxValue obj = do
  value <- obj .: name
  case range name minValue maxValue value of
    Right value' -> return value'
    Left errormsg -> fail (toString errormsg)

parseItem :: FromJSON a => Key -> Aeson.Object -> Aeson.Parser a
parseItem = flip (.:)

range :: (Show a, Ord a) => Key -> a -> a -> a -> Either Text a
range name minValue maxValue value =
  if (minValue <= value) && (value <= maxValue)
    then Right value
    else Left $ show name <> " must be in range [" <> show minValue <> ", " <> show maxValue <> "]."

createConfig :: RTConfig -> Config
createConfig conf =
  let def opt param enable =
        mconcat
          [ "\n",
            if enable then "" else "//",
            "#define " <> opt <> " ",
            if show param == "()" then "" else show param
          ]
   in Config $
        mconcat
          [ "#pragma once\n",
            def "PIXEL_WIDTH" conf.width True,
            def "PIXEL_HEIGHT" conf.height True,
            "\n//#define RT_DEBUG",
            "\n#define RT_MULTITHREAD 4",
            "\n//#define RT_WINDOWS",
            def "RT_JITTERING" () conf.jittering,
            def "RT_AREA_LIGHT" () conf.areaLight,
            def "RT_ANTIALIASING" () conf.antiAliasing,
            def "RT_DOF" () conf.toggle,
            def "RT_DOF_APERTURE" conf.aperture conf.toggle,
            def "RT_DOF_FOCUS" conf.focus conf.toggle,
            def "RT_SCENE_NO" conf.sceneNo True,
            "\n"
          ]
