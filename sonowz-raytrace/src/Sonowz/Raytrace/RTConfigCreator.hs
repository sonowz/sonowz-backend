module Sonowz.Raytrace.RTConfigCreator (Config(..), jsonToConfig) where

import Relude
import GHC.Generics
import Data.Aeson

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
  } deriving (Generic, Show)

instance FromJSON RTConfig


checkConfigIntegrity :: RTConfig -> Maybe String
checkConfigIntegrity conf =
  bound pixelWidth 2 500 "Pixel width" .
  bound pixelHeight 2 500 "Pixel height" .
  bound dofAperture 1.0 50.0 "Depth of field aperture" .
  bound dofFocus 1 600 "Depth of field focus" .
  bound sceneNo 1 3 "Scene number" $ Nothing where
    bound :: (Show a, Ord a) => (RTConfig -> a) -> a -> a -> String -> Maybe String -> Maybe String
    bound field _min _max text (Just message) = Just message
    bound field _min _max text Nothing =
      if (_min <= field conf) && (field conf <= _max)
        then Nothing
        else Just $ text ++ " must be in range [" ++ show _min ++ ", " ++ show _max ++ "]."


data Config =
    Config String
  | DecodeFail
  | IntegrityFail String

jsonToConfig :: LByteString -> Config
jsonToConfig _json =
  case decode _json of
    Nothing -> DecodeFail
    Just rtConfig ->
      case checkConfigIntegrity rtConfig of
        Just errorMessage -> IntegrityFail errorMessage
        Nothing -> Config $ createConfig rtConfig

createConfig :: RTConfig -> String
createConfig conf =
  let 
    def opt param enable =
      "\n" ++ (if enable then "" else "//") ++
      "#define " ++ opt ++ " " ++ (if show param == "()" then "" else show param) in
  "#pragma once" ++
  "\n" ++
  def "PIXEL_WIDTH" (pixelWidth conf) True ++
  def "PIXEL_HEIGHT" (pixelHeight conf) True ++
  "\n//#define RT_DEBUG" ++
  "\n#define RT_MULTITHREAD 4" ++
  "\n//#define RT_WINDOWS" ++
  def "RT_JITTERING" () (jittering conf) ++
  def "RT_AREA_LIGHT" () (areaLight conf) ++
  def "RT_ANTIALIASING" () (antiAliasing conf) ++
  def "RT_DOF" () (dofToggle conf) ++
  def "RT_DOF_APERTURE" (dofAperture conf) (dofToggle conf) ++
  def "RT_DOF_FOCUS" (dofFocus conf) (dofToggle conf) ++
  def "RT_SCENE_NO" (sceneNo conf) True ++ "\n"
