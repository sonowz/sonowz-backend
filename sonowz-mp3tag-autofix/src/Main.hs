module Main where

import Sonowz.Mp3tagAutofix.AudioTag.Types
import Sonowz.Mp3tagAutofix.AudioTagIO.Effect
import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib

main :: IO ()
main =
  (do
      tag <- readAudioTag "./test.mp4"
      let tag' = tag { title = mkTitle $ unTitle (title tag) <> "x" }
      writeAudioTag tag'
    )
    & runAudioTagIOIO
    & runError
    & fmap (fromRight (error "asdf"))
    & runM
