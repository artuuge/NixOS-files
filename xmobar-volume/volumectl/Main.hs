module Main where

import Control.Applicative (liftA2)
import Control.Monad (unless)

import Data.Attoparsec.Text (decimal, parseOnly)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)

import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.FilePath.Posix ((</>))
import System.IO (hFlush, hGetLine, hPutStrLn, IOMode (ReadMode, WriteMode), stdout, withFile)
import System.Posix.Process (executeFile)

----------

type Volume = Int
type Mute = Int

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just -- % Data.Either.Combinators

parseInt :: String -> Either String Int
parseInt = (parseOnly decimal) . decodeUtf8 . fromString 

getIntMay :: FilePath -> IO (Maybe Int)
getIntMay fp = withFile fp ReadMode $ \h -> 
  rightToMaybe . parseInt <$> hGetLine h

putInt :: Int -> FilePath -> IO ()
putInt a fp = withFile fp WriteMode $ \h -> do
  hPutStrLn h $ show a

rectifyVolume :: Volume -> Volume
rectifyVolume v = case v < 0 of
  True -> 0
  False -> case v > 90000 of
    True -> 90000
    False -> v

getVolumeMay :: FilePath -> IO (Maybe Volume)
getVolumeMay = getIntMay

setVolume :: String -> Volume -> FilePath -> IO ()
setVolume pactl volume fp = do
  putInt volume fp
  executeFile pactl True ["set-sink-volume", "0", show volume] Nothing

modifyVolume :: String -> Volume -> FilePath -> IO ()
modifyVolume pactl delta fp = do
  mv <- getVolumeMay fp
  let mu = fmap (rectifyVolume . (+ delta)) mv
  case mu of
    Nothing -> return ()
    Just u -> setVolume pactl u fp

getMuteMay :: FilePath -> IO (Maybe Mute)
getMuteMay = getIntMay
    
setMute :: String -> Mute -> FilePath -> IO ()
setMute pactl mute fq = do
  putInt mute fq
  executeFile pactl True ["set-sink-mute", "0", show mute] Nothing

changeVolume :: String -> Volume -> FilePath -> FilePath -> IO ()
changeVolume pactl delta fp fq = do
  mm <- getMuteMay fq
  case mm of
    Just 0 -> modifyVolume pactl delta fp
    _ -> return ()

repVolume :: Volume -> String
repVolume volume =
  let
    v = rectifyVolume volume
    (w, u) = ( v `div` 9000, (v `mod` 9000) `div` 3000)
    m = (max 0) . (min 10) $ w
    n = (max 0) . (min 2) $ u
    xs = [' ', '.', ':']
  in
    case (m, n) of
      (10, _) -> "[" ++ (replicate 10 '|') ++ "]"
      _ -> "[" ++ (replicate m '|') ++ [(xs !! n)] ++ (replicate (9 - m) ' ') ++ "]"

repVolumeMute :: Volume -> Mute -> String
repVolumeMute volume mute =
  case mute of
    0 -> repVolume volume
    _ -> "[  (mute)  ]"

----------

main :: IO ()
main = do

  home <- fromMaybe "./" <$> lookupEnv "HOME"
  let fp = home </> ".volume"
      fq = home </> ".mute"

  pactl <- fromMaybe "pactl" <$> lookupEnv "NIX_PACTL"
  
  bp <- doesFileExist fp
  unless bp $ do
    writeFile fp "42000"
    setVolume pactl 42000 fp
    
  bq <- doesFileExist fq
  unless bq $ do
    writeFile fq "0"
    setMute pactl 0 fq
    
  args <- getArgs
  case args of
    [] -> do
      mr <- liftA2 repVolumeMute <$> getVolumeMay fp <*> getMuteMay fq
      case mr of
        Nothing -> return ()
        Just r -> putStr r >> hFlush stdout
    [ "increase" ] -> changeVolume pactl 3000 fp fq
    [ "decrease" ] -> changeVolume pactl (-3000) fp fq
    [ "mute" ] -> setMute pactl 1 fq
    [ "unmute" ] -> setMute pactl 0 fq
    _ -> return ()          

----------
