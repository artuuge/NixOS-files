{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom

submitName :: MonadWidget t m => m (Event t String) 
submitName = do
  rec t <- textInput $ def & setValue .~ fmap (\_ -> "") e
      e <- button "Submit"
  return $ ffilter (/= "") $ tag (current (value t)) e

sayHello :: MonadWidget t m => Event t String -> m ()
sayHello e = do
  let h = fmap (\xs -> "Hello, " ++ xs ++ ".") e 
  d <- holdDyn "" h 
  dynText d

main :: IO ()
main = mainWidget $
  el "div" $ do
    e <- el "div" $ submitName
    el "div" $ sayHello e
