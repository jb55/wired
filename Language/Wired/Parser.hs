{-# LANGUAGE ViewPatterns #-}

module Language.Wired.Parser (
    parseElem
  , getRaw
  , getElem
) where

import           Language.Wired.Types
import           Control.Lens
import           Control.Monad (guard)
import           Data.Vector ((!?))
import qualified Data.ByteString.Char8 as B
import           Control.Monad.Error (throwError)

loc :: Int -> Int -> Loc
loc x y = (x, y)

parseElem :: Char -> Parser Elem
parseElem 'o'  = return $ Start
parseElem '-'  = return $ Line Horizontal
parseElem '|'  = return $ Line Vertical
parseElem '/'  = return $ Line UpRight
parseElem '\\' = return $ Line UpLeft
parseElem ' '  = return $ Empty
parseElem c = do
  l <- use pLoc
  throwError $ InvalidChar c l

getRaw' :: CircuitData -> Int -> Int -> Maybe Char
getRaw' (getCircuit -> cd) x y = do
  line <- cd !? y
  guard (x < B.length line)
  return $ line `B.index` x

getRaw :: CircuitData -> Int -> Int -> Parser (Maybe Char)
getRaw cd x y = do
  mc <- return $ getRaw' cd x y
  pLoc .= loc x y
  return mc

getElem :: CircuitData -> Int -> Int -> Parser Elem
getElem cd x y = do
  raw <- getRaw cd x y
  e   <- maybe (return TheVoid) parseElem raw
  pElem .= e
  return e

