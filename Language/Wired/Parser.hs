{-# LANGUAGE ViewPatterns #-}

module Language.Wired.Parser (
    parseElem
  , parseFork
  , getRaw
  , moveTo
  , parsePad
  , move
  , topleft, top, topright
  , left, middle, right
  , bottomleft, bottom, bottomright
) where

import           Control.Applicative
import           Language.Wired.Types
import           Control.Lens hiding (left, right)
import           Control.Monad (guard, mfilter)
import           Data.Vector ((!?))
import qualified Data.ByteString.Char8 as B
import           Control.Monad.Error (throwError)
import           Data.Maybe (catMaybes)


parseElem :: Char -> Parser Elem
parseElem '-'  = return $ Line Horizontal
parseElem '>'  = return $ Socket
parseElem '/'  = return $ Line UpRight
parseElem '\\' = return $ Line UpLeft
parseElem ' '  = return $ Empty
parseElem c = do
  l <- use pLoc
  throwError $ InvalidChar c l

getRaw' :: CircuitData -> Int -> Int -> Maybe Char
getRaw' (getCircuit -> cd) x y = do
  line <- cd !? y
  guard (x >= 0 && x < B.length line)
  return $ line `B.index` x

getRaw :: Int -> Int -> Parser (Maybe Char)
getRaw x y = do
  cd <- use pData
  mc <- return $ getRaw' cd x y
  pLoc .= (x, y)
  return mc

moveTo :: Int -> Int -> Parser Elem
moveTo x y = do
  raw <- getRaw x y
  e   <- maybe (return TheVoid) parseElem raw
  pElem .= e
  return e

move :: Int -> Int -> Parser Elem
move mx my = do
  (x, y) <- use pLoc
  moveTo (x + mx) (y + my)

topleft, top, topright, left, middle, right, bottomleft, bottom, bottomright
  :: Parser Elem

topleft     = move (-1) (-1)
top         = move 0    (-1)
topright    = move 1    (-1)
left        = move (-1) 0
middle      = move 0    0
right       = move 1    0
bottomleft  = move (-1) 1
bottom      = move 0    1
bottomright = move 1    1

peek :: Parser a -> Parser a
peek p = do
  oloc <- use pLoc
  r    <- p
  pLoc .= oloc
  return r

parsePad :: Parser Pad
parsePad = Pad <$> peek topleft    <*> peek top    <*> peek topright
               <*> peek left       <*> middle      <*> peek right
               <*> peek bottomleft <*> peek bottom <*> peek bottomright

satisfy :: (Elem -> Bool) -> Parser Elem
satisfy pred = do
  e <- middle
  l <- use pLoc
  if pred e
    then return e
    else throwError $ Failure "parse failed in satisfy" l


-- | Valid forks
--       /
--     ----
--       \
--
-- This wouldn't parse
--
--
--
parseFork :: Parser Fork
parseFork = filterP (not . null) $ whateverWorks $ located . peek <$> [
    topright    >> isLine UpRight
  , right       >> isAnyLine
  , bottomright >> isLine UpLeft
  ]

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP pred p = do
  x <- p
  l <- use pLoc
  if pred x
    then return x
    else throwError $ Failure "filterP failed" l

isLine x = satisfy (==Line x)
isAnyLine = satisfy check
  where
    check (Line _) = True
    check _        = False

-- | reify the parsers location into a user data type
located :: Parser a -> Parser (Located a)
located p = do
  r <- p
  l <- use pLoc
  return $ Located l r

whateverWorks :: [Parser a] -> Parser [a]
whateverWorks = fmap catMaybes . sequence . map optional
