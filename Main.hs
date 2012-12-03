{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
  main
) where

import           Prelude hiding (lines)
import           Data.Vector
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Binary (lines, sourceFile)
import           Control.Applicative
import           Data.Monoid (mappend, mempty)
import           Data.Monoid (Monoid)
import           Control.Monad (guard, (<=<))
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Lens

newtype CircuitData = CircuitData { getCircuit :: Vector ByteString }
                    deriving Show

data Orientation = Horizontal
                 | Vertical
                 | UpRight
                 | UpLeft
                 deriving (Show, Eq, Ord)

data Elem = Start
          | Empty
          | TheVoid
          | Line !Orientation
          deriving (Show, Eq, Ord)

type Loc = (Int, Int)
data ParserState = ParserState { _pLoc  :: Loc
                               , _pElem :: Elem
                               } deriving (Show, Eq, Ord)

$(makeLenses ''ParserState)

data ParseError = InvalidChar Char Loc
                | OutOfBounds Loc
                deriving (Show, Eq, Ord)

instance Error ParseError where
  noMsg = InvalidChar '\0' (0, 0)

newtype Parser a = Parser { runParser :: StateT ParserState (Either ParseError) a }
                 deriving (Monad, Functor, MonadError ParseError, MonadState ParserState)

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

sinkCircuitData :: Monad m => GSink ByteString m CircuitData
sinkCircuitData = CircuitData <$> (lines >+> buildM)

buildM :: (Applicative f, Monoid (f a), Monad m) => GSink a m (f a)
buildM = CL.fold (\x y -> x `mappend` pure y) mempty

run x = (runStateT . runParser) x (ParserState (0,0) Empty)

main = do
  out <- runResourceT $ sourceFile "test.txt" $$ sinkCircuitData
  let ge = getElem out
  print $ run $ do
    a <- ge 1 1
    b <- ge 1 0
    c <- ge 50 50
    return (a, b, c)
  print out
