{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Wired.Types (
    -- | * Parser types
    ParserState(..)
  , ParseError(..)
  , Parser(..)
  , Loc
  , pLoc
  , pElem

    -- | * Circuit types
  , CircuitData(..)
  , Orientation(..)
  , Elem(..)
) where

import           Control.Lens
import           Data.ByteString.Char8 (ByteString)
import           Data.Vector (Vector)
import           Control.Monad.State
import           Control.Monad.Error (MonadError, Error(..))

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

