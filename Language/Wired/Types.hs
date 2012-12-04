{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Wired.Types (
    -- | * Parser types
    ParserState(..)
  , ParseError(..)
  , Parser(..)
  , Loc

    -- | * Misc visualization
  , Pad(..)

    -- | * Parser state lenses
  , pLoc
  , pElem
  , pData

    -- | * Circuit types
  , CircuitData(..)
  , Orientation(..)
  , Elem(..)
) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Error (MonadError, Error(..))
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import           Data.Vector (Vector)

newtype CircuitData = CircuitData { getCircuit :: Vector ByteString }
                    deriving Show

data Pad = Pad !Elem !Elem !Elem
               !Elem !Elem !Elem
               !Elem !Elem !Elem
               deriving (Eq, Ord)

toChar :: Elem -> Char
toChar (Line Horizontal) = '-'
toChar (Line UpRight)    = '/'
toChar (Line UpLeft)     = '\\'
toChar Empty             = '_'
toChar Socket            = '>'
toChar TheVoid           = 'X'
toChar _                 = '?'

instance Show Pad where
  show (Pad tl t tr l m r bl b br) =
    unlines [ ""
      , toChar tl:' ':toChar t:' ':toChar tr:[]
      , toChar l :' ':toChar m:' ':toChar r :[]
      , toChar bl:' ':toChar b:' ':toChar br:[]
    ]

data Orientation = Horizontal
                 | Vertical
                 | UpRight
                 | UpLeft
                 deriving (Show, Eq, Ord)

data Elem = Start
          | Empty
          | Socket
          | TheVoid
          | Line !Orientation
          deriving (Show, Eq, Ord)

type Loc = (Int, Int)
data ParserState = ParserState { _pLoc  :: Loc
                               , _pElem :: Elem
                               , _pData :: CircuitData
                               } deriving (Show)

$(makeLenses ''ParserState)

data ParseError = InvalidChar Char Loc
                | OutOfBounds Loc
                deriving (Show, Eq, Ord)

instance Error ParseError where
  noMsg = InvalidChar '\0' (0, 0)

newtype Parser a = Parser { runParser :: StateT ParserState (Either ParseError) a }
                 deriving ( Monad, Functor, MonadError ParseError
                          , MonadState ParserState, Applicative)

