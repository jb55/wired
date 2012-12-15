{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Wired.Types (
    -- | * Parser types
    ParserState(..)
  , ParseError(..)
  , Parser(..)
  , Located(..)
  , Loc

    -- | * Type classes
  , IsLocated(..)

    -- | * Misc visualization
  , Pad(..)

    -- | * Parser state lenses
  , pLoc
  , pElem
  , pData

    -- | * Circuit types
  , CircuitData(..)
  , Orientation(..)
  , Fork
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

type Fork = [Located Elem]

toChar :: Elem -> Char
toChar (Line Horizontal) = '-'
toChar (Line SlantRight)    = '/'
toChar (Line SlantLeft)     = '\\'
toChar Empty             = ' '
toChar Socket            = '>'
toChar TheVoid           = 'X'
toChar _                 = '?'

instance Show Pad where
  show (Pad tl t tr l m r bl b br) =
    unlines [ ""
      , toChar tl:toChar t:toChar tr:[]
      , toChar l :toChar m:toChar r :[]
      , toChar bl:toChar b:toChar br:[]
    ]

data Orientation = Horizontal
                 | Vertical
                 | SlantRight
                 | SlantLeft
                 deriving (Show, Eq, Ord)

data Elem = Start
          | Empty
          | Socket
          | TheVoid
          | C !Char
          | Line !Orientation
          deriving (Show, Eq, Ord)

type Loc = (Int, Int)
data ParserState = ParserState { _pLoc  :: Loc
                               , _pElem :: Elem
                               , _pData :: CircuitData
                               } deriving (Show)

class IsLocated a where
  locOf :: a -> Loc

data Located a = Located Loc a
               deriving (Show, Eq, Ord)

instance IsLocated (Located a) where
  locOf (Located l _) = l

$(makeLenses ''ParserState)

instance IsLocated ParserState where
  locOf ps = ps^.pLoc

data ParseError = InvalidChar Char Loc
                | OutOfBounds Loc
                | Failure String Loc
                deriving (Show, Eq, Ord)

peLoc :: ParseError -> Loc
peLoc (InvalidChar _ l) = l
peLoc (OutOfBounds  l)  = l
peLoc (Failure _ l)     = l

instance Error ParseError where
  noMsg = InvalidChar '\0' (0, 0)

newtype Parser a = Parser { runParser :: StateT ParserState (Either ParseError) a }
                 deriving ( Monad, Functor, MonadError ParseError
                          , MonadState ParserState, Applicative, Alternative
                          )

