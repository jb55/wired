
module Main (
  main
) where

import           Control.Applicative
import           Control.Monad.State (runStateT)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary (lines, sourceFile)
import           Data.Monoid (Monoid)
import           Data.Monoid (mappend, mempty)
import           Language.Wired
import           Prelude hiding (lines)
import qualified Data.Conduit.List as CL

sinkCircuitData :: Monad m => GSink ByteString m CircuitData
sinkCircuitData = CircuitData <$> (lines >+> buildM)

buildM :: (Applicative f, Monoid (f a), Monad m) => GSink a m (f a)
buildM = CL.fold (\x y -> x `mappend` pure y) mempty

run :: CircuitData -> Parser a -> Either ParseError (a, ParserState)
run cd x = (runStateT . runParser) x (ParserState (0,0) TheVoid cd)

main :: IO ()
main = do
  out <- runResourceT $ sourceFile "test.txt" $$ sinkCircuitData
  print $ run out $ do
    moveTo 3 1
    many followLine *> parseFork
