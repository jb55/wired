
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

run :: Parser a -> Either ParseError (a, ParserState)
run x = (runStateT . runParser) x (ParserState (0,0) Empty)

main :: IO ()
main = do
  out <- runResourceT $ sourceFile "test.txt" $$ sinkCircuitData
  let ge = getElem out
  print $ run $ do
    a <- ge 1 1
    b <- ge 1 0
    c <- ge 50 50
    return (a, b, c)
  print out
