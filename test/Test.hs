module Main (main) where

import Control.Monad.Stack
import Control.Monad.Stack.State
import Control.Monad.Stack.Reader
import Control.Monad.Stack.Writer
import Control.Monad.Stack.List

example :: ( MonadWriterT [String] (Stack ts)
           , MonadReaderT Int      (Stack ts)
           , MonadStateT  Int     (Stack ts)
           , Ordered '[ReaderT Int, StateT Int] ts
            ) => Stack ts IO ()

example = do
  tell @[String] ["Hello"]
  i <- ask @Int
  put @Int 5


example' = example @ ('[WriterT [String], ReaderT Int, StateT Int])

main :: IO ()
main = putStrLn "Test suite not yet implemented."
