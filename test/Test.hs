{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Control.Monad.Stack
import Control.Monad.Stack.State
import Control.Monad.Stack.Error
import Control.Monad.Stack.List
import Control.Monad.Stack.Writer
import Control.Monad.Stack.Reader

example :: ( MonadStateT  Int     (Stack ts)
           , MonadErrorT  String  (Stack ts)
           , MonadStateT  Int     st
           , MonadErrorT  String  et
           , Ordered '[st, et] ts
            ) => Stack ts IO ()

example = do
  throw @String "Hello"

example' :: Stack '[WriterT [String], ReaderT Int, StateT Int, ReaderT Bool, ErrorT String] IO ()
example' = example @_ @(StateT Int) @(ErrorT String)

main :: IO ()
main = do
  x <-   runStack
       . popErrorT
       . popReaderT True
       . popStateT 3
       . popReaderT 5 
       . popWriterT $ example'
  print x
