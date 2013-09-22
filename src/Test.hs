{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

import OM
import Backend
import NSObject
import NSArray

import Text.PrettyPrint.Mainland

test = do
  let t = codeGen testP
  putStrLn $ prettyPragma 90 (ppr (snd t))
  putStrLn $ prettyPragma 90 (ppr (fst t))

testP :: OM ()
testP = do
  xs <- newProp
  x <- new :: OM (Bind Local TTrue NSInteger)
  insertObject x xs
  h <- arrayHead xs
  printDescription h
