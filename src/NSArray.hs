{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module NSArray where

import Language.C.Quote.ObjC
import Language.C hiding (Func)
import Data.Loc
import Data.String
import Backend
import OM

import System.IO.Unsafe

arrayHead :: TypeLC t => DefTypes d => Bind t TTrue (NSArray d) -> OM (Bind Local TTrue d)
arrayHead xs = let n = unsafePerformIO fresh
  in mkOM (Bind n (FFunctionE [cexp|[$id:xs firstObject]|] (fst defaultType)))

insertObject :: TypeLC t1 => TypeLC t2 => Bind t1 TTrue d -> Bind t2 TTrue (NSArray d) -> OM ()
insertObject x xs = addOM (NoBind (
  FFunction [cexp|[$id:xs addObject:$id:x]|]))
