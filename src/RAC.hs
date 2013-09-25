{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module RAC where

import Language.C.Quote.ObjC
import Language.C hiding (Func)
import Data.Loc
import Data.String
import Backend
import OM

import System.IO.Unsafe

rac_textSignal :: TypeLC t => (Bind t TTrue NSTextField) -> OM (Bind Local TTrue (RACSignal NSString))
rac_textSignal x = mkOM $ Bind (unsafePerformIO fresh)
                        $ FFunctionE [cexp|[$id:x rac_textSignal]|] (FRACSignal)
