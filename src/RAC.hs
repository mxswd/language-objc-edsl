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
                        $ FFunctionE [cexp|[$id:x rac_textSignal]|] FRACSignal

-- XXX: doesn't have disposables yet
-- need fresh vars for "x"
rac_subscribeNext :: TypeLC t => (Bind t TTrue (RACSignal v)) -> ((Bind Local TTrue v) -> OM ()) -> OM ()
rac_subscribeNext x f = addOM $ NoBind
                              $ FFunction [cexp|[$id:x subscribeNext:^(id x) {$(expu $ fst $ codeGen $ f $ valOf x);}]|]

-- a lambda abstraction. it is always local
valOf :: Bind t TTrue (RACSignal v) -> Bind Local TTrue v
valOf (Bind s _) = Bind "x" undefined

expu x = StmExpr [BlockStm x] noLoc