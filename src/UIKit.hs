{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module UIKit where

import Language.C.Quote.ObjC
import Language.C hiding (Func)
import Data.Loc
import Data.String
import Backend
import OM

import System.IO.Unsafe

-- newArrayProp :: OM (Bind Global TTrue (NSArray t))
-- newArrayProp = do
--   let n = "wow"
--   addOM (NoBind (FFunction [cexp|$id:n = [[NSArray alloc] init]|])) -- global
--   mkOM $ Bind n FArray -- local

newTextField :: OM (Bind Global TTrue (NSTextField))
newTextField = undefined

  -- printString h -- This is a type error!
