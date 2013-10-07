{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module NSObject where

import Language.C.Quote.ObjC
import Language.C hiding (Func)
import Data.Loc
import Data.String
import Backend
import OM

import System.IO.Unsafe

-- Polymorphic "new"
new :: DefTypes t => OM (Bind Local TTrue t)
new = mkOM $ Bind (unsafePerformIO fresh) (fst defaultType)

-- polymorphic new property
-- TODO: 2 classes. DefTypes with just type. DefTypeE with default implementation
newProp :: DefTypes t => OM (Bind Global TTrue t)
newProp = do
  let n = unsafePerformIO fresh
      (dty, din) = defaultType
  addOM (NoBind (FFunction [cexp|self.$id:n = $din|])) -- local
  mkOM $ Bind n dty -- global

newInteger :: Int -> OM (Bind Local TTrue NSInteger)
newInteger x = mkOM $ Bind (unsafePerformIO fresh) (FInteger x)

printDescription :: TypeLC t => Bind t TTrue d -> OM ()
printDescription s = addOM (NoBind (FFunction [cexp|NSLog("%@", $id:s)|]))

-- assign the first object to the 2nd
assign :: TypeLC s1 => TypeLC s2 => Bind s1 TTrue t -> Bind s2 TTrue t -> OM ()
assign x y = addOM (NoBind (FFunction [cexp|$id:x = $id:y|]))

-- these are valid expressions (no OM required / pure)
instance ToExp (Func CGRect) where
  toExp CGRectZero _ = [cexp|CGRectZero|]

instance ToExp (Func (RACSignal CGRect)) where
  toExp (RCLBox d) _ = [cexp|RCLBox($d)|]