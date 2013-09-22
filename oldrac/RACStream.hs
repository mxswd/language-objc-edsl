{-# LANGUAGE DataKinds, RankNTypes, TypeOperators, GADTs, KindSignatures, FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, QuasiQuotes #-}
module RACStream where

import RAC
import qualified Prelude as P
import Language.C.Quote.ObjC
import Language.C.Syntax

skip :: P.Int -> (RACSignal a) -> RAC (RACSignal a)
skip i x = RAC (RACBind [cexp|[$x skip:$i]|]) []

map :: RACSignal (a ~> b) -> (RACSignal a) -> RAC (RACSignal b)
map (RACFunction f) x = RAC b []
  where
    b = RACBind [cexp|[$x map:$f]|]

concat :: RACSignal a -> RACSignal a -> RAC (RACSignal a)
concat a b = RAC (b) []
