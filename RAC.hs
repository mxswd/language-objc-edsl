{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell #-}
module RAC where

import Data.List
import Data.Monoid
import Control.Monad.State
import System.IO.Unsafe
import Data.Unique
import Control.Applicative
import Language.C.Quote.ObjC
import Language.C
import Data.Loc
import Data.String

-- Foreign Kind
data FK = FKRect | FKSize | FKTuple FK FK

data RACSignal :: FK -> * where
  RACSigSize      :: Exp -> RACSignal FKSize
  Rcl_frameSignal :: Id -> RACSignal FKRect
  RCLBox          :: Double -> RACSignal FKRect

rac_sigsize :: Id -> RACSignal FKSize
rac_sigsize (Id x l) = RACSigSize $ Var (Id x l) l

instance ToExp Id where
  toExp (Id x l) = Var (Id x l)

instance IsString Id where
  fromString x = Id x noLoc

instance ToExp (RACSignal a) where
  toExp (RACSigSize x) l = x
  toExp (Rcl_frameSignal x) _ = [cexp|[$x rcl_frameSignal]|]
  toExp (RCLBox d) _ = [cexp|RCLBox($d)|]

data CGRect = CGRectZero | CGRectMake String
instance ToExp CGRect where
  toExp CGRectZero _ = [cexp|CGRectZero()|]

data Bind = forall a . Bind Id (RACSignal a)
          | forall a . BindTuple Id Id (RACSignal a, RACSignal a)

data RAC a = RAC a [Bind]

mkBlock :: Bind -> BlockItem
mkBlock (Bind s x) = [citem|typename RACSignal *$id:s = $x;|]
mkBlock (BindTuple a b (x, _)) = BlockStm [cstm|RACTupleUnpack(RACSignal *$a, RACSignal *$b) = $x;|]

instance Functor RAC where
  fmap = liftM

instance Applicative RAC where
  pure = return
  (<*>) = ap

instance Monad RAC where
  return x = RAC x []
  (RAC x bs) >>= f = let (RAC v bs') = f x
                     in RAC v (bs ++ bs')

fresh :: RACSignal a -> RAC Id
fresh x = let f = "f_" <> (show . hashUnique . unsafePerformIO $ newUnique)
          in RAC (Id f noLoc) [Bind (Id f noLoc) x]

fresh2 :: RACSignal a -> RAC (Id, Id)
fresh2 x = let (f, g) = ("f_" <> (show . hashUnique . unsafePerformIO $ newUnique), "f_" <> (show . hashUnique . unsafePerformIO $ newUnique))
           in RAC (Id f noLoc, Id g noLoc) [BindTuple (Id f noLoc) (Id g noLoc) (x, x)]

