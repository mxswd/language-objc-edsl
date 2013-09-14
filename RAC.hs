{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, RankNTypes, TypeOperators, TypeFamilies, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, FlexibleContexts #-}
module RAC where

import Data.List
import Data.Monoid
import Control.Monad.State
import System.IO.Unsafe
import Data.Unique
import Control.Applicative

-- Foreign Kind
data FK = FKRect | FKSize | FKUnit | FKTuple FK FK
        | (~>) FK FK | FKBool | FKInt

data RACSignal :: FK -> * where
  RACSigSize      :: String -> RACSignal FKSize
  Rcl_frameSignal :: String -> RACSignal FKRect
  RCLBox          :: Double -> RACSignal FKRect
  RACFunction     :: (RACSignal a -> RACSignal b) -> RACSignal (a ~> b)
  RACBool         :: Bool -> RACSignal FKBool

-- Well, you can define it ;P
apply :: RACSignal (a ~> b) -> RACSignal a -> RACSignal b
apply (RACFunction f) v = f v

instance Show (RACSignal a) where
  show (RACSigSize x) = x
  show (Rcl_frameSignal x) = x <> ".rcl_frameSignal"
  show (RCLBox d) = "RCLBox(" <> show d <> ")"
  show (RACFunction f) = "* Function *"

data CGRect = CGRectZero | CGRectMake String deriving Show

type Var = String
data Bind = forall a . Bind Var (RACSignal a)
          | forall a . BindTuple Var Var (RACSignal a, RACSignal a)

data RAC a = RAC a [Bind]

instance Show Bind where
  show (Bind s _) = "RACSignal *" <> s
  show (BindTuple x y _) = "RACTupleUnpack(RACSignal *" <> x <> ", RACSignal *" <> y <>")"
  showList x = flip (++) $ unlines (map showCode x)

showCode :: Bind -> String
showCode b@(Bind _ x) = show b <> " = " <> show x <> ";\n"
showCode b@(BindTuple _ _ (x, _)) = show b <> " = " <> show x <> ";\n"

instance Functor RAC where
  fmap = liftM

instance Applicative RAC where
  pure = return
  (<*>) = ap

instance Monad RAC where
  return x = RAC x []
  (RAC x bs) >>= f = let (RAC v bs') = f x
                     in RAC v (bs ++ bs')

class RACT a where
  rep :: a -> Bind

fresh :: RACSignal a -> RAC Var
fresh x = let f = "f_" <> (show . hashUnique . unsafePerformIO $ newUnique)
          in RAC f [Bind f x]

fresh2 :: RACSignal a -> RAC (Var, Var)
fresh2 x = let (f, g) = ("f_" <> (show . hashUnique . unsafePerformIO $ newUnique), "f_" <> (show . hashUnique . unsafePerformIO $ newUnique))
           in RAC (f, g) [BindTuple f g (x, x)]

