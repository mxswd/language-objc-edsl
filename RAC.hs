{-# LANGUAGE FlexibleInstances #-}
module RAC where

import Data.List
import Data.Monoid
import Control.Monad.State
import System.IO.Unsafe
import Data.Unique

data RACSignal = RACSignal String
               | Rcl_frameSignal String
               | RCLBox Double

instance Show RACSignal where
  show (RACSignal x) = x
  show (Rcl_frameSignal x) = x <> ".rcl_frameSignal"
  show (RCLBox d) = "RCLBox(" <> show d <> ")"

data CGRect = CGRectZero | CGRectMake String deriving Show

type Var = String
data Bind = Bind Var RACSignal
          | BindTuple Var Var (RACSignal, RACSignal)

data RAC a = RAC a [Bind]

instance Show Bind where
  show (Bind s _) = "RACSignal *" <> s
  show (BindTuple x y _) = "RACTupleUnpack(RACSignal *" <> x <> ", RACSignal *" <> y <>")"
  showList x = flip (++) $ unlines (map showCode x)

showCode :: Bind -> String
showCode b@(Bind _ x) = show b <> " = " <> show x <> ";\n"
showCode b@(BindTuple _ _ (x, _)) = show b <> " = " <> show x <> ";\n"

instance Monad RAC where
  return x = RAC x []
  (RAC x bs) >>= f = let (RAC v bs') = f x
                     in RAC v (bs ++ bs')

class RACT a where
  rep :: a -> Bind

fresh :: RACSignal -> RAC Var
fresh x = let f = "f_" <> (show . hashUnique . unsafePerformIO $ newUnique)
          in RAC f [Bind f x]

fresh2 :: RACSignal -> RAC (Var, Var)
fresh2 x = let (f, g) = ("f_" <> (show . hashUnique . unsafePerformIO $ newUnique), "f_" <> (show . hashUnique . unsafePerformIO $ newUnique))
           in RAC (f, g) [BindTuple f g (x, x)]

