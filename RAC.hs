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
        -- views
        | FKTextField | FKScrollView

data RACSignal :: FK -> * where
  RACSigSize      :: Exp -> RACSignal FKSize
  Rcl_frameSignal :: Exp -> RACSignal FKRect
  RCLBox          :: Double -> RACSignal FKRect
  -- views
  RACTextField    :: [Stm] -> RACSignal FKTextField
  RACScrollView   :: [Stm] -> RACSignal FKScrollView

rac_sigsize :: Exp -> RACSignal FKSize
rac_sigsize = RACSigSize

instance ToExp Id where
  toExp (Id x l) = Var (Id x l)

instance IsString Id where
  fromString x = Id x noLoc

instance IsString Exp where
  -- TODO: this is really limited
  fromString x = case fromMethod x of
                  ([], x') -> Var (Id x' noLoc) noLoc
                  (s, c) -> Member (Var (Id s noLoc) noLoc) (Id c noLoc) noLoc

fromMethod x = case elemIndex '.' x of
                  Nothing -> ([], x)
                  Just i  -> let (s, _:c) = splitAt i x in (s, c)

instance ToExp (RACSignal a) where
  -- TODO: line numbers?
  toExp (RACSigSize x) _ = x
  toExp (Rcl_frameSignal x) _ = [cexp|[$x rcl_frameSignal]|]
  toExp (RCLBox d) _ = [cexp|RCLBox($d)|]
  toExp (RACTextField s) l = StmExpr (map BlockStm s) l
  toExp (RACScrollView s) l = StmExpr (map BlockStm s) l

-- Binding

data Bind = forall a . Bind Id (RACSignal a) -- a reference
          | forall a . BindTuple Id Id (RACSignal a, RACSignal a) -- a tuple
          | forall a . Bindless (RACSignal a) -- no binding
          | forall a . PropertyBind ObjCIfaceDecl (RACSignal a) -- binding an @property

mkBlock :: Bind -> BlockItem
mkBlock (Bind s x) = [citem|typename RACSignal *$id:s = $x;|]
mkBlock (BindTuple a b (x, _)) = BlockStm [cstm|RACTupleUnpack(RACSignal *$a, RACSignal *$b) = $x;|]
mkBlock (Bindless s) = [citem|$s;|]
mkBlock (PropertyBind _ s) = [citem|$s;|]

mkProperty :: Bind -> Maybe ObjCIfaceDecl
mkProperty (PropertyBind s _) = Just s
mkProperty _ = Nothing

-- RAC monad

data RAC a = RAC a [Bind]

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
