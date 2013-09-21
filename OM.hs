{-# LANGUAGE DataKinds, GADTs, KindSignatures, ExistentialQuantification, TypeOperators, RankNTypes, TypeFamilies #-}
-- Objective-C Monad
module OM where

import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative

-- Semantics container
data OM a = OM a (TypeList Local) (TypeList Global)

-- kind
data Ty = NSBool | NSInteger | NSString | NSArray Ty
        | (~>) Ty Ty

-- kind
data TypeListType = Local | Global

-- data TypeList (t :: TypeListType) = TypeListNil
--               | forall (a :: Ty) . TypeListCons (Bind a) (TypeList t)
data family TypeList :: TypeListType -> *
data instance TypeList Local
  = TypeListNilL
  | forall (a :: Ty) . TypeListConsL (Bind Local a) (TypeList Local)
data instance TypeList Global
  = TypeListNilG
  | forall (a :: Ty) . TypeListConsG (Bind Global a) (TypeList Global)

-- Binding
data family Bind :: TypeListType -> Ty -> *
data instance Bind Local a where
  Bind :: String -> Func a -> Bind Local a -- bound var
  NoBind :: Func a -> Bind Local a -- no bound var
data instance Bind Global a where
  GBind :: String -> Func a -> Bind Global a -- global / class property var

-- Ty constructors
data Func (a :: Ty) where
  FBool :: Bool -> Func NSBool
  FInt :: Int -> Func NSInteger
  FFunction :: String -> Func (a ~> b)

-- Monad
instance Functor OM where
  fmap = liftM

instance Applicative OM where
  pure = return
  (<*>) = ap

instance Monad OM where
  return x = OM x typeListNil typeListNil
  (OM x ls gs) >>= f = let (OM x' ls' gs') = f x
                       in OM x' (typeListAppend ls ls') (typeListAppend gs gs')

-- type list type class
class TypeLC (t :: TypeListType) where
  typeListNil :: TypeList t
  typeListCons :: forall (a :: Ty) . (Bind t a) -> TypeList t -> TypeList t
  typeListAppend :: TypeList t -> TypeList t -> TypeList t
  typeListMap :: (forall (a :: Ty) . Bind t a -> b) -> TypeList t -> [b]
  nameOf :: Bind t a -> String

instance TypeLC Local where
  typeListNil = TypeListNilL
  typeListCons = TypeListConsL
  typeListAppend TypeListNilL xs = xs
  typeListAppend (TypeListConsL v xs) ys = TypeListConsL v (typeListAppend xs ys)
  typeListMap _ TypeListNilL = []
  typeListMap f (TypeListConsL x xs) = (f x) : (typeListMap f xs)
  nameOf (Bind x _) = x
  nameOf (NoBind _) = error "taking name of unnamed binding"

instance TypeLC Global where
  typeListNil = TypeListNilG
  typeListCons = TypeListConsG
  typeListAppend TypeListNilG xs = xs
  typeListAppend (TypeListConsG v xs) ys = TypeListConsG v (typeListAppend xs ys)
  typeListMap _ TypeListNilG = []
  typeListMap f (TypeListConsG x xs) = (f x) : (typeListMap f xs)
  nameOf (GBind x _) = x
