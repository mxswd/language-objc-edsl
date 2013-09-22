{-# LANGUAGE DataKinds, GADTs, KindSignatures, ExistentialQuantification, TypeOperators, RankNTypes, TypeFamilies #-}
-- Objective-C Monad
module OM -- monad
          ( OM, mkOM, addOM, runOM
          -- type class TypeLC
          , TypeList, TypeLC, typeListMap
          -- kinds
          , TypeListType(..), TBool(..), Ty(..)
          -- types
          , Func(..), Bind(..)
          ) where

import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative
import Language.C (Exp) -- for FFunction to contain a Exp

-- Semantics container
data OM a = OM a (TypeList Local) (TypeList Global)

runOM :: OM t -> (TypeList Local -> t1) -> (TypeList Global -> t2) -> (t1, t2)
runOM (OM _ ls gs) f g = let
  ls' = f ls
  gs' = g gs
  in (ls', gs')

-- kind
-- TODO: open type family? so importers can add types?
data Ty = NSBool | NSInteger | NSString | NSArray Ty
        -- gui
        | NSTextField
        -- Types
        | (~>) Ty Ty
        | NSUnit -- "void"

-- kind
data TypeListType = Local | Global

-- kind
data TBool = TTrue | TFalse

-- Ty constructors
-- TODO: open data family? so importers can add types?
data Func (a :: Ty) where
  FBool :: Bool -> Func NSBool
  FInt :: Int -> Func NSInteger
  FArray :: Func (NSArray a)
  FFunction :: Exp -> Func (a ~> b)
  FFunctionE :: Exp -> Func (a)
  FUnit :: Func NSUnit
  FTextField :: Func NSTextField

-- A list for bindings
data family TypeList :: TypeListType -> *
data instance TypeList Local
  = TypeListNilL
  | forall (a :: Ty) (n :: TBool) . TypeListConsL (Bind Local n a) (TypeList Local)
data instance TypeList Global
  = TypeListNilG
  | forall (a :: Ty) . TypeListConsG (Bind Global TTrue a) (TypeList Global)

-- Binding
-- If you can bind it (TTrue), then it can be any a.
-- If you can not (TFalse), then it is local and NSUnit (void).
data Bind :: TypeListType -> TBool -> Ty -> * where
  Bind :: String -> Func a -> Bind t TTrue a -- bound var
  NoBind :: Func (a ~> b) -> Bind Local TFalse NSUnit -- no bound var, must be "function"

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

-- Locally add a NoBind
addOM :: forall (a :: Ty) . (Bind Local TFalse a) -> OM ()
addOM f = OM () (TypeListConsL f typeListNil) typeListNil

-- type list type class
class TypeLC (t :: TypeListType) where
  typeListNil :: TypeList t
  typeListAppend :: TypeList t -> TypeList t -> TypeList t
  typeListMap :: (forall (a :: Ty) (n :: TBool) . Bind t n a -> b) -> TypeList t -> [b]
  nameOf :: Bind t TTrue a -> String
  mkOM :: forall (a :: Ty) . (Bind t TTrue a) -> OM (Bind t TTrue a)

instance TypeLC Local where
  typeListNil = TypeListNilL
  typeListAppend TypeListNilL xs = xs
  typeListAppend (TypeListConsL v xs) ys = TypeListConsL v (typeListAppend xs ys)
  typeListMap _ TypeListNilL = []
  typeListMap f (TypeListConsL x xs) = (f x) : (typeListMap f xs)
  nameOf (Bind x _) = x
  mkOM f = OM f (TypeListConsL f typeListNil) typeListNil

instance TypeLC Global where
  typeListNil = TypeListNilG
  typeListAppend TypeListNilG xs = xs
  typeListAppend (TypeListConsG v xs) ys = TypeListConsG v (typeListAppend xs ys)
  typeListMap _ TypeListNilG = []
  typeListMap f (TypeListConsG x xs) = (f x) : (typeListMap f xs)
  nameOf (Bind x _) = x
  mkOM f = OM f typeListNil (TypeListConsG f typeListNil)
