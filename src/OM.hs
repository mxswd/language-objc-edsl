{-# LANGUAGE DataKinds, GADTs, KindSignatures, ExistentialQuantification, TypeOperators, RankNTypes, TypeFamilies #-}
-- Objective-C Monad
module OM
          ( OM, mkOM, addOM, runOM
          -- type class TypeLC
          , TypeLC, nameOf
          -- kinds
          , TypeListType(..), TBool(..), Ty(..)
          -- types
          , Func(..), Bind(..)
          ) where

import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative
import Language.C (Exp, BlockItem) -- for FFunction to contain an Exp

-- Semantics container
data OM a = OM a (TypeList Local) (TypeList Global)

runOM :: OM t
      -> (forall (a :: Ty) (n :: TBool) . Bind Local n a -> [t1])
      -> (forall (a :: Ty) (n :: TBool) . Bind Global n a -> [t2])
      -> ([t1], [t2])
runOM (OM _ ls gs) f g = let
  ls' = typeListMap f ls
  gs' = typeListMap g gs
  in (concat ls', concat gs')

-- kind
-- TODO: open type family? so importers can add types?
data Ty = NSBool | NSInteger | NSDouble | NSString | NSArray Ty
        -- Types
        | (~>) Ty Ty
        | NSUnit -- "void"
        -- UIKit
        | NSView
        | NSTextField
        | NSTextView
        | NSScrollView
        -- CG
        | CGAlpha
        | CGSize
        | CGRect
        -- RAC
        | RACSignal Ty
        | RACTuple Ty Ty

-- kind
data TypeListType = Local | Global

-- kind
data TBool = TTrue | TFalse

-- Ty constructors
-- TODO: open data family? so importers can add types?
data Func (a :: Ty) where
  FBool :: Bool -> Func NSBool
  FInteger :: Int -> Func NSInteger
  FDouble :: Double -> Func NSDouble
  FString :: String -> Func NSString
  FArray :: Func (NSArray a)
  FUnit :: Func NSUnit

  -- Not sure about these types yet
  FFunction :: Exp -> Func (a ~> b)
  FBlockItem :: BlockItem -> Func a -- some side effecting code
  FFunctionE :: Exp -> Func a -> Func a

  -- CG
  FCGAlpha :: Double -> Func NSDouble
  FCGSize :: (Int, Int) -> Func CGSize
  FCGRect :: (Int, Int, Int, Int) -> Func CGRect
  CGRectZero :: Func CGRect
  
  -- UIKit
  FTextField :: Func NSTextField
  FScrollView :: Func NSScrollView
  FNSTextView :: Func NSTextView
  FNSView :: Func NSView
  -- RAC
  FRACSignal :: Func (RACSignal a)
  FRACTuple :: Func (RACSignal a) -> Func (RACSignal b) -> Func (RACTuple (RACSignal a) (RACSignal b))
  FFunctionT :: Exp -> Func (RACTuple (RACSignal a) (RACSignal b)) -> Func (RACTuple (RACSignal a) (RACSignal b)) -- rac tuple return type
  
  -- RCL
  RCLBox :: Double -> Func (RACSignal CGRect)
  

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
  BindTuple :: String -> String -> Func (RACTuple a b) -> Bind t TTrue (RACTuple a b) -- RAC only
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
  nameOf (Bind x _) = "self." ++ x
  mkOM f = OM f typeListNil (TypeListConsG f typeListNil)
