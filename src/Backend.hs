{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}
module Backend where
  
import OM

import Language.C.Quote.ObjC
import Language.C.Quote.Base
import Language.C hiding (Func)
import Text.PrettyPrint.Mainland
import Data.Loc
import Data.String
-- fresh
import Data.Unique

instance IsString Id where
  fromString x = Id x noLoc

-- this should be poly-kinded so defaultType can take arguments
class DefTypes (a :: Ty) where
  -- fst is a default value, for pattern matching for a type
  -- snd is an expression to initialize objects (NULL doesn't count)
  defaultType :: (Func a, Exp)

instance DefTypes (NSArray t) where
  defaultType = (FArray, [cexp|[[NSArray alloc] init]|])

instance DefTypes NSInteger where
  defaultType = (FInteger 0, error "nsinteger deftype unimplemented")

instance DefTypes NSString where
  defaultType = (FString "", error "nsstring deftype unimplemented")

-- idents
instance TypeLC t => ToIdent (Bind t TTrue t1) where
  toIdent x l = Id (nameOf x) l

codeGen :: OM a -> (Stm, [ObjCIfaceDecl])
codeGen pr = let
  f' :: Bind Local n a1 -> [BlockItem]
  f' = mkBlocks
  g' :: Bind Global n a1 -> [ObjCIfaceDecl]
  g' = mkProps
  res = runOM pr f' g'
  fst' = flip Block noLoc $ fst res
  in (fst', snd res)

-- makes blockitems (local bindings)
mkBlocks :: Bind Local n a -> [BlockItem]
mkBlocks (Bind x (FBool True)) = [[citem|typename NSNumber *$id:x = @(YES);|]]
mkBlocks (Bind x (FBool False)) = [[citem|typename NSNumber *$id:x = @(NO);|]]
mkBlocks (Bind x (FInteger b)) = [[citem|typename NSNumber *$id:x = @($b);|]]
mkBlocks (Bind x (FArray)) = [[citem|typename NSArray *$id:x = [[NSArray alloc] init];|]]
mkBlocks (Bind x (FString s)) = [[citem|typename NSString *$id:x = @$string:s;|]]
mkBlocks (Bind x (FFunction s)) = [[citem|$id:x = $s;|]]
mkBlocks (Bind x (FFunctionE s ty)) = let typ = typename ty
  in [[citem|typename $id:typ $id:x = $s;|]]
mkBlocks (BindTuple x1 x2 (FFunctionT s t)) = [[citem|RACTupleUnpack(RACSignal *$id:x1 , RACSignal *$id:x2) = $s;|]]
mkBlocks (NoBind (FFunction s)) = [BlockStm (Exp (Just s) noLoc)]
mkBlocks (NoBind (FBlockItem s)) = [s]

typename :: Func a -> Id
typename (FBool _) = "NSNumber *"
typename (FArray) = "NSArray *"
typename (FInteger _) = "NSNumber *"
typename (FRACSignal) = "RACSignal *"
typename (FNSTextView) = "NSTextView *"

-- makes property decls (global bindings)
mkProps :: Bind Global n a -> [ObjCIfaceDecl]
mkProps (Bind x (FArray)) = [mkProp "NSArray" x]
mkProps (Bind x (FTextField)) = [mkProp "NSTextField" x]
mkProps (Bind x (FScrollView)) = [mkProp "NSScrollView" x]
mkProps (Bind x (FNSView)) = [mkProp "NSView" x]
mkProps (Bind x (FNSTextView)) = [mkProp "NSTextView" x]

-- Take a type to make, and an identifier for it.
mkProp :: Id -> String -> ObjCIfaceDecl
mkProp t s' = ObjCIfaceProp [ObjCStrong noLoc]
              (FieldGroup (DeclSpec [] []
              (Tnamed t [] noLoc) noLoc) [q] noLoc) noLoc
    where
      q = Field (Just (Id (s') noLoc))
                (Just (Ptr [] (DeclRoot noLoc) noLoc)) (Nothing) noLoc

-- instance (TypeLC s2 , ABool b) => ToExp (Bind s2 b f) where
  -- true so use the identifier
  -- false use constant
  -- toExp x _ = [cexp|$id:x|]

-- instance TypeLC s2 => ToExp (Bind s2 TFalse f) where
--   -- true so use the identifier
--   -- false use constant
--   toExp x _ = undefined

-- instance ToExp (Bind s2 fu f) where
  
fresh = fmap (("f_" ++) . show . hashUnique) newUnique
