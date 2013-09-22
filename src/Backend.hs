{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

import OM

import Language.C.Quote.ObjC
import Language.C hiding (Func)
import Text.PrettyPrint.Mainland
import Data.Loc
import Data.String

instance IsString Id where
  fromString x = Id x noLoc

class DefTypes (a :: Ty) where
  defaultType :: (Func a, Exp)

instance DefTypes (NSArray t) where
  defaultType = (FArray, [cexp|[[NSArray alloc] init]|])

instance DefTypes NSInteger where
  defaultType = (FInt 0, undefined)

  
  -- newBool >>= flip insertBool xs
  -- p_xs <- newProp


pr :: OM ()
pr = do
  xs <- new
  x <- newInteger 4
  insertObject x xs
  h <- arrayHead xs
  printNum h
  
  -- printString h -- This is a type error!

newInteger :: Int -> OM (Bind Local TTrue NSInteger)
newInteger x = mkOM $ Bind "f1" (FInt x)

new :: DefTypes t => OM (Bind Local TTrue t)
new = mkOM $ Bind "f2" (fst defaultType)

printNum :: TypeLC t => Bind t TTrue NSInteger -> OM ()
printNum (Bind s _) = addOM (NoBind (FFunction [cexp|NSLog("%@", $id:s)|]))

printString :: TypeLC t => Bind t TTrue NSString -> OM ()
printString (Bind s _) = addOM (NoBind (FFunction [cexp|NSLog("%@", $id:s)|]))

arrayHead :: DefTypes t => Bind t1 TTrue (NSArray t) -> OM (Bind Local TTrue t)
arrayHead (Bind xs _) = let n = "f3"
  in mkOM (Bind n (FFunctionE [cexp|[$id:xs firstObject]|] (fst defaultType)))

newProp :: DefTypes t => OM (Bind Global TTrue t)
newProp = do
  let n = "wow"
      (dty, din) = defaultType
  addOM (NoBind (FFunction [cexp|$id:n = $din|])) -- local
  mkOM $ Bind n dty -- global

insertObject :: Bind t1 TTrue t -> Bind t2 TTrue (NSArray t) -> OM ()
insertObject (Bind x _) (Bind xs _) = addOM (NoBind (
  FFunction [cexp|[$id:xs addObject:$id:x]|]))

-- newArrayProp :: OM (Bind Global TTrue (NSArray t))
-- newArrayProp = do
--   let n = "wow"
--   addOM (NoBind (FFunction [cexp|$id:n = [[NSArray alloc] init]|])) -- global
--   mkOM $ Bind n FArray -- local

newTextField :: OM (Bind Global TTrue (NSTextField))
newTextField = undefined

test :: ([BlockItem], [ObjCIfaceDecl])
test = runOM pr f' g'
test' = do
  putStrLn $ prettyPragma 90 (ppr (snd test))
  putStrLn $ prettyPragma 90 (ppr (fst test))

f' :: TypeList Local -> [BlockItem]
f' = concat . typeListMap mkBlocks
g' :: TypeList 'Global -> [ObjCIfaceDecl]
g' = concat . typeListMap mkProps

-- makes blockitems (local bindings)
mkBlocks :: Bind Local n a -> [BlockItem]
mkBlocks (Bind x (FBool True)) = [[citem|typename NSNumber *$id:x = @(YES);|]]
mkBlocks (Bind x (FBool False)) = [[citem|typename NSNumber *$id:x = @(NO);|]]
mkBlocks (Bind x (FInt b)) = [[citem|typename NSNumber *$id:x = @($b);|]]
mkBlocks (Bind x (FArray)) = [
    [citem|typename NSArray *$id:x = [[NSArray alloc] init];|]
    ]
mkBlocks (Bind x (FFunction s)) = [[citem|$id:x = $s;|]]
mkBlocks (Bind x (FFunctionE s ty)) = let typ = typename ty
  in [[citem|typename $id:typ $id:x = $s;|]]
mkBlocks (NoBind (FFunction s)) = [BlockStm (Exp (Just s) noLoc)]

typename :: Func a -> String
typename (FBool _) = "a"
typename (FArray) = "a"
typename (FInt _) = "NSNumber *"

-- makes property decls (global bindings)
mkProps :: Bind Global n a -> [ObjCIfaceDecl]
mkProps (Bind x (FArray)) = [mkProp "NSArray" x]

-- Take a type to make, and an identifier for it.
mkProp :: Id -> String -> ObjCIfaceDecl
mkProp t s' = ObjCIfaceProp [ObjCStrong noLoc]
              (FieldGroup (DeclSpec [] []
              (Tnamed t [] noLoc) noLoc) [q] noLoc) noLoc
    where
      q = Field (Just (Id (s') noLoc))
                (Just (Ptr [] (DeclRoot noLoc) noLoc)) (Nothing) noLoc
