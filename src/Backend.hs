{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

import OM

import Language.C.Quote.ObjC
import Language.C
import Text.PrettyPrint.Mainland
import Data.Loc
import Data.String

instance IsString Id where
  fromString x = Id x noLoc

pr :: OM ()
pr = do
  x <- newInteger 4
  xs <- newArray
  insertNum x xs
  -- newBool >>= flip insertBool xs
  p_xs <- newArrayProp
  printNum x

newInteger :: Int -> OM (Bind Local TTrue NSInteger)
newInteger x = mkOM $ Bind "wat" (FInt x)

newArray :: OM (Bind Local TTrue (NSArray t))
newArray = mkOM $ Bind "wo" FArray

insertNum :: Bind t1 TTrue NSInteger -> Bind t2 TTrue (NSArray t3) -> OM ()
insertNum (Bind x _) (Bind xs _) = addOM (NoBind (
  FFunction [cexp|[$id:xs addObject:$id:x]|]))

newArrayProp :: OM (Bind Global TTrue (NSArray t))
newArrayProp = do
  let n = "wow"
  addOM (NoBind (FFunction [cexp|$id:n = [[NSArray alloc] init]|]))
  mkOM $ Bind n FArray

newTextField :: OM (Bind Global TTrue (NSTextField))
newTextField = undefined

printNum :: TypeLC t => Bind t TTrue NSInteger -> OM ()
printNum (Bind s _) = addOM (NoBind (FFunction [cexp|NSLog()|]))

test = runOM pr f' g'
test' = do
  putStrLn $ prettyPragma 90 (ppr (snd test))
  putStrLn $ prettyPragma 90 (ppr (fst test))

f' = concat . typeListMap mkBlocks
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
mkBlocks (NoBind (FFunction s)) = [BlockStm (Exp (Just s) noLoc)]

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
