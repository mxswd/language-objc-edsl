{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables #-}

import OM

import Language.C.Quote.ObjC
import Language.C

pr = do
  x <- newInteger 4
  printNum x

newInteger :: Int -> OM (Bind Local TTrue NSInteger)
newInteger x = mkOM f
  where
    f = Bind "wat" (FInt x)

printNum :: TypeLC t => Bind t TTrue a -> OM (Bind Local TFalse NSUnit)
printNum s = addOM (NoBind (FUnit)) >> return (NoBind FUnit)

-- runOm :: OM t -> ([BlockItem], [ObjCIfaceDecl])
-- runOm :: OM x -> ((Bind Local n1 a1) -> r1) -> ((Bind Global n2 a2) -> r2) -> ([r1], [r2])
-- runOm :: OM t2 -> (Bind Local n1 a1 -> [String]) -> (Bind Global n2 a2 -> [String]) -> ([String], [String])

test = runOM pr f' g'

f' = concat . typeListMap (mkBlocks)
g' = concat . typeListMap mkProps

-- makes blockitems (local bindings)
mkBlocks :: Bind Local n a -> [String]
mkBlocks (Bind x (FBool b)) = [x]
mkBlocks (Bind x (FInt b)) = [x]
mkBlocks (Bind x (FFunction b)) = [x]
mkBlocks (NoBind _) = []

-- makes property decls (global bindings)
mkProps :: Bind Global n a -> [String]
mkProps = undefined
