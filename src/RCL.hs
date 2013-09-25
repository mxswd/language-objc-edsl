{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module RCL where

import Language.C.Quote.ObjC
import Language.C.Quote.Base
import Language.C hiding (Func)
import Data.Loc
import Data.String
import Backend
import OM

import UIKit
import NSObject

import qualified Data.Map as M

import System.IO.Unsafe

data RCLKeys = Rcl_rect | Rcl_size | Rcl_origin | Rcl_center
             | Rcl_centerX | Rcl_centerY | Rcl_height | Rcl_width
             | Rcl_trailing | Rcl_leading | Rcl_top | Rcl_bottom
             | Rcl_right | Rcl_left | Rcl_baseline deriving (Eq, Ord, Enum)

instance Show RCLKeys where
  show Rcl_rect = "rcl_rect"
  show Rcl_size = "rcl_size"
  show Rcl_origin = "rcl_origin"
  show Rcl_center = "rcl_center"
  show Rcl_centerX = "rcl_centerX"
  show Rcl_centerY = "rcl_centerY"
  show Rcl_height = "rcl_height"
  show Rcl_width = "rcl_width"
  show Rcl_trailing = "rcl_trailing"
  show Rcl_leading = "rcl_leading"
  show Rcl_top = "rcl_top"
  show Rcl_bottom = "rcl_bottom"
  show Rcl_right = "rcl_right"
  show Rcl_left = "rcl_left"
  show Rcl_baseline = "rcl_baseline"

-- rclbox

data NSLayout = NSLayoutAttributeBottom
instance ToExp NSLayout where
  toExp NSLayoutAttributeBottom _ = [cexp|NSLayoutAttributeBottom|]

-- data CGRect = CGRectZero | CGRectMake String
-- instance ToExp CGRect where
--   toExp CGRectZero _ = [cexp|CGRectZero|]
-- 
-- data View = View Exp RCLAlignment
-- 
-- data RCLAlignment = RCLAlignment (M.Map RCLKeys (RACSignal FKSize))
-- 
-- instance ToExp View where
--   toExp (View n rcl) _ = [cexp|RCLAlignment($n) = $rcl|]
-- 
-- instance ToExp RCLAlignment where
--   toExp (RCLAlignment xs) l = ObjCLitDict (mkPairs xs l) l
-- 
-- mkPairs :: M.Map RCLKeys (RACSignal FKSize) -> SrcLoc -> [(Exp, Exp)]
-- mkPairs xs l = map (\(k, i) -> (Var (fromString (show k)) l, toExp i l)) $ M.toList xs
-- 
-- mkRCL :: Exp -> [(RCLKeys, RACSignal FKSize)] -> View
-- mkRCL s = View s . RCLAlignment . M.fromList
-- 
-- -- | Typical pretty print.
-- runRCL :: [BlockItem] -> String
-- runRCL r = unlines $ map (prettyPragma 80) $ map ppr r
-- 
-- runExpr :: RAC [View] -> ([BlockItem], [ObjCIfaceDecl])
-- runExpr (RAC xs bs) = let
--       xs' = map (mkBlock) bs <> map (BlockStm . mkStm) xs
--       ds' = catMaybes (map mkProperty bs)
--     in (xs', ds')
-- 
-- mkStm :: View -> Stm
-- mkStm v = Exp (Just (toExp v noLoc)) noLoc

-- [self.window.contentView.rcl_frameSignal insetWidth:RCLBox(32.25) height:RCLBox(16.75) nullRect:CGRectZero];

rcl_frameSignal :: TypeLC t => View v => Bind t TTrue v -> OM (Bind Local TTrue (RACSignal CGRect))
rcl_frameSignal x = mkOM $ Bind (unsafePerformIO fresh)
                         $ FFunctionE [cexp|[$id:x rcl_frameSignal]|] (FRACSignal)

rcl_boundsSignal :: TypeLC t => View v => Bind t TTrue v -> OM (Bind Local TTrue (RACSignal CGRect))
rcl_boundsSignal x = mkOM $ Bind (unsafePerformIO fresh)
                          $ FFunctionE [cexp|[$id:x rcl_boundsSignal]|] (FRACSignal)

insetWidthHeightNull :: TypeLC t
                     => Func (RACSignal CGRect)
                     -> Func (RACSignal CGRect)
                     -> Func CGRect
                     -> Bind t TTrue (RACSignal CGRect)
                     -> OM (Bind Local TTrue (RACSignal CGSize))
insetWidthHeightNull w h n x = mkOM $ Bind (unsafePerformIO fresh)
    $ FFunctionE [cexp|[$id:x insetWidth:$w height:$h nullRect:$n]|] FRACSignal

divideWithAmountPaddingEdge :: TypeLC t
                     => Func (RACSignal CGRect)
                     -> Func (RACSignal CGRect)
                     -> NSLayout
                     -> Bind t TTrue (RACSignal CGSize)
                     -> OM (Bind Local TTrue (RACSignal CGSize), Bind Local TTrue (RACSignal CGSize))
divideWithAmountPaddingEdge d p e x = fmap unpack $ mkOM
    $ BindTuple (unsafePerformIO fresh) (unsafePerformIO fresh)
    $ FFunctionT [cexp|[$id:x divideWithAmount:$d padding:$p fromEdge:$e]|] (FRACTuple FRACSignal FRACSignal)

unpack :: (Bind Local TTrue (RACTuple (RACSignal a) (RACSignal b))) -> (Bind Local TTrue (RACSignal a), Bind Local TTrue (RACSignal b))
unpack (BindTuple x1 x2 (FFunctionT _ (FRACTuple v1 v2))) = (Bind x1 v1, Bind x2 v2)

rcl_alignment :: TypeLC t => View v => Bind Global TTrue v -> [(RCLKeys, Bind t TTrue (RACSignal CGSize))] -> OM ()
rcl_alignment x xs = addOM (NoBind (FFunction [cexp|RCLAlignment($id:x) = $(mkPairs (M.fromList xs))|]))

mkPairs :: TypeLC t => M.Map RCLKeys (Bind t TTrue (RACSignal v)) -> Exp
mkPairs xs = let l = noLoc
    in flip ObjCLitDict l
       $ map (\(k, i) -> (Var (fromString (show k)) l, [cexp|$id:i|])) $ M.toList xs
