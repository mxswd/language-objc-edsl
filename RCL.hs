{-# LANGUAGE DataKinds, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module RCL where

import Data.List
import qualified Data.Map as M
import Data.Monoid
import RAC
import Language.C.Syntax
import Language.C.Quote.ObjC
import Data.Loc
import Text.PrettyPrint.Mainland
import Language.C.Pretty
import Data.String
import Data.Maybe

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

data NSLayout = NSLayoutAttributeBottom
instance ToExp NSLayout where
  toExp NSLayoutAttributeBottom _ = [cexp|NSLayoutAttributeBottom|]

data CGRect = CGRectZero | CGRectMake String
instance ToExp CGRect where
  toExp CGRectZero _ = [cexp|CGRectZero|]

data View = View Exp RCLAlignment

data RCLAlignment = RCLAlignment (M.Map RCLKeys Id)

instance ToExp View where
  toExp (View n rcl) _ = [cexp|RCLAlignment($n) = $rcl|]

instance ToExp RCLAlignment where
  toExp (RCLAlignment xs) l = ObjCLitDict (mkPairs xs l) l

mkPairs :: M.Map RCLKeys Id -> SrcLoc -> [(Exp, Exp)]
mkPairs xs l = map (\(k, i) -> (Var (fromString (show k)) l, toExp i l)) $ M.toList xs

mkRCL :: Exp -> [(RCLKeys, Id)] -> View
mkRCL s = View s . RCLAlignment . M.fromList

-- | Typical pretty print.
runRCL :: [BlockItem] -> String
runRCL r = unlines $ map (prettyPragma 80) $ map ppr r

runExpr :: RAC [View] -> ([BlockItem], [ObjCIfaceDecl])
runExpr (RAC xs bs) = let
      xs' = map (mkBlock) bs <> map (BlockStm . mkStm) xs
      ds' = catMaybes (map mkProperty bs)
    in (xs', ds')

mkStm :: View -> Stm
mkStm v = Exp (Just (toExp v noLoc)) noLoc

-- [self.window.contentView.rcl_frameSignal insetWidth:RCLBox(32.25) height:RCLBox(16.75) nullRect:CGRectZero];
rcl_frameSignal :: Exp -> RAC Id
rcl_frameSignal x = fresh $ Rcl_frameSignal x

-- Skeleton generator
-- self.scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
-- self.scrollView.wantsLayer = YES;
-- self.scrollView.autoresizingMask = NSViewMaxXMargin | NSViewMinYMargin;
-- [self.contentView addSubview:self.scrollView];
-- | var to bind to, var to `addSubview` to
-- the return value is for convenience, it is left to the reader why.
newScrollView :: Id -> Exp -> RAC Exp
newScrollView s v = RAC (Var s noLoc) [PropertyBind prop sig]
    where 
      prop = mkProp "NSScrollView" s
      sig = RACScrollView
              ( [cstm|$s = [[NSScrollView alloc] initWithFrame:NSZeroRect];|]
              : [cstm|$s.wantsLayer = YES;|]
              : [cstm|$s.autoresizingMask = NSViewMaxXMargin | NSViewMinYMargin;|]
              : [cstm|[$v addSubview:$s];|]
              : []
              )

-- self.textField = [[NSTextField alloc] initWithFrame:NSZeroRect];
-- self.textField.wantsLayer = YES;
-- self.textField.stringValue = @"";
-- [self.textField sizeToFit];
-- self.textField.autoresizingMask = NSViewMaxXMargin | NSViewMinYMargin;
-- [self.contentView addSubview:self.textField];
newTextField :: Id -> Exp -> RAC Exp
newTextField s v = RAC (Var s noLoc) [PropertyBind prop sig]
     where
       prop = mkProp "NSTextField" s
              -- when https://github.com/mainland/language-c-quote/issues/26 is fixed?
              -- [ciface|@property (strong) typename NSTextField *$sdecl:q;|]
       sig = RACTextField
               ( [cstm|$s = [[NSTextField alloc] initWithFrame:NSZeroRect];|]
               : [cstm|$s.wantsLayer = YES;|]
               : [cstm|$s.stringValue = @"";|]
               : [cstm|[$s sizeToFit];|]
               : [cstm|$s.autoresizingMask = NSViewMaxXMargin | NSViewMinYMargin;|]
               : [cstm|[$v addSubview:$s];|]
               : []
               )

-- make objective c property
mkProp t (Id s' _) = ObjCIfaceProp [ObjCStrong noLoc]
              (FieldGroup (DeclSpec [] []
              (Tnamed t [] noLoc) noLoc) [q] noLoc) noLoc
    where
      q = Field (Just (Id (snd (fromMethod s')) noLoc))
                (Just (Ptr [] (DeclRoot noLoc) noLoc)) (Nothing) noLoc

-- [x insetWidth:RCLBox(32.25) height:RCLBox(16.75) nullRect:CGRectZero]
insetWidthHeightNull :: RACSignal FKRect -> RACSignal FKRect -> CGRect -> Id -> RAC Id
insetWidthHeightNull w h n s = fresh $ RACSigSize $ [cexp|[$s insetWidth:$w height:$h nullRect:$n]|]

-- [x divideWithAmount:RCLBox(20) padding:self.verticalPadding fromEdge:NSLayoutAttributeBottom];
divideWithAmountPaddingEdge :: RACSignal FKRect -> RACSignal FKSize -> NSLayout -> Id -> RAC (Id, Id)
divideWithAmountPaddingEdge d p e s = fresh2 $ RACSigSize $ [cexp|[$s divideWithAmount:$d padding:$p fromEdge:$e]|]

