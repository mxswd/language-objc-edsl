{-# LANGUAGE DataKinds #-}
module RCL where

import Data.List
import qualified Data.Map as M
import Data.Monoid
import RAC

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
  
data NSLayout = NSLayoutAttributeBottom deriving Show
data View = View String RCLAlignment

data RCLAlignment = RCLAlignment (M.Map RCLKeys String)

instance Show View where
  show (View n rcl) = "RCLAlignment(" <> n <> ") = " <> show rcl
  showList x = flip (++) $ unlines (map show x)

showP (k, s) = show k <> ": " <> s

instance Show RCLAlignment where
  show (RCLAlignment xs) = "@{\n  " <> intercalate ",\n  " (map showP (M.toList xs)) <> "\n};"

mkRCL :: String -> [(RCLKeys, String)] -> View
mkRCL s = View s . RCLAlignment . M.fromList

runRCL :: RAC [View] -> String
runRCL (RAC xs bs) = show bs <> "\n" <> show xs

-- [self.window.contentView.rcl_frameSignal insetWidth:RCLBox(32.25) height:RCLBox(16.75) nullRect:CGRectZero];
rcl_frameSignal :: String -> RAC Var
rcl_frameSignal x = fresh $ Rcl_frameSignal x

-- [x insetWidth:RCLBox(32.25) height:RCLBox(16.75) nullRect:CGRectZero]
insetWidthHeightNull :: RACSignal FKRect -> RACSignal FKRect -> CGRect -> Var -> RAC Var
insetWidthHeightNull w h n s = fresh $ RACSigSize $ "[" <> s <> " insetWidth:" <> show w <> " height:" <> show h <> " nullRect:" <> show n <> "]"

-- [x divideWithAmount:RCLBox(20) padding:self.verticalPadding fromEdge:NSLayoutAttributeBottom];
divideWithAmountPaddingEdge :: RACSignal FKRect -> RACSignal FKSize -> NSLayout -> Var -> RAC (Var, Var)
divideWithAmountPaddingEdge d p e s = fresh2 $ RACSigSize $ "[" <> s <> " divideWithAmount:" <> show d <> " padding:" <> show p <> " fromEdge:" <> show e <> "]"
