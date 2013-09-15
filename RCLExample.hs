{-# LANGUAGE OverloadedStrings #-}
import RAC
import RCL

main = putStrLn $ runRCL $ runExpr $ do
  rect <- rcl_frameSignal "self.contentView" >>= insetWidthHeightNull (RCLBox 32.25) (RCLBox 16.75) (CGRectZero)
  (textRect, scrollRect) <- divideWithAmountPaddingEdge (RCLBox 20) (rac_sigsize "self.verticalPadding") (NSLayoutAttributeBottom) rect
  return $
        [ mkRCL "self.scrollView" [(Rcl_rect, scrollRect)]
        , mkRCL "self.textField" [(Rcl_rect, textRect)]
        ]
