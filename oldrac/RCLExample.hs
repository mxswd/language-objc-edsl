{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import RAC
import RCL
import RACStream
import Prelude hiding (map, concat)

import Language.C.Quote.ObjC

main = putStrLn $ runRCL $ fst $ runExpr $ do
  textField <- newTextField "self.textField" "self.contentView"
  scrollView <- newScrollView "self.scrollView" "self.contentView"
  
  -- RAC(self, confirmEmailVisible) = [[self.emailField.rac_textSignal
  --     skip:1]
  --     map:^(NSString *str) {
  --       return @(str.length > 0);
  --     }];
  -- RACSignal *confirmAlpha =
  --    [[RACSignal zero] concat:[RACObserve(self, confirmEmailVisible) animate]];
  -- 
  -- RAC(self.confirmEmailLabel, rcl_alphaValue) = confirmAlpha;
  
  -- text_signal <- rac_textSignal textField
  
  scrollVisible <- rac_textSignal textField >>= skip 1
                   >>= map (RACFunction [citem|^(id x) {return @(x.length > 0);};|]) -- Bool's are numbers. wat.
  animatedVisible <- animate (toDouble (RACObserve scrollVisible))
  scrollAlpha <- rac_zero >>= concat animatedVisible
  rcl_alphaSignal scrollView scrollAlpha

  rect <- rcl_frameSignal "self.contentView" >>= insetWidthHeightNull (RCLBox 32.25) (RCLBox 16.75) (CGRectZero)
  (textRect, scrollRect) <- divideWithAmountPaddingEdge (RCLBox 20) (rac_sigsize "self.verticalPadding") (NSLayoutAttributeBottom) rect
  return $
        [ mkRCL scrollView [(Rcl_rect, scrollRect)]
        , mkRCL textField [(Rcl_rect, textRect)]
        ]
