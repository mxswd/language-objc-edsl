{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
import RAC
import RCL
import Language.C.Quote.ObjC
import Test.HUnit

main = runTestTT tests

tests = test [ "test1" ~: test1_m ~=? test1_v ]

test1_m = runExpr $ do
  rect <- rcl_frameSignal "self.contentView" >>= insetWidthHeightNull (RCLBox 32.25) (RCLBox 16.75) (CGRectZero)
  (textRect, scrollRect) <- divideWithAmountPaddingEdge (RCLBox 20) (rac_sigsize "self.verticalPadding") (NSLayoutAttributeBottom) rect
  return $
        [ mkRCL "self.scrollView" [(Rcl_rect, scrollRect)]
        , mkRCL "self.textField" [(Rcl_rect, textRect)]
        ]

-- typename strikes again!

test1_v = [ [citem|typename RACSignal * f_1 = [self.contentView rcl_frameSignal];|]
          , [citem|typename RACSignal * f_2 = [f_1 insertWidth: RCLBox(32.25) height: RCLBox(16.75) nullRect: CGRectZero()];|]
          , [citem|RACTupleUnpack(RACSignal * f_3, RACSignal * f_4) = [f_2 divideWithAmount: RCLBox(20.0) padding: self.verticalPadding fromEdge: NSLayoutAttributeBottom()];|]
          , [citem|RCLAlignment(self.scrollView) = @{rcl_rect : f_4};|]
          , [citem|RCLAlignment(self.textField) = @{rcl_rect : f_3};|]
          ]

