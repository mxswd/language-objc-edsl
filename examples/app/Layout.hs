{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module Layout where

import OM
import Backend
import NSObject
import NSArray
import UIKit
import RAC
import RCL

(onLoad, props) = codeGen $ do
  textField <- newTextField
  scrollView <- newScrollView
  
  
  -- frame <- rcl_frameSignal textField
  -- bounds <- rcl_boundsSignal textField
  
  rect <- rcl_frameSignal contentView >>= insetWidthHeightNull (RCLBox 32.25) (RCLBox 16.75) (CGRectZero)
  (textRect, scrollRect) <- divideWithAmount_fromEdge (RCLBox 20) (RCLBox 8) (NSLayoutAttributeBottom) rect
  rcl_alignment scrollView [(Rcl_rect, scrollRect)]
  rcl_alignment textField [(Rcl_rect, textRect)]
  
  -- view for expression result to be printed
  -- @property (strong) typename NSTextView *textView;
  -- self.textView = self.scrollView.documentView;
  -- [self.textView becomeFirstResponder];
  textView <- newProp
  documentView scrollView >>= assign textView
  -- becomeFirstResponder textView
  
  
  -- txt <- rac_textSignal textField
  -- rac_subscribeNext txt $ \text -> do
  --   setString txt ""
  --   appendString textView text
  
  -- // - (void)textFieldDidSend:(NSTextField*)sender
  -- // {
  -- //     [self appendOutput: evalExpr([sender stringValue])];
  -- //     [sender setStringValue: @""];
  -- // }
  -- // - (void)appendOutput:(NSString*)text
  -- // {
  -- //     NSFont* menlo13 = [NSFont fontWithName: @"Menlo-Regular" size: 13];
  -- //     NSAttributedString* attrText = [[NSAttributedString alloc]
  -- //                                     initWithString: text
  -- //                                       attributes: @{NSFontAttributeName : menlo13}];
  -- //     
  -- //     [self.textView.textStorage appendAttributedString: attrText];
  -- // }
  -- 
  
  
  
  return ()
  

