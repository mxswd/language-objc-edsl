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
  
  txt <- rac_textSignal textField
  
  -- frame <- rcl_frameSignal textField
  -- bounds <- rcl_boundsSignal textField
  
  rect <- rcl_frameSignal contentView >>= insetWidthHeightNull (RCLBox 32.25) (RCLBox 16.75) (CGRectZero)
  (textRect, scrollRect) <- divideWithAmount_fromEdge (RCLBox 20) (RCLBox 8) (NSLayoutAttributeBottom) rect
  rcl_alignment scrollView [(Rcl_rect, scrollRect)]
  rcl_alignment textField [(Rcl_rect, textRect)]
  return ()
