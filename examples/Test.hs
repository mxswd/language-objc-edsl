{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

import OM
import Backend
import NSObject
import NSArray
import UIKit
import RAC
import RCL

import Text.PrettyPrint.Mainland

-- prereq: self.contentView
test = do
  let t = codeGen testP
  putStrLn $ prettyPragma 90 (ppr (snd t))
  putStrLn $ prettyPragma 90 (ppr (fst t))

-- TODO: put in comments with the original haskell?

testP :: OM ()
testP = do
  -- newTextField >>= rac_textSignal >>= printDescription
  textField <- newTextField
  scrollView <- newScrollView
  
  txt <- rac_textSignal textField
  
  frame <- rcl_frameSignal textField
  bounds <- rcl_boundsSignal textField
  
  rect <- rcl_frameSignal contentView >>= insetWidthHeightNull (RCLBox 32.25) (RCLBox 16.75) (CGRectZero)
  (textRect, scrollRect) <- divideWithAmount_fromEdge (RCLBox 20) (RCLBox 8) (NSLayoutAttributeBottom) rect
  rcl_alignment scrollView [(Rcl_rect, scrollRect)]
  rcl_alignment textField [(Rcl_rect, textRect)]
  return ()

testP_NSArray :: OM ()
testP_NSArray = do
  xs <- newProp
  x <- new :: OM (Bind Local TTrue NSInteger)
  insertObject x xs
  h <- arrayHead xs
  printDescription h
