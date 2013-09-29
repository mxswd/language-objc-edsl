{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module UIKit where

import Language.C.Quote.ObjC
import Language.C hiding (Func)
import Data.Loc
import Data.String
import Backend
import OM

import System.IO.Unsafe

-- subclassing
class View (a :: Ty) where
instance View NSView
instance View NSTextField
instance View NSScrollView

-- "pure", but really a pre-req to the code generator
contentView :: Bind Global TTrue NSView
contentView = Bind "contentView" FNSView

newTextField :: OM (Bind Global TTrue NSTextField)
newTextField = do
  let n' = unsafePerformIO fresh
      n = Bind n' FTextField :: Bind Global TTrue NSTextField
      sig = [citem| {
              $id:n = [[NSTextField alloc] initWithFrame:NSZeroRect];
              $id:n.wantsLayer = YES;
              $id:n.stringValue = @"";
              [$id:n sizeToFit];
              $id:n.autoresizingMask = NSViewMaxXMargin | NSViewMinYMargin;
              [$id:(contentView) addSubview:$id:n];
            }|]
  addOM (NoBind (FBlockItem sig))
  mkOM n

newScrollView :: OM (Bind Global TTrue NSScrollView)
newScrollView = do
  let n' = unsafePerformIO fresh
      -- you must qualify the scope before it is given to language-c-quote toIdent
      n = Bind n' FScrollView :: Bind Global TTrue NSScrollView
      sig = [citem| {
              $id:n = [[NSScrollView alloc] initWithFrame:NSZeroRect];
              $id:n.wantsLayer = YES;
              $id:n.autoresizingMask = NSViewMaxXMargin | NSViewMinYMargin;
              [$id:(contentView) addSubview:$id:n];
            }|]
  addOM (NoBind (FBlockItem sig))
  mkOM n

instance DefTypes NSTextView where
  defaultType = (FNSTextView, undefined)

documentView :: Bind s1 TTrue NSScrollView -> OM (Bind Local TTrue NSTextView)
documentView = undefined

becomeFirstResponder :: Bind s1 TTrue t -> OM ()
becomeFirstResponder = undefined
