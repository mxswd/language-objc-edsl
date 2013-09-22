{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppWindow (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends
import Interpreter
import Layout

objc_import ["<Cocoa/Cocoa.h>", "<AppKit/AppKit.h>", "<Archimedes/Archimedes.h>", "<ReactiveCocoa/ReactiveCocoa.h>", "<ReactiveCocoaLayout/ReactiveCocoaLayout.h>"]

evalExpr :: String -> IO String
evalExpr expr 
  = do { result <- eval expr
       ; return $ "Prelude> " ++ expr ++ "\n" ++ result ++ "\n"
       }

objc_interface [cunit|

@interface AppWindow : NSWindowController

@end
|]



objc_implementation ['evalExpr] [cunit|

@interface AppWindow ()

// This must be the first @property. Must.
$ifaces:props;

@property (readonly) typename NSView   *contentView;
@property (readonly) typename RACSignal  *verticalPadding;

@property (strong) typename NSTextView *textView;


@end


@implementation AppWindow

- (typename NSView *)contentView {
	return self.window.contentView;
}

- (typename RACSignal *)verticalPadding {
	return RCLBox(8);
}

- (id)init {
  return [self initWithWindowNibName:@"AppWindow"];
}

- (void)windowDidLoad
{
  [super windowDidLoad];
  
  $stm:layout;
  
  self.textView = self.scrollView.documentView;  
  [self.textView becomeFirstResponder];
  
}

// TODO: hook this up
- (void)textFieldDidSend:(typename NSTextField *)sender
{
  [self appendOutput:evalExpr([sender stringValue])];
  [sender setStringValue:@""];
}

- (void)appendOutput:(typename NSString *)text
{
  typename NSFont             *menlo13  = [NSFont fontWithName:@"Menlo-Regular" size:13];
  typename NSAttributedString *attrText = [[NSAttributedString alloc] initWithString:text 
                                                                          attributes:@{ NSFontAttributeName : menlo13 }];
  [self.textView.textStorage appendAttributedString:attrText];
}

@end
|]


objc_emit
