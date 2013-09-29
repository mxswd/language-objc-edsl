{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>", "AppWindow_objc.h"]

objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (strong) typename AppWindow *mainWindowController;

@end
|]


objc_implementation [] [cunit|

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.mainWindowController = [[AppWindow alloc] init];
  [self.mainWindowController showWindow:self];
}

@end
|]


objc_emit
