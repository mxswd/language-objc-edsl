{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends
import Interpreter

objc_import ["<Cocoa/Cocoa.h>", "AppWindow_objc.h"]


-- Haskell code used from Objective-C.

launchMsg :: String
launchMsg = "HSApp did finish launching!"

objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (strong) typename AppWindow *mainWindowController;

@end
|]


objc_implementation ['launchMsg] [cunit|

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.mainWindowController = [[AppWindow alloc] init];
  [self.mainWindowController showWindow:self];
  NSLog(@"%@", launchMsg());
}

@end
|]


objc_emit
