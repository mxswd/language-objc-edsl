HC      = ghc
CFLAGS  = -fobjc-arc
HCFLAGS = 
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -package hint \
          -framework Cocoa -framework ReactiveCocoaLayout -framework ReactiveCocoa -framework Archimedes -optl-ObjC

OBJS = Main.o App.o App_objc.o AppDelegate.o AppDelegate_objc.o Interpreter.o Layout.o RAC.o RCL.o AppWindow.o AppWindow_objc.o

default: HSApp.app/Contents/MacOS/HSApp

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

Interpreter.o:
AppDelegate.o: Interpreter.o AppWindow.o
App.o:
AppWindow.o: Layout.o
Main.o: App.o AppDelegate.o
Layout.o: RAC.o RCL.o

App_objc.m: App.o
AppDelegate_objc.m: AppDelegate.o
AppWindow_objc.m: AppDelegate.o

HSApp: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

HSApp.app/Contents/MacOS/HSApp: HSApp
	cp $< $@

.PHONY: clean

clean:
	rm -f *.o *.hi App_objc.[hm] AppDelegate_objc.[hm] AppWindow_objc.[hm] *_stub.h HSApp HSApp.app/Contents/MacOS/HSApp
	