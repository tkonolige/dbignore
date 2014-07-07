all: combine

i386:
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc -c ignore.hs
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc -no-hs-main dropbox_inj.c ignore.o -optl -dynamiclib -o dropbox_inj-i386.dylib

x86_64:
	ghc -c ignore.hs
	ghc -no-hs-main dropbox_inj.c ignore.o -optl -dynamiclib -o dropbox_inj-x86_64.dylib

combine: x86_64 i386
	lipo dropbox_inj-i386.dylib dropbox_inj-x86_64.dylib -output dropbox_inj.dylib -create
