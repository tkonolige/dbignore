PACK=-package bytestring -package bytestring-trie -package posix-paths
C_CMD=-no-hs-main dropbox_inj.c ignore.o -optl -dynamiclib $(PACK) -optc -lz -optl -lz
HS_CMD=-c ignore.hs

all: combine

i386:
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc $(HS_CMD)
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc $(C_CMD) -o dropbox_inj-i386.dylib 

x86_64:
	ghc $(HS_CMD)
	ghc $(C_CMD) -o dropbox_inj-x86_64.dylib 

combine: x86_64 i386
	lipo dropbox_inj-i386.dylib dropbox_inj-x86_64.dylib -output dropbox_inj.dylib -create
