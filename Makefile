PACK=-package bytestring -package bytestring-trie -package posix-paths -package Glob -package directory
C_CMD=-no-hs-main dropbox_inj.c ignore.o -optl -dynamiclib $(PACK) -optc -lz -optl -lz
HS_CMD=-c ignore.hs

i386:
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc $(HS_CMD)
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc $(C_CMD) -o dropbox_inj.dylib
