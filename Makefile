PACK=-package bytestring -package bytestring-trie -package posix-paths -package Glob -package directory -package aeson
C_CMD=-no-hs-main dropbox_inj.c ignore.o -optl -dynamiclib $(PACK) -optc -lz -optl -lz -optl -static-libgcc -static
HS_CMD=-c ignore.hs

all: i386

i386:
	/usr/local/stow/bin/ghc -threaded $(HS_CMD)
	/usr/local/stow/bin/ghc -threaded $(C_CMD) -o dropbox_inj.dylib
