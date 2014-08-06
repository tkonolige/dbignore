PACK=-package bytestring -package bytestring-trie -package posix-paths -package Glob -package directory -package aeson
C_CMD=-no-hs-main dropbox_inj.c ignore.o -optl -dynamiclib $(PACK) -optc -lz -optl -lz -optl -static-libgcc -static
HS_CMD=-c ignore.hs

all: i386 launch

i386:
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc -threaded $(HS_CMD)
	/usr/local/stow/ghc-7.8.2-i386/bin/i386-apple-darwin-ghc -threaded $(C_CMD) -o dropbox_inj.dylib

launch:
	clang++ launch.mm -o launch -framework ApplicationServices
