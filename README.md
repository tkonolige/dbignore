# Dropbox Ignore

#Installation
Download the latest release (look under the releases tab), unzip it, and run `./install`

# Building
Dropbox is a 32-bit executable on OSX and must be built by a 32-bit version of ghc.

Given a ghc which targets i386, the following steps are required to set up the build environment.

1. Install bytestring, Glob, posix-paths, directory, aeson using cabal.
2. Install the custom version of bytestring-trie located in this repository.
3. run `make`

#Thanks
- Saurik for providing numerous explanations and help on hooking functions.
- wren ng thornton for the bytestring-trie package.
