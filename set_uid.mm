#include <unistd.h>
#include <iostream>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>

using namespace std;

int main(int argc, char** argv) {
  setuid(0);
  setgid(0);
  setenv("DYLD_INSERT_LIBRARIES", "/Users/tristankonolige/Dropbox/Hacking/dbignore/dbfs_inj.dylib", true);
  execv(argv[0], argv);
}
