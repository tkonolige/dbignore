#include <CydiaSubstrate/CydiaSubstrate.h>
#include <Foundation/Foundation.h>
#include <sys/types.h>
#include <sys/dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <cstring>
#include <dirent.h>
#include <libgen.h>
#include <syslog.h>
#include <errno.h>

#include <crt_externs.h>

#include "ignore.hpp"

using namespace std;

MSHook(int, execve, const char *path, char * const *argv, char * const *envp) {
  if(strstr(path, "dbfseventsd") != NULL) { // only hook dbfseventsd
    path = "/Users/tristankonolige/Dropbox/Hacking/dbignore/set_uid";
  }
  NSLog(@"execing: %s", path);
  return _execve(path, argv, envp);
}

int(*lstat_old)(const char*, struct stat*);
int lstat_new(const char* path, struct stat* buf) {
  if(ignore_file(string(path))) {
    errno = ENOENT;
    return -1;
  }
  return lstat_old(path, buf);
}

MSInitialize {
  MSHookFunction(execve, MSHake(execve));

  void* lstat_ = MSFindSymbol(NULL, "_lstat");
  if(lstat_ != NULL)
    MSHookFunction(lstat_, (void*)(&lstat_new), (void **) &lstat_old);
}

