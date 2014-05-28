#include <CydiaSubstrate/CydiaSubstrate.h> // TODO: dont use substrate =(
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

// TODO: remove extra includes

// hook execvp so that we can inject into dbfsevents
// this requires a custom setuid executable that passes the correct DYLD_INSERT_LIBRARIES
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

ssize_t (*open_old)(const char*, int, int);
ssize_t open_new(const char* path, int flag, int mode) {
  if(ignore_file(string(path))) {
    syslog(LOG_NOTICE, "Ignoring... %s", path);
    return open_old("/dev/null", flag, mode);
  }
  return open_old(path, flag, mode);
}

MSInitialize {
  // MSHookFunction(execve, MSHake(execve)); // not used for now

  void* lstat_ = MSFindSymbol(NULL, "_lstat");
  if(lstat_ != NULL)
    MSHookFunction(lstat_, (void*)(&lstat_new), (void **) &lstat_old);

  void* open_ = MSFindSymbol(NULL, "_open");
  if(open_ != NULL)
    MSHookFunction(open_, (void*)(&open_new), (void **) &open_old);
}

