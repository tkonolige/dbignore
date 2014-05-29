#include <Foundation/Foundation.h>
#include <syslog.h>
#include <errno.h>

#include "ignore.hpp"

using namespace std;

// hook execvp so that we can inject into dbfsevents
// this requires a custom setuid executable that passes the correct DYLD_INSERT_LIBRARIES
// MSHook(int, execve, const char *path, char * const *argv, char * const *envp) {
//   if(strstr(path, "dbfseventsd") != NULL) { // only hook dbfseventsd
//     path = "/Users/tristankonolige/Dropbox/Hacking/dbignore/set_uid";
//   }
//   NSLog(@"execing: %s", path);
//   return _execve(path, argv, envp);
// }

extern "C" int lstat(const char*, struct stat*);

// from http://opensource.apple.com/source/dyld/dyld-210.2.3/include/mach-o/dyld-interposing.h
#define DYLD_INTERPOSE(_replacement,_replacee) \
   __attribute__((used)) static struct{ const void* replacement; const void* replacee; } _interpose_##_replacee \
            __attribute__ ((section ("__DATA,__interpose"))) = { (const void*)(unsigned long)&_replacement, (const void*)(unsigned long)&_replacee };

int(*lstat_old)(const char*, struct stat*);
int lstat_new(const char* path, struct stat* buf) {
  if(ignore_file(string(path))) {
    errno = ENOENT;
    return -1;
  }
  return lstat(path, buf);
}
DYLD_INTERPOSE(lstat_new, lstat);

ssize_t (*open_old)(const char*, int, int);
ssize_t open_new(const char* path, int flag, int mode) {
  if(ignore_file(string(path))) {
    syslog(LOG_NOTICE, "Ignoring... %s", path);
    return open("/dev/null", flag, mode);
  }
  return open(path, flag, mode);
}
DYLD_INTERPOSE(open_new, open);
