#include <Foundation/Foundation.h>
#include <CydiaSubstrate/CydiaSubstrate.h>
#include <stdio.h>
#include <syslog.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <iostream>
#include <iostream>
#include <fstream>

#include <sys/syslimits.h>
#include <fcntl.h>

#include <libproc.h>
#include <sys/proc_info.h>

#include <regex>

#include "fsevents.h"
#include "ignore.hpp"

using namespace std;

int (*read_old)(int fildes, char *buf, size_t nbyte);
int read_new(int fildes, char *buf, size_t nbyte) {
  if(fildes == 1) { // first file descriptor is always fsevents TODO: make sure
    int res = read_old(fildes, buf, nbyte);

    int32_t type = *((int32_t*)buf);
    pid_t pid = *((pid_t*)(buf+sizeof(int32_t)));
    int off = sizeof(int32_t) + sizeof(pid_t);
    string p;
    bool found_path = false;
    while(off < nbyte) {
      int16_t etype = *(int16_t*)(buf+off);
      off += sizeof(int16_t);
      u_int16_t len = *(u_int16_t*)(buf+off);
      switch (etype) {
        case FSE_ARG_VNODE:
          off += sizeof(vnode_t);
          break;
        case FSE_ARG_STRING:
          p = buf+off+sizeof(u_int16_t);
          found_path = true;
          off += sizeof(u_int16_t) + len;
          break;
        case FSE_ARG_PATH:
          break;
        case FSE_ARG_INT32:
          off += sizeof(int32_t);
          break;
        case FSE_ARG_INT64:
          off += sizeof(int64_t);
          break;
        case FSE_ARG_RAW:
          break;
        case FSE_ARG_INO:
          off += sizeof(ino_t);
          break;
        case FSE_ARG_UID:
          off += sizeof(uid_t);
          break;
        case FSE_ARG_DEV:
          off += sizeof(dev_t);
          break;
        case FSE_ARG_MODE:
          off += sizeof(int32_t);
          break;
        case FSE_ARG_GID:
          off += sizeof(gid_t);
          break;
        case FSE_ARG_FINFO:
          off += sizeof(dev_t) + sizeof(ino_t) + sizeof(int32_t) + sizeof(uid_t) + sizeof(gid_t);
          break;
        case -19649: // this is FSE_ARG_DONE if put into int16_t
          off = nbyte; // we are done
          break;
        case 256: // no clue what this one is
          break;
        default:
          break;
      }
    }
    if(found_path && ignore_file(p)) {
      return read(fildes, buf, nbyte);
    }
    return res;
  } else {
    return read_old(fildes, buf, nbyte);
  }
}

MSInitialize {
  if(getuid() == 0) { // we should be root user, as it is the only one to read /dev/fsevents
    void* read_nocancel = MSFindSymbol(NULL, "_read$NOCANCEL$UNIX2003");

    MSHookFunction(read_nocancel, (void*)(&read_new), (void **) &read_old);
    NSLog(@"Loaded in %d", getpid());
  }
}
