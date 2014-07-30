// Copyright   :  (C) 2014 Tristan Konolige
// License     :  BSD-style (see the file LICENSE)

#include <ApplicationServices/ApplicationServices.h>
#include <string>
#include <libgen.h>

int main(int argc, char** argv) {
  std::string cwd = dirname(dirname(argv[0]));
  LSApplicationParameters params;
  params.version = 0;
  params.flags = kLSLaunchDefaults;
  FSRef ref;
  Boolean b = 0;
  FSPathMakeRef((const UInt8 *)(cwd + "/Resources/Dropbox.app").c_str(), &ref, &b);
  params.application = &ref;
  params.asyncLaunchRefCon = NULL;
  CFStringRef key = CFSTR("DYLD_INSERT_LIBRARIES");
  CFStringRef val = CFStringCreateWithCString(NULL, (cwd + "/Resources/dropbox_inj.dylib").c_str(), kCFStringEncodingASCII); //TODO: correct encoding
  CFDictionaryRef dict = CFDictionaryCreate(NULL, (const void **)&key, (const void**)&val, 1, NULL, NULL);
  params.environment = dict;
  params.argv = NULL;
  params.initialEvent = NULL;
  OSStatus stat = LSOpenApplication(&params, NULL);
  return stat;
}
