#pragma once

#include <fstream>
#include <fnmatch.h>
#include <libgen.h>

#include <map>
#include <vector>

using namespace std;

static const string IGNORE_NAME = ".dbignore";

// TODO: cache needs to be updated if new dbignore file is created in subdirectory

// cache ignore files for better performance
// TODO: is this taking too much memory?
// Directory -> (File, [Regex])
map<string, pair<string, vector<string>>> ignore_cache;

// dirname from std::string
string sdirname(const string dir) {
  char tmp[dir.length()+1];
  strncpy(tmp, dir.c_str(), dir.length());
  return dirname(tmp);
}

// check if file exists
bool exists(string f) {
  return access(f.c_str(), R_OK) == 0;
}

// recurses up the directory tree to find the closest dbignore file
// returns an empty string if no dbignore is found
// path should be a directory
string find_dbignore(string path) {
  string dbignore = path + "/" + IGNORE_NAME;
  if(exists(dbignore)) {
    return dbignore;
  } else if(path == "/") {
    return "";
  } else {
    return find_dbignore(sdirname(path));
  }
}

// check if file matches any regexs (split by newlines)
bool match_file(string file, vector<string> regexs) {
  for(auto line : regexs) {
    if(fnmatch(line.c_str(), file.c_str(), 0) != FNM_NOMATCH) {
      return true;
    }
  }
  return false;
}

void update_dbignore(string dbignore) {

}

// check if a file should be ignored
bool ignore_file(string file) {
  string dir = sdirname(file);

  // check if we have a dbignore file
  if(file.length() >= IGNORE_NAME.length() && file.substr(file.length() - IGNORE_NAME.length())  == IGNORE_NAME) {
    
  }

  // look up directory in cache
  auto pos = ignore_cache.find(dir);
  if(pos != ignore_cache.end()) {
    if(pos->second.first == file) {

    }
    return match_file(file, pos->second.second); // in cache, check against it
  } else {
    // not in cache, find correct .dbignore and update
    string dbignore = find_dbignore(dir);
    ignore_cache[dir]->first = dbignore;
    if(dbignore != "") {
      ifstream infile(dbignore);
      string line;
      while(getline(infile, line)) {
        ignore_cache[dir]->second.push_back(sdirename(dbignore) + "/" + line);
      }
    }
  }

}
