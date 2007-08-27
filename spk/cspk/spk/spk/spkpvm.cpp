//  spkpvm.cpp -- misc. shared functions

#include "spkpvm.h"

extern const char *spkpvm_spawn_error(int error) {
  char *err_string = "unknown";
  switch (error) {
  case PvmBadParam:
    err_string = "bad parameter";
    break;
  case PvmNoHost:
    err_string = "host not not in virtual machine";
    break;
  case PvmNoFile:
    err_string = "code file can't be found";
    break;
  case PvmNoMem:
    err_string = "malloc failed";
    break;
  case PvmSysErr:
    err_string = "pvmd not responding";
    break;
  case PvmOutOfRes:
    err_string = "out of resources";
    break;
  default:
    break;
  }
  return err_string;
}
