// spkpvm.h

#ifndef SPKPVM_H
#define SPKPVM_H

#include <pvm3.h>

// function prototypes
extern const char *spkpvm_spawn_error(int);

// paths
#define SPK_SHARE      "/usr/local/spk/share"
#define SPK_WORKING    "/usr/local/spk/share/working"
#define SPKLOG_PATH    "/usr/local/spk/share/log/messages"


// message tags
#define SpkPvmExitValue 500
#define SpkPvmKill      501

// architectures
static char *i686 = "i686";

// arbitrary program limits
#define SPK_MAX_INDIVIDUALS 1000


#endif // SPKPVM_H