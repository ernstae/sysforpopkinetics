#include <iostream>
#include <fstream>
#include <pvm3.h>
#include <spk/spkpvm.h>

using namespace std;

enum { SUCCESSFUL             = 0,
	OTHER_KNOWN_ERROR      = 1,
	UNKNOWN_FAILURE        = 2,
	PVM_FAILURE            = 3,
        USER_ABORT             = 4,
	USER_INPUT_ERROR       = 14,
	FILE_ACCESS_FAILURE    = 100
};

static int parent_tid;
static int exit_value = SpkPvmUnreported;
static void finish(int exit_value, char* msg)
{
    // Append error message to stderr
    cerr << "midDriver: " << msg << endl;

    // Send error message to jobDriver
    pvm_initsend(PvmDataDefault);
    pvm_pkint(&exit_value, 1, 1);
    pvm_send(parent_tid, SpkPvmExitValue);
    pvm_exit();

    // Close the file for stderr
    fclose( stderr );
}

int main(int argc, const char *argv[])
{
    if(chdir(argv[1]) != 0)
    {
       finish(FILE_ACCESS_FAILURE, "could not change directory");
       return FILE_ACCESS_FAILURE;
    }

    // Open a file to append stderr
    freopen("software_error", "a", stderr);

    parent_tid = pvm_parent();
    pvm_setopt(PvmRoute, PvmDontRoute);
    ifstream driver("driver");    
    if(!driver.good())
    {
        finish(FILE_ACCESS_FAILURE, "could not find driver");
        return FILE_ACCESS_FAILURE;
    }

    int tid = 0;
    int rval, host;
    char task[100];
    sprintf(task, "%s/driver", argv[1]);

    char* arg[2];
    arg[0] = const_cast<char*>(argv[1]);
    arg[1] = NULL;
    bool isHostDeleted;

start:
    rval = pvm_spawn(task, arg, 0, NULL, 1, &tid);

    if(rval != 1)
    {
        finish(PVM_FAILURE, "could not spawn a task for driver");
        return PVM_FAILURE;
    }

    if((host = pvm_tidtohost(tid)) < 0)
    {
        finish(PVM_FAILURE, "can't determine which host driver started on");
        return PVM_FAILURE;
    }

    if(pvm_notify(PvmHostDelete, PvmHostDelete, 1, &host) < 0)
    {
        finish(PVM_FAILURE, "pvm_notify failed for PvmHostDelete of the host");
        return PVM_FAILURE;
    }

    if(pvm_notify(PvmTaskExit, PvmTaskExit, 1, &parent_tid) < 0)
    {
        finish(PVM_FAILURE, "pvm_notify failed for PvmTaskExit of the driver");
        return PVM_FAILURE;
    }

    if(pvm_notify(PvmTaskExit, PvmTaskExit, 1, &tid) > 0)
    {
        finish(PVM_FAILURE, "pvm_notify failed for PvmtaskExit of the jobDriver");
        return PVM_FAILURE;
    }

    int bufid, exit_tid;
    while(exit_value == SpkPvmUnreported && (bufid = pvm_recv(-1, -1)) > 0)
    {
        int bytes, msgtag, source;
        if(pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
        {
            finish(PVM_FAILURE, "pvm_bufinfo failed in midDriver");
            return PVM_FAILURE;
        }
        if(msgtag == PvmTaskExit)
        {
            if(pvm_upkint(&exit_tid, 1, 1) < 0)
            {
                finish(PVM_FAILURE, "pvm_upkint failed in midDriver");
                return PVM_FAILURE;
            }
            if(exit_tid == parent_tid)
            {
                pvm_kill(tid);
                finish(USER_ABORT, "user abort job");
                return USER_ABORT;
            }
            if(exit_tid == tid && exit_value == SpkPvmUnreported)
            {
                finish(UNKNOWN_FAILURE, "pvm task exited due to error");
                return UNKNOWN_FAILURE;
            }
        }
        if(msgtag == SpkPvmExitValue && source == tid)
        {
            if(pvm_upkint(&exit_value, 1, 1) < 0)
            {
                finish(PVM_FAILURE, "pvm_upkint failed in midDriver");
                return PVM_FAILURE;
            }
        }
        if(msgtag == PvmHostDelete)
        {
            goto start;
        }
    }

    int exit_value = SUCCESSFUL;
    pvm_initsend(PvmDataDefault);
    pvm_pkint(&exit_value, 1, 1);
    pvm_send(parent_tid, SpkPvmExitValue);
    pvm_exit();
    fclose( stderr );
    return SUCCESSFUL;
}
//g++ -I/usr/share/pvm3/include -L/usr/share/pvm3/lib/LINUX -I/usr/local/include/spktest -L/usr/local/lib/spktest midDriver.cpp -o midDriver -lpvm3 -lspk
//cp midDriver /usr/local/bin/spktest/.
