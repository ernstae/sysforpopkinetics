#include <iostream>
#include <pvm3.h>
#include <spk/spkpvm.h>
#include <fstream>

using namespace std;

enum { SUCCESSFUL             = 0,
	OTHER_KNOWN_ERROR      = 1,
	UNKNOWN_FAILURE        = 2,
	PVM_FAILURE            = 3,
        USER_ABORT             = 4,
	USER_INPUT_ERROR       = 14,
	FILE_ACCESS_FAILURE    = 100
};

static int exit_value = SpkPvmUnreported;
static void stop(const char* msg, int exit_value)
{
    cerr << "jobDriver: " << msg << endl;
    pvm_exit();
    fclose( stderr );
    cout << "exit_value: " << exit_value << endl;
}

static bool isInt(const char* str)
{
    int i = 0;
    while(str[i] != '\0')
    {
        if(!isdigit(str[i++]))
            return false;
    }
    return true;
}

int main(int argc, const char *argv[])
{
    char* usage = "Usage: jobDriver [argument1] [argument2]\n\nargument1: number of processes\nargument2: only for single process mode it is the path of the midDriver";
    if(argc == 1 || argc > 3)
    {
        cerr << usage << endl;
        return USER_INPUT_ERROR;
    }
    else if(argc == 2)
    {
        if(strcmp(argv[1], "--help") == 0)
        {
            cerr << usage << endl;
            return USER_INPUT_ERROR;
        }
        else if(!isInt(argv[1]) || atoi(argv[1]) < 2)
        {
            cerr << usage << endl;
            return USER_INPUT_ERROR;
        }
    }
    else
    {
        if(atoi(argv[1]) != 1)
        {
            cerr << usage << endl;
            return USER_INPUT_ERROR;
        }
    }

    char cwd[100];
    getcwd(cwd, 100);
    freopen("software_error", "a", stderr);
    pvm_setopt(PvmRoute, PvmDontRoute);
    const char* driver_name = argc > 2 ? argv[2] : "driver";
    ifstream driver(driver_name);
    if(!driver.good())
    {
        stop("could not find driver instance", FILE_ACCESS_FAILURE);
        return FILE_ACCESS_FAILURE;
    }

    int tid = 0;
    int rval, host;
    char** arg;

start:
    if(argc > 2)
    {
        char* task = const_cast<char*>(argv[2]);
        arg = new char*[2];
        arg[0] = cwd;
        arg[1] = NULL;
        rval = pvm_spawn(task, arg, 0, NULL, 1, &tid);
    }
    else if(argc > 1)
    {
        char task[100];
        sprintf(task, "%s/driver", cwd);
        arg = new char*[3];
        arg[0] = cwd;
        arg[1] = const_cast<char*>(argv[1]);
        arg[2] = NULL;
        rval = pvm_spawn(task, arg, 0, NULL, 1, &tid);
    }

    delete[] arg;

    if(rval != 1)
    {
        stop("could not spawn a master", PVM_FAILURE);
        return PVM_FAILURE;
    }

    if((host = pvm_tidtohost(tid)) < 0)
    {
        stop("can't determine which host driver started on", PVM_FAILURE);
        return PVM_FAILURE;
    }

    if(pvm_notify(PvmHostDelete, PvmHostDelete, 1, &host) < 0)
    {
        stop("pvm_notify failed for PvmHostDelete of the host", PVM_FAILURE);
        return PVM_FAILURE;
    }

    if(pvm_notify(PvmTaskExit, PvmTaskExit, 1, &tid) < 0)
    {
        stop("pvm_notify failed for PvmTaskExit of the task", PVM_FAILURE);
        return PVM_FAILURE;
    }

    int bufid, exit_tid;
    while(exit_value == SpkPvmUnreported && (bufid = pvm_recv(-1, -1)) > 0)
    {
        int bytes, msgtag, source;
        if(pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
        {
            stop("pvm_bufinfo failed", PVM_FAILURE);
            return PVM_FAILURE;
        }
        if(msgtag == PvmTaskExit)
        {
            if(pvm_upkint(&exit_tid, 1, 1) < 0)
            {
                stop("pvm_upkint failed in midDriver", PVM_FAILURE);
                return PVM_FAILURE;
            }
            if(exit_tid == tid && exit_value == SpkPvmUnreported)
            {
                stop("pvm task exited due to error", UNKNOWN_FAILURE);
                return UNKNOWN_FAILURE;
            }
        }
        if(msgtag == SpkPvmExitValue && source == tid)
        {
            if(pvm_upkint(&exit_value, 1, 1) < 0)
            {
                stop("pvm_upkint failed in midDriver", PVM_FAILURE);
                return PVM_FAILURE;
            }
            if(exit_value != SUCCESSFUL)
            {
                pvm_exit();
                fclose( stderr );
                cout << "exit_value: " << exit_value << endl;
                return exit_value;
            }
        }
        if(msgtag == PvmHostDelete)
        {
            goto start;
        }
    }
    
    pvm_exit();
    fclose( stderr );
    cout << "exit_value: " << exit_value << endl;
    return exit_value;
}
//g++ -I/usr/share/pvm3/include -L/usr/share/pvm3/lib/LINUX -I/usr/local/include/spktest -L/usr/local/lib/spktest jobDriver.cpp -o jobDriver -lpvm3 -lspk
//cp jobDriver /usr/local/bin/spktest/.
