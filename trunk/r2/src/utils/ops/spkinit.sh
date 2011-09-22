#!/bin/bash

if [ "$1" = "spkprod" ] 
then
    if [ $2 = "stop" ]
	then
	ssh cspk /etc/init.d/spkrund stop
	/etc/init.d/spkcmpd stop
	ssh webserver /etc/init.d/jobqd stop
    fi

    if [ $2 = "start" ]
	then
	ssh webserver /etc/init.d/jobqd start
	sleep 5
	/etc/init.d/spkcmpd start
	ssh cspk /etc/init.d/spkrund start
    fi
fi


if [ $1 = "spktest" ]
then
    if [ $2 = "stop" ]
	then
        ssh cspk /etc/init.d/spkruntestd stop
        /etc/init.d/spkcmptestd stop
        ssh webserver /etc/init.d/jobqtestd stop
    fi

    if [ $2 = "start" ]
	then
        ssh webserver /etc/init.d/jobqtestd start
        sleep 5
        /etc/init.d/spkcmptestd start
        ssh cspk /etc/init.d/spkruntestd start
    fi
fi
