#!/bin/bash

ASPKSERVER="aspkserver"
CSPKSERVER="cspkserver"
JOBQSERVER="jobqserver"

echo "====================================================="
echo " You are about to start the SPK test environment"
echo ""
echo " If you would like to abort, please press CTRL+C now"
echo "====================================================="

sleep 3

for server in $ASPKSERVER
do
  ssh $server /etc/init.d/spkcmptestd restart
done

for server in $CSPKSERVER
do
 ssh $server /etc/init.d/spkruntestd restart
done

for server in $JOBQSERVER
do
  ssh $server /etc/init.d/jobqtestd restart
done


echo "System startped"

