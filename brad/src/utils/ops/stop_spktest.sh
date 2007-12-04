#!/bin/bash

ASPKSERVER="aspkserver"
CSPKSERVER="cspkserver"
JOBQSERVER="jobqserver"

echo "====================================================="
echo " You are about to stop the SPK test environment"
echo ""
echo " If you would like to abort, please press CTRL+C now"
echo "====================================================="

sleep 3

for server in $ASPKSERVER
do
  ssh $server /etc/init.d/spkcmptestd stop
  ssh $server rm -rfv /tmp/lock_spkcmptestd
done

for server in $CSPKSERVER
do
 ssh $server /etc/init.d/spkruntestd stop
 ssh $server rm -rfv /tmp/lock_spkruntestd
done

for server in $JOBQSERVER
do
  ssh $server /etc/init.d/jobqtestd stop
  ssh $server rm -rfv /tmp/lock_jobqtestd
done



echo "System stopped"

