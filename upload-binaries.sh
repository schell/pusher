#! /bin/bash

ARCH=`getconf LONG_BIT`
PLAT=`uname`
NAME="pusher_$PLAT$ARCH.tar.gz"

if [ $# -eq 0 ]
then
	cd .cabal-sandbox/bin
	echo "Archiving and uploading $NAME"
	tar czfv $NAME pusher
	tar czfv "manager_$NAME" pusher-manager
	./pusher -k synapse -b synapse-binaries -z $NAME "manager_$NAME"
else
        echo $NAME
fi
