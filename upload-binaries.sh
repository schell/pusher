#! /bin/bash

cd .cabal-sandbox/bin
tar czfv pusher_darwin64.tar.gz pusher
tar czfv pusher-manager_darwin64.tar.gz pusher-manager
./pusher -k synapse -b synapse-binaries -z pusher_darwin64.tar.gz pusher-manager_darwin64.tar.gz
