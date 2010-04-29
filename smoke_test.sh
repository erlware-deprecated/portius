#!/bin/sh

sinan dist; faxien ir
pkill beam
rm -rf /tmp/repo
portius /tmp/repo 1156
vim `epkg cfp faxien`
faxien publish
sleep 35
faxien describe-app web_interface
faxien ir portius
curl http://localhost:1156/repo-doc/lib_index.html
vim `epkg cfp faxien`
