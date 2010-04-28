#!/bin/sh

rm -rf /tmp/repo
pkill beam
portius /tmp/repo
vim `epkg cfp faxien`
faxien publish
faxien describe-app portius
faxien ir portius
curl http://localhost:1156/repo-doc
