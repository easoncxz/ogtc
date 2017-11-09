#!/bin/bash

set -e
set -x

if ! which elm-make
then
  npm -g install elm
fi

DIST_DIR=_dist

rm -rfv "$DIST_DIR"
mkdir "$DIST_DIR"
cp extras.js index.html "$DIST_DIR"

elm-make src/Main.elm --output "$DIST_DIR"/elm-bundle.js
