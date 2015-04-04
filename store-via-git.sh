#!/usr/bin/env bash

socdiff || {
  echo "Something bad happened. Examine above output and try again."
  exit 1
}

pushd ~/.socdiff_cache
[ -d .git ] || git init
git add -A
git commit -m '[scripted] socdiff run'
popd
