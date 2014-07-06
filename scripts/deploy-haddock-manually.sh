#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
cabal haddock
f=`mktemp -d`
git clone git@github.com:CodeBlock/socdiff.git "$f/socdiff.git"
pushd "$f/socdiff.git"
  git checkout gh-pages
  git rm -rf api
popd
mv dist/doc/html/socdiff/ "$f/socdiff.git/api"
pushd "$f/socdiff.git"
  git add -A
  git commit -m "[scripted] Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://codeblock.github.io/socdiff/api/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
