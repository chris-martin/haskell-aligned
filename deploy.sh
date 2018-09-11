#!/usr/bin/env bash

set -eu

GITHUB_URL=git@github.com:chris-martin/haskell-aligned.git

SITE=$(mktemp -d)
cp aligned.html $SITE/index.html

pushd "$SITE"
git init
git add .
git commit --message "Deploy to GitHub Pages"
git push --force $GITHUB_URL HEAD:gh-pages
