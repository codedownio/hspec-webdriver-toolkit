#!/bin/bash

# If this script succeeds, then the project can be built with both stack and cabal

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Update cabal.project.freeze (needs yq to pull resolver from stack.yaml)
RESOLVER=$(cat $SCRIPTDIR/stack.yaml | yq -r .resolver)
curl https://www.stackage.org/${RESOLVER}/cabal.config > $SCRIPTDIR/cabal.project.freeze

# Build with stack. This will update hspec-webdriver-toolkit.cabal
stack build

# Build with cabal
cabal build
