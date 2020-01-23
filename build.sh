#!/bin/bash

# If this script succeeds, then the project can be built with both stack and cabal

# Build with stack. This will update hspec-webdriver-toolkit.cabal
stack build

# Build with cabal
stack2cabal
cabal build
