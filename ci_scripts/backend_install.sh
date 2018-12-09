#!/usr/bin/env bash

cd backend

cabal v2-update
cabal v2-build --only-dependencies all
