#!/usr/bin/env bash

cd backend
cabal v2-build --enable-documentation all && cabal v2-test all
