#!/usr/bin/env bash

cd backend_hs

stack --no-terminal --install-ghc test --only-dependencies
