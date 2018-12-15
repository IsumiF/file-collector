#!/usr/bin/env bash

cd backend

stack --no-terminal --install-ghc test --only-dependencies
