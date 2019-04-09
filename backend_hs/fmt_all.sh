#!/usr/bin/env bash

find src test app -name "*.hs" -exec stylish-haskell -i {} \;