# file-collector

[![Build Status](https://dev.azure.com/IsumiF/file-collector/_apis/build/status/IsumiF.file-collector?branchName=master)](https://dev.azure.com/IsumiF/file-collector/_build/latest?definitionId=1&branchName=master)

A website for uploading and collecting files. One use case is to collect
students' homework.

# Build from source

## Build the server

```sh
stack build file-collector-backend
```

The server executable will be located at
`$(stack path --local-install-root)/bin/server`

## Build the client

### GHC Version

```sh
stack build file-collector-frontend
```

### GHCJS Version

```sh
cabal new-build --project-file=cabal-ghcjs.project file-collector-frontend
```

## (Optional) Build Hoogle Database

```sh
stack hoogle --rebuild
# Run hoogle with the following command
stack hoogle -- server --local --port 9000
```
