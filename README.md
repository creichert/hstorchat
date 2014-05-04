# HSTorChat

A TorChat implementation in Haskell and QML.

[![Build Status](https://secure.travis-ci.org/creichert/hstorchat.png?branch=master)](http://travis-ci.org/creichert/hstorchat)

## Build

```
    $ cabal sandbox init
    $ cabal install --dependencies-only
```

## Run

```
    $ tor -f torrc # Initialize the hidden_service/hostname file on first run.
    $ ./dist/build/hstorchat/hstorchat
```
