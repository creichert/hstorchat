# HSTorChat

A TorChat implementation in Haskell and QML.

[![Build Status](https://secure.travis-ci.org/creichert/hstorchat.png?branch=master)](http://travis-ci.org/creichert/hstorchat)

## Configure and Build

```
    $ cabal sandbox init

    $ export PATH=$PWD/.cabal-sandbox/bin:$PATH

    $ cabal install c2hs
    $ cabal install --dependencies-only --enable-tests

    $ cabal configure

    $ cabal build
```

## Run

```
    $ tor -f torrc # Initialize the hidden_service/hostname file on first run.

    $ ./dist/build/hstorchat/hstorchat
```
