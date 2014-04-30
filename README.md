# HSTorChat

A TorChat implementation in Haskell and QML.

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

## Notes

This implementation can send and receive messages from with TorChat clients.
Support for adding buddies coming soon. Received messages from a new buddy add
that buddy to the buddy list.
