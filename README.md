# Games on Hydra

An experiment to build distributed and decentralised games running on top of Cardano Layer 2, [Hydra](https://hydra.family).
This project started with [Black Jack](https://en.wikipedia.org/wiki/Blackjack) and then switched to focus on _Chess_ which is a much more interesting game.

> [!WARNING]
> This project is a work-in-progress and experimental, it's not meant (yet?) to be used for real playing on mainnet and it's advised to only run it on test networks.
> The code follows a duct-tape-and-staples design strategy which basically means I hack stuff until they work just enough to make some progress, then move on. Furthermore, it suffers from premature generalisation as I attempted to decouple from the interaction with the execution and settlement layer too early.

## Status

* [x] Rules [in PlutusTx](./black-jack-core/src/Chess/Game.hs) test-driven in [Haskell](./black-jack-core/test/Chess/GameSpec.hs)
* [x] Barebones Console based interface
* [x] ~~Mock server simulating lifecycle on Hydra (for testing purpose)~~
* [x] Basic [Plutus](./black-jack-core/src/Chess/Contract.hs) smart contract
* [x] Create Cardano transactions using only [cardano-cli](https://github.com/IntersectMBO/cardano-cli)
* [x] Advanced smart contract: Check play is valid for any given game state
* [ ] ~~Proper "randomness"~~
* [x] Startup & configure Hydra server in the background
* [x] Startup & configure cardano-node in the background
* [ ] Use mithril to bootstrap Cardano-node
* [x] Support for 2-players
* [ ] User guide
* [ ] Provide pre-compiled binaries
* [ ] Web UI

# Installation

> [!WARNING]
> The only tested and supported operating systems are currently Linux (and more precisely Debian-ish distros) and Mac OS X.
> There are of course plans to support other operating systems and in particular Windows.

The only currently supported method for installing hydra-chess is to build it from source.

## Build from source

*Requirements*:
* Haskell toolchain: Checkout [ghcup](https://www.haskell.org/ghcup/) which is a great tool to provision a Haskell toolchain. hydra-chess requires GHC 9.6 and Cabal 3.10
* System libraries: Install [libffi](https://sourceware.org/libffi/), [libiconv](https://www.gnu.org/software/libiconv/), and `libz`
* Custom libraries: `hydra-chess` still depends on some custom native libraries for compilation, checkout the [cardano-node installation](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md) page for instructions on how to install those, in particular `libsodium` and `libsecp256k1`.

Assuming that all dependencies are installed, then do:

```
$ cabal install hydra-chess
```

to build all the components of `hydra-chess` and install binaries in the `~/.cabal/bin` directory.

# Why?

The ability to play games in a safe and decentralised way is one
possible use case of blockchains. However, Layer 1 is way too
expensive and slow to enable a good gaming experience, hence the need
to rely on Layer 2 to speed things up and make it cheap and affordable
to play games with other people.

Having contributed to [Hydra](https://hydra.family) since its
inception, I also wanted to start experimenting with how one could
build such a system and application to make it reasonably easy to use
Hydra for gaming purpose.

The [Experience Report](./2023-experience-report.md) contains some
thoughts about Cardano, Hydra, DApps development, stemming from this
experiment.
