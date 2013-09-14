## hs-IRC

Simple IRC bot writen in Haskell.

This bot's source code was made in reference to [haskellwiki/Roll_your_own_IRC_bot](http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot)

## Try it

If you install cabal packages, __strongly recommended__ use cabal-dev.

### git clone

```bash
$ git clone https://github.com/cosmo0920/hs-IRC.git
$ cd hs-IRC
```

### Install dependent libraries

when you use __Debian and related distributions...__

```bash
$ cabal update
$ cabal install cabal-dev
$ ~/.cabal/bin/cabal-dev install --dry-run --only-dependencies #prevent dependency hell
$ ~/.cabal/bin/cabal-dev install --only-dependencies
```

### build application

```bash
$ ~/.cabal/bin/cabal-dev configure
$ ~/.cabal/bin/cabal-dev build
$ cp setting-dummy.json setting.json
```

* * * *

This code is provided under the MIT LICENSE. see: [LICENSE](LICENSE)
