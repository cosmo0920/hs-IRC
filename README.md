hs-IRC
===

[![Build Status](https://travis-ci.org/cosmo0920/hs-IRC.png?branch=master)](https://travis-ci.org/cosmo0920/hs-IRC)

Simple IRC bot writen in Haskell.

This bot's source code was made in reference to [haskellwiki/Roll_your_own_IRC_bot](http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot)

## Requirement

* Haskell Platform 2012.1.0.0 or later

### currently, it can build following platforms:

* ghc 7.4.1 @ Ubuntu 12.04.3 LTS
* ghc 7.6.3 __with hsenv__ @ Ubuntu 12.04.3.LTS
* ghc 7.6.3 @ Windows 7 and 8
* ghc 7.6.3 @ OSX 10.8
   - Lexical error occurred when it installs dependent libraries, it must define `LANG=C` to avoid this error.

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

### Haddock

This code contains haddock style comments.

So, you can generate haddock document as follows:

```bash
$ cabal[-dev] haddock --executables [--hyperlink-source] # if you want to see highlighted code in document.
```

* * * *

### Settings

setting.yml contains follwing items:

* server
    - server URL
* port
    - port number
* nick
    - nickname
* password
    - password (It does not need always.)
* channel
    - connecting channel info
* realname
    - realname setting (Mixed case is not allowed.)
* usessl
    - use SSL/TLS (true/false)

* * * *

This code is provided under the MIT LICENSE. see: [LICENSE](LICENSE)
