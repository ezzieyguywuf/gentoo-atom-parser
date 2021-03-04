Motivation
==========

This project is intended as a proof-of-concept. The intent is to show how the
version specification laid out in the [Gentoo Package Manager Specificion,
section 3.2](https://dev.gentoo.org/~ulm/pms/head/pms.html#x1-250003.2) can be
parsed.

Building
========

It shouldn't be too hard to do if you have `cabal` installed (you probably want
to use [ghcup](https://www.haskell.org/ghcup/) to get it if you don't, it's the
official thingy)

```sh
git clone https://github.com/ezzieyguywuf/gentoo-atom-parser.git
cd gentoo-atom-parser
cabal build
```

Usage
=====

Currently it doesn't actually do anything. I'll update this when it does. In the
meantime, you can run the executable with:

```sh
cabal run exe:gentoo-atom-parser
```
