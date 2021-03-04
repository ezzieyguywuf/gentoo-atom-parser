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

Results
=======

It's currently only a proof-of-concept, so it doesn't accept command-line
arguments or anything. You can add more atoms to try to parse to Main.hs 

Here's some sample output/results though:

```sh
Hello, Haskell!
Trying to parse dev-lib/foo-1.0
    Category     = dev-lib
    Package Name = foo
    Version      = 1.0
    suffixes     = 
    revision     = r0

Trying to parse dev-lib/bar-100dpi-2.0
    Category     = dev-lib
    Package Name = bar
    Version      = 100d
    suffixes     = 
    revision     = r0

Trying to parse dev-lib/baz-3g
    Category     = dev-lib
    Package Name = baz
    Version      = 3g
    suffixes     = 
    revision     = r0

```
