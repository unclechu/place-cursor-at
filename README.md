# place-cursor-at

Utility for X11 that helps to move cursor using only keyboard
written in Haskell.

I made it for myself to use with
[xmonad](https://github.com/unclechu/xmonadrc)
and [i3](https://github.com/unclechu/i3rc).

![Screenshot](./screenshot.png)

## Requirements

- Either [Nix Package Manager] or [The Haskell Tool Stack]
- `libX11` and `libXinerama` with development files

## How to use

### With Nix

See [Nix Package Manager].

Write a `shell.nix` like this one:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  place-cursor-at = import (
    let c = "e8d624f056f2fe386f97c2f3828a08581e1fd909"; in fetchTarball {
      url = "https://github.com/unclechu/place-cursor-at/archive/${c}.tar.gz";
      sha256 = "0qviy1xmazg0299lgx6rsh540amls6rrm4p55iclah0pswqix1jn";
    }
  ) {};
in
  pkgs.mkShell { buildInputs = [ place-cursor-at ]; }
```

Where `c` is a git commit hash, take latest from `master` branch,
this may be and probably is outdated, just an example.
You would also have to fix `sha256`, Nix will tell
you it mismatches and show you the actual hash-sum.

Then run `nix-shell --run place-cursor-at`.

### With Stack

See [The Haskell Tool Stack].

```bash
stack build
stack exec place-cursor-at
```

You could install `place-cursor-at` binary to `~/.local/bin` directory
(make sure you have this directory in your `PATH` environment variable):

```bash
stack install
```

#### NixOS note

Under NixOS if you for some reason decide to use Stack you would be forced to
use `--system-ghc`. You may call each of the command from above with this flag
of just define this alias before running anything else:

```bash
alias stack='stack --system-ghc'
```

## Using it as script

It could be started as [stack](https://haskellstack.org/)-script:

```bash
src/place-cursor-at.hs
```

But it is supposed to be used very often and to be very responsive,
so it's better to precompile it to reduce startup time.

## Xinerama note

By default it appears and do its stuff on display where your mouse cursor is.
But you can specify which display it should appear on:

```bash
place-cursor-at 1
```

Or on third display:

```bash
place-cursor-at 3
```

## Jump to specific position without GUI

You can immediately jump to specific position of a screen without showing any
GUI, like this:

```bash
place-cursor-at LT
```

It's case-insensitive, this also would work:

```bash
place-cursor-at lt
```

And you also can specify a display you want to jump to:

```bash
place-cursor-at lt 1
```

In any order of arguments:

```bash
place-cursor-at 1 lt
```

### Codes of available positions

| Code | Deciphering           |
| -    | -                     |
| `LT` | **L**eft-**T**op      |
| `LC` | **L**eft-**C**enter   |
| `LB` | **L**eft-**B**ottom   |
| `CT` | **C**enter-**T**op    |
| `CC` | **C**enter-**C**enter |
| `CB` | **C**enter-**B**ottom |
| `RT` | **R**ight-**T**op     |
| `RC` | **R**ight-**C**enter  |
| `RB` | **R**ight-**B**ottom  |

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[GNU/GPLv3](./LICENSE)

[The Haskell Tool Stack]: https://docs.haskellstack.org/en/stable/README/
[Nix Package Manager]: https://nixos.org/nix/manual/#ch-about-nix
