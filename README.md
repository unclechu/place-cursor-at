# place-cursor-at

Utility for X11 that helps to move cursor using only keyboard
written in Haskell.

I made it for myself to use with
[xmonad](https://github.com/unclechu/xmonadrc)
and [i3](https://github.com/unclechu/i3rc).

![Screenshot](./screenshot.png)

## Requirements

- [Haskell Tool Stack](https://haskellstack.org/)
- Development files of `libX11`

## How to use

```bash
stack build
stack exec place-cursor-at
```

You could install `place-cursor-at` binary to `~/.local/bin` directory
(make sure you have this directory in your `PATH` environment variable):

```bash
stack install
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
