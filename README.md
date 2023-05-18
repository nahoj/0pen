`0pen` is a command to open media or other files with geeky specifications on the order in which they are opened.

0pen can either:
- interactively play files with MPlayer, VLC, or any given command (supports [Playtag](https://github.com/nahoj/playtag));
- output a list of files that can be opened e.g. with `feh` for images or as a standard M3U playlist by any media player.

## Example uses

Shuffle all files under `music/` (excluding dirs and files starting with `+`, `.`, `_`, or `~`) and open them in succession with MPlayer, looping indefinitely (reshuffle between each loop). Only play audio even for video files:

```shell
0pen.ml music/ --music
```

Play all files under `foo fighters/` once, in alphabetical / "file-manager" order, i.e., in lexical order except `9.mp3` comes before `10.mp3`:

```shell
0pen.ml foo\ fighters/ --alpha
```

Play all files under `comedy/`, including dirs and files starting with `+`, picking them indefinitely at random. Each subdirectory's probability of being picked is proportional to the s**q**uare root of the total number of (leaf) files it contains. As an exception, directories named in `ALL_CAPS` or starting with `@` have their children treated as if they were direct children of the parent directory.

```shell
0pen.ml comedy/ -+ --pick q
```

First play `toto/africa.mp3`, then randomly play files from `foo fighters/` and `toto/`. When picking files, both top-level directories `foo fighters/` and `toto/` are picked as often as a single file (**c**onstant weight, so here it's 50 % Foo Fighters and 50 % Toto), then all further levels of subdirectories weigh as much as the total number of files they contain (**l**inear weight):

```shell
0pen.ml foo\ fighters/ toto/ --pick c/l --first toto/africa.mp3
```

Find photos modified (or taken) less than a year ago to open them as a full-screen slideshow. With each directory sorted **a**lphabetically, move by the **g**olden angle 1-1/φ ≈ 0.38 of the length of the list between picking files, which maximizes diversity. Pick series of 3 consecutive photos if possible (Files are considered consecutive if they differ only by a single number. For instance, `IMG_101.jpg` and `IMG_105.jpg` are consecutive but `IMG_106_Cat.jpg` isn't.):

```shell
find photos/ -mtime -365 | 0pen.ml --pick ag --series 3 | feh -Ff -
```

Shuffle files found under `comedy/`, write a list of the first 100 files, and open the list with VLC (0pen can also open each file in VLC separately with `--vlc`.):

```shell
0pen.ml comedy/ -n 100 >tmp.m3u8 && vlc tmp.m3u8
```

For a detailed list of available options, see `0pen --help`.

## Install

0pen requires OCaml 4.14 with Camomile 1.x, Ocamlfind, dolog, magic-mime, and ubase.

Using `opam` on Ubuntu, installing from the ground up could look something like:

```shell
sudo apt install opam
opam init
opam switch create 4.14.1
opam install 'camomile<2' ocamlfind dolog magic-mime ubase

# and then, for instance
ln -s /path/to/0pen.ml ~/bin/
```

## FAQ
### What does the name mean?

To open files indefinitely, like `mplayer -loop 0`. I pronounce it "zeropen".
