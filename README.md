# file-grouper

This program groups the files according to some predicates.

## Installation

To install this program, you can use `cabal` or `stack`. If you use `stack`, you can simply run the following command:

```sh
stack install
```

### Autocompletion

You can enable autocompletion of this program by adding this line in your `.bashrc`:

```sh
eval "$(file-grouper --bash-completion-script file-grouper)"
```

## Options

To view the full options, you can run the following command:

```sh
file-grouper --help
```

which will display the following prompt:

```
file-grouper - Group your files

Usage: file-grouper [--type TYPE] [--pattern PATTERN] [--maxdepth DEPTH]
                    [--mindepth DEPTH] (--name | --last-accessed ([--year] |
                    [--month] | [--day] | [--custom PATTERN]) | --last-modified
                    ([--year] | [--month] | [--day] | [--custom PATTERN]))
                    [-i|--input PATH] [-o|--output DESTINATION] [--version]
  Group files in folders.

Available options:
  --type TYPE              The filetype to match when searching for file. The
                           following types are available: b Block device c
                           Character device f Regular file l Symbolic link p
                           Named pipe s Socket
  --pattern PATTERN        The glob pattern to match when searching for files.
  --maxdepth DEPTH         The maximum depth to use when recursing.
  --mindepth DEPTH         The minimum depth to use when recursing.
  --name                   Group the files by their names.
  --last-accessed          Group the files by the last time they were accessed.
  --year                   Create year named folders.
  --month                  Create month named folders.
  --day                    Create day named folders.
  --custom PATTERN         Create custom named folders. To see the available
                           options, see the following page:
                           https://hackage.haskell.org/package/time-1.8.0.3/docs/Data-Time-Format.html
  --last-modified          Group the files by the last time they were modified.
  --year                   Create year named folders.
  --month                  Create month named folders.
  --day                    Create day named folders.
  --custom PATTERN         Create custom named folders. To see the available
                           options, see the following page:
                           https://hackage.haskell.org/package/time-1.8.0.3/docs/Data-Time-Format.html
  -i,--input PATH          The folder that contains the files.
  -o,--output DESTINATION  The folder where to put the sorted files.
  -h,--help                Show this help text
  --version                Show program's version.
```
