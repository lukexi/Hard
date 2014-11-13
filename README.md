```
cabal install
```
(with ~/.cabal/bin in your PATH)

Optionally, add a ```.hard``` file to your directory with extensions like
```
.hs
.frag
.vert
```
to only rebuild your project when a file with one of those extensions changes.

then use like:
```
hard cabal run
```

to monitor the current directory (and subdirectories),
and kill-and-rerun the command when any files change.

Ignores .git and anything in .gitignore.