```
cabal install
```
(with ~/.cabal/bin in your PATH)

then use like:
```
hard cabal run
```

to monitor the current directory (and subdirectories),
and kill-and-rerun the command when any files change.

Ignores .git and anything in .gitignore.