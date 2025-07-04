# trout

![](trout.png)

A chess engine in Haskell. Haskell, as it turns out, is not the best language to write a chess engine in.

## Features

- Magic bitboards
- Principal variation search
- Null move pruning
- Quiescence search
- Transposition table move ordering
- Killer heuristic move ordering
- History heuristic move ordering
- Static exchange evaluation move ordering
- PeSTO eval
- [En passant](https://en.wikipedia.org/wiki/En_passant)

## Running

Install [Stack](https://docs.haskellstack.org/en/stable/) (like through [GHCup](https://www.haskell.org/ghcup/)) and run `stack run`.

The executable is... somewhere in the `.stack-work` directory after running or building.

Alternatively, I *think* you might be able to run the project with cabal only using stack's generated `trout.cabal`?
It should work but I don't use cabal personally so YMMV.

## Limitations

- Very limited subset of UCI supported for now (e.g. no options yet)
- Sometimes times out with `go movetime <time>` or under time pressure because it is slow and not very accurate when terminating the search thread
