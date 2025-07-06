# trout

![](trout.png)

A chess engine in Haskell.

If you check the commit history you can see this was started a long, long time ago.
When I first started this engine I spent way too much time on making the game-management (moving, move generation, etc.) part fast for perft.
This was a terrible mistake because that section actually has a relatively small impact on overall strength.
Most comes from search features like pruning, which actually makes a Haskell chess engine more viable than you might think.
(Haskell is not usually used for extremely performance-sensitive/based applications like chess engines, and in my experience is relatively difficult to get predictable performance from.)

The two other decent ones I've found are [turncoat](https://github.com/albertprz/turncoat) and [Barbarossa](https://github.com/nionita/Barbarossa), check them out!

## Features

- Magic bitboards
- Principal variation search
- Null move pruning
- Reverse futility pruning
- Late move reductions
- Check extensions
- Quiescence search
- Transposition table move ordering
- Killer heuristic move ordering
- History heuristic move ordering
- Static exchange evaluation move ordering
- PeSTO PST + mobility eval
- [En passant](https://en.wikipedia.org/wiki/En_passant)

## Running

Install [Stack](https://docs.haskellstack.org/en/stable/) (like through [GHCup](https://www.haskell.org/ghcup/)) and run `stack run`.

The executable is... somewhere in the `.stack-work` directory after running or building.

Alternatively, I *think* you might be able to run the project with cabal only using stack's generated `trout.cabal`?
It should work but I don't use cabal personally so YMMV.

## Limitations

- Very limited subset of UCI supported for now (e.g. no options yet)
- Sometimes times out with `go movetime <time>` or under time pressure because it is slow and not very accurate when terminating the search thread
