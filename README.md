# readFilePerformance

performance optimization

## build and run

### Nix

    nix-shell --attr env

Run with profiling

    cabal new-run --enable-profiling readFilePerformance -- +RTS -hc -s

### Stack

    stack run --profile -- +RTS -hc -s
