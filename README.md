# readFilePerformance

performance optimization

## build and run

### Nix

    nix-shell --attr env

Run with profiling

    cabal run --enable-profiling readFilePerformance -- +RTS -hc -s

### Stack

    stack run --profile -- +RTS -hc -s

## Licence information

The word frequency information in the file `deu_news_2020_freq.txt` has been provided to me by the Natural Language Processing Group, Uni Leipzig. It is generated out of a corpus of 35 Million sentences and distributed under the [Creative Commons Attribution-NonCommercial 4.0 International Public Licence](https://creativecommons.org/licenses/by-nc/4.0/).

The file `german.utf8.dict` has its origin [here](https://sourceforge.net/projects/germandict/) and is public domain.
