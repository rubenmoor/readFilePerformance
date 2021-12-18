# readFilePerformance

performance optimization

## build and run

### Nix

    nix-shell --attr env

Run with profiling

    cabal run --enable-profiling readFilePerformance -- +RTS -hc -s

### Stack

    stack run --profile -- +RTS -hc -s

## tweaking the garbage collector

    $ for ghcVersion in ghc8107 ghc901 ghc921; do; nix-shell --attr env --argstr ghcStr $ghcVersion --run "/usr/bin/env time -f '%M %e s' cabal run readFilePerformance -- +RTS -s --copying-gc"; done
    $ for ghcVersion in ghc8107 ghc901 ghc921; do; nix-shell --attr env --argstr ghcStr $ghcVersion --run "/usr/bin/env time -f '%M %e s' cabal run readFilePerformance -- +RTS -s --nonmoving-gc"; done

| ghc   | gc       | peak mem | runtime |
|-------|----------|----------|---------|
| 8.10.7| copying  | 1.13 GB  | 8.9s   |
| 8.10.7| nonmoving| 1.91 GB  | 9.4s   |
| 9.0.1 | copying  | 1.12 GB  | 9.0s   |
| 9.0.1 | nonmoving| 1.47 GB  | 9.4s   |
| 9.2.1 | copying  | 1.23 GB  | 8.9s   |
| 9.2.1 | nonmoving| 1.57 GB  | 9.5s   |

### ghc-8.10.7, --copying-gc

```
  10,854,192,904 bytes allocated in the heap
   4,168,751,200 bytes copied during GC
     483,539,712 bytes maximum residency (12 sample(s))
       2,217,216 bytes maximum slop
            1099 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10286 colls,     0 par    1.849s   1.851s     0.0002s    0.0007s
  Gen  1        12 colls,     0 par    1.014s   1.014s     0.0845s    0.2666s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    4.700s  (  4.746s elapsed)
  GC      time    2.863s  (  2.866s elapsed)
  EXIT    time    0.000s  (  0.008s elapsed)
  Total   time    7.563s  (  7.620s elapsed)

  Alloc rate    2,309,560,431 bytes per MUT second

  Productivity  62.1% of total user, 62.3% of total elapsed
```

### ghc-8.10.7, --nonmoving-gc

```
  10,854,191,096 bytes allocated in the heap
   3,014,050,232 bytes copied during GC
   1,359,623,712 bytes maximum residency (35 sample(s))
18,446,744,073,709,367,944 bytes maximum slop
            2253 MiB total memory in use (436 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10263 colls,     0 par    2.946s   2.949s     0.0003s    0.0011s
  Gen  1        35 colls,     0 par    0.041s   0.041s     0.0012s    0.0148s
  Gen  1        35 syncs,                       0.040s     0.0012s    0.0261s
  Gen  1      concurrent,              3.057s   6.547s     0.1871s    2.2230s

  TASKS: 38 (35 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    5.292s  (  5.338s elapsed)
  GC      time    2.987s  (  2.990s elapsed)
  CONC GC time    3.057s  (  6.547s elapsed)
  EXIT    time    0.000s  (  0.002s elapsed)
  Total   time   11.335s  (  8.330s elapsed)

  Alloc rate    2,051,224,999 bytes per MUT second

  Productivity  73.6% of total user, 64.1% of total elapsed
```

### ghc-9.0.1, --copying-gc

```
  11,142,965,792 bytes allocated in the heap
   4,160,725,696 bytes copied during GC
     481,143,792 bytes maximum residency (12 sample(s))
       2,229,264 bytes maximum slop
            1095 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10566 colls,     0 par    1.917s   1.920s     0.0002s    0.0008s
  Gen  1        12 colls,     0 par    1.017s   1.017s     0.0847s    0.2659s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    4.750s  (  4.797s elapsed)
  GC      time    2.934s  (  2.937s elapsed)
  EXIT    time    0.001s  (  0.006s elapsed)
  Total   time    7.685s  (  7.740s elapsed)

  Alloc rate    2,345,705,836 bytes per MUT second

  Productivity  61.8% of total user, 62.0% of total elapsed
```

### ghc-9.0.1, --nonmoving-gc

```
  11,142,964,104 bytes allocated in the heap
   3,006,568,272 bytes copied during GC
     994,390,544 bytes maximum residency (38 sample(s))
18,446,744,073,708,941,944 bytes maximum slop
            1680 MiB total memory in use (4 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10540 colls,     0 par    2.944s   2.948s     0.0003s    0.0015s
  Gen  1        38 colls,     0 par    0.026s   0.026s     0.0007s    0.0019s
  Gen  1        38 syncs,                       0.022s     0.0006s    0.0083s
  Gen  1      concurrent,              3.136s   6.654s     0.1751s    1.2637s

  TASKS: 41 (38 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.000s elapsed)
  MUT     time    5.362s  (  5.409s elapsed)
  GC      time    2.971s  (  2.974s elapsed)
  CONC GC time    3.136s  (  6.654s elapsed)
  EXIT    time    0.000s  (  0.007s elapsed)
  Total   time   11.470s  (  8.390s elapsed)

  Alloc rate    2,077,983,578 bytes per MUT second

  Productivity  74.1% of total user, 64.5% of total elapsed
```

### ghc-9.2.1, --copying-gc

```
  11,142,998,064 bytes allocated in the heap
   4,018,312,024 bytes copied during GC
     539,641,000 bytes maximum residency (11 sample(s))
       2,390,872 bytes maximum slop
            1197 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2635 colls,     0 par    1.877s   1.878s     0.0007s    0.0032s
  Gen  1        11 colls,     0 par    0.988s   0.988s     0.0898s    0.3087s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    4.729s  (  4.779s elapsed)
  GC      time    2.864s  (  2.866s elapsed)
  EXIT    time    0.000s  (  0.005s elapsed)
  Total   time    7.594s  (  7.650s elapsed)

  Alloc rate    2,356,120,004 bytes per MUT second

  Productivity  62.3% of total user, 62.5% of total elapsed
```

### ghc-9.2.1, --nonmoving-gc

```
  11,142,996,392 bytes allocated in the heap
   2,935,283,184 bytes copied during GC
   1,059,977,008 bytes maximum residency (29 sample(s))
18,446,744,073,708,492,376 bytes maximum slop
            1802 MiB total memory in use (342 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2617 colls,     0 par    3.008s   3.010s     0.0011s    0.0056s
  Gen  1        29 colls,     0 par    0.072s   0.072s     0.0025s    0.0097s
  Gen  1        29 syncs,                       0.099s     0.0034s    0.0536s
  Gen  1      concurrent,              2.818s   6.257s     0.2158s    1.1607s

  TASKS: 33 (30 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    5.300s  (  5.350s elapsed)
  GC      time    3.081s  (  3.082s elapsed)
  CONC GC time    2.818s  (  6.257s elapsed)
  EXIT    time    0.001s  (  0.008s elapsed)
  Total   time   11.201s  (  8.440s elapsed)

  Alloc rate    2,102,289,662 bytes per MUT second

  Productivity  72.5% of total user, 63.4% of total elapsed
```

## Licence information

The word frequency information in the file `deu_news_2020_freq.txt` has been provided to me by the Natural Language Processing Group, Uni Leipzig. It is generated out of a corpus of 35 Million sentences and distributed under the [Creative Commons Attribution-NonCommercial 4.0 International Public Licence](https://creativecommons.org/licenses/by-nc/4.0/).

The file `german.utf8.dict` has its origin [here](https://sourceforge.net/projects/germandict/) and is public domain.
