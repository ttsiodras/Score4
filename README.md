This is the code accompanying [my blog post](https://www.thanassis.space/score4.html) about implementing the game of *Score4* (also known as *Connect4*). The goal of the game is to form horizontal, vertical or diagonal series of 4 of your chips (green). The computer tries to do the same, using red chips.

I coded implementations of the game engine in C, C++, OCaml, F#, C#, Lisp and Rust, using both functional and imperative styles of coding.  Ports also came from all over the Web to many more languages (so far: Java, Python, Haskell, Go, D - see Hacker News discussion [here](http://news.ycombinator.com/item?id=2750894) and Reddit/programming discussion [here](http://www.reddit.com/r/programming/comments/imfqd/ai_playing_score4_in_functional_and_imperative/)). *Update, March 2012: **pypy** did an amazing job optimizing the Python version, bringing it up to performance levels similar to the rest of them*.

I also [prepared a standalone windows binary](https://github.com/downloads/ttsiodras/Score4/Score4-win32.binary.zip) of the game - using py2exe for the PyGame Python GUI, and a MinGW-compiled binary of the C version of the game engine.

To fiddle with the sources:

1. Checkout

2. Play or benchmark:

    - "make play" to play a graphics game of score4 (via PyGame)
    - "make playSimple" to play a console game of score4
    - "make benchmark" to benchmark the languages

3. The "make play" controls:

   - Click with mouse to drop a green chip on a column
   - ESCAPE to exit 
   - SPACE to start a new game.

4. To run the benchmark in only one of the languages, e.g. OCaml...

    cd OCaml ; make ; make test

5. To benchmark in all available languages:

    make benchmark

These are the results I get on my Celeron E3400 under Arch Linux:

    ======================
    = Running benchmarks =
    ======================
    Benchmarking imperative memoized C++ ... 0.087 sec
    Benchmarking imperative C++ ... 0.115 sec
    Benchmarking imperative C ... 0.120 sec
    Benchmarking imperative Lisp (SBCL) ... 0.272 sec
    Benchmarking imperative LISP (CMUCL) ... 0.270 sec
    Benchmarking imperative Java ... 0.385 sec
    Benchmarking imperative OCaml ... 0.306 sec
    Benchmarking imperative Python (Pypy) ... 0.638 sec
    Benchmarking functional OCaml ... 0.693 sec
    Benchmarking imperative C# ... 0.863 sec
    Benchmarking imperative F# ... 0.792 sec
    Benchmarking functional F# ... 1.958 sec

**Update, 3 years later** - Rust entered the game with
furious speed ; it's almost as fast as the imperative group but
the code is functional-style - ported from the OCaml version.
CMUCL is gone, too (can't find it in Arch's repos anymore).
Go and D are there too...

Results below from my MBair running Arch Linux:

    Benchmarking imperative memoized C++ ...: 0.072000
    Benchmarking imperative C ...: 0.085000
    Benchmarking imperative C++ ...: 0.088000
    Benchmarking imperative D ...: 0.139000
    Benchmarking imperative Lisp (SBCL) ...: 0.143000
    Benchmarking functional Rust ...: 0.155000
    Benchmarking imperative Java ...: 0.220000
    Benchmarking imperative OCaml ...: 0.230000
    Benchmarking functional OCaml ...: 0.434000
    Benchmarking imperative C# ...: 0.568000
    Benchmarking imperative F# ...: 0.590000
    Benchmarking imperative Go ...: 0.697000
    Benchmarking functional F# ...: 1.295000

**Update, 10 years later** - benchmark run on a Celeron N5095
under Arch Linux:

    Benchmarking imperative memoized C++ ...: 0.058000
    Benchmarking imperative C ...: 0.070000
    Benchmarking imperative C++ ...: 0.071000
    Benchmarking functional Rust ...: 0.126000
    Benchmarking imperative Lisp (SBCL) ...: 0.130000
    Benchmarking imperative D ...: 0.132000
    Benchmarking imperative Java ...: 0.251000
    Benchmarking imperative OCaml ...: 0.323000
    Benchmarking functional OCaml ...: 0.444000
    Benchmarking imperative Go ...: 0.491000
    Benchmarking imperative C# ...: 0.626000
    Benchmarking imperative F# ...: 0.693000
    Benchmarking functional F# ...: 1.064000
