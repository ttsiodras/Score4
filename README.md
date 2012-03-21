This is the code accompanying [my blog post](http://users.softlab.ntua.gr/~ttsiod/score4.html) about implementing Score4 (also called Connect4). The goal is to form horizontal, vertical or diagonal series of 4 of your chips (green). The computer tries to do the same, using red chips.

I've coded implementations in C, C++, OCaml, F#, C# and Lisp, using both functional and imperative styles of coding.  Ports also came from all over the Web to many more languages (so far: Java, Python, Haskell, Go, D). *Update, March 2012: **pypy** does an amazing job optimizing Python, bringing it up to the same level of performance as the rest*.

I've also [prepared a standalone windows binary](https://github.com/downloads/ttsiodras/Score4/Score4-win32.binary.zip) of the game (using py2exe for the PyGame Python GUI, and the C version of the game engine).

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
