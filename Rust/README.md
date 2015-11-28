Everybody talks about Rust these days, so I decided to dive in.
Here's my experience so far, after 24h with the language - porting
my Score4 AI engine.

Executive summary
-----------------

It works.

But in my humble opinion, it's a lot less clear than my OCaml version - the one
I wrote 4 years ago when I started coding in functional style.

To see for yourself, here's
[the minimax function in OCaml](https://github.com/ttsiodras/Score4/blob/master/OCaml/score4_functional.ml#L38),
and here's
[the minimax function in Rust](https://github.com/ttsiodras/Score4/blob/master/Rust/src/main.rs#L94),

- Notice how `.iter()` are required, because Lists and Vectors and other collections can't
be `map`-ed or `filter`-ed - you need to get an iterator from them.
- Notice how I need to provide type specs to help the compiler when `collect`-ing.
- As for the references thing... at some point I forgot what I was doing (i.e. the algorithm)
  and got lost in the minutiae of what's a reference and what isn't... the Rust compiler
  complained about requiring stars, ampersands and eventually, double ampersands (not joking,
  look at the code)... I responded by fixing what it was complaining about:
  "I want an & here" - "OK, here's one"...  

And 3h later, I ended up with double ampersands :-) 

The error messages were also quite complex - try removing the `.collect()`s - e.g. changing...

    let moves_and_boards: LinkedList<_> = valid_moves.iter().map(
        |column| (*column, drop_disk(board, *column, color))).collect();

to the way it looks in OCaml, i.e.

    let moves_and_boards = valid_moves.map(
        |column| (*column, drop_disk(board, *column, color)));

Do this in all the 5 places where we `filter` and `map` in the code ...and then marvel at
the errors spat out by the Rust compiler.

Who's afraid of C++ template errors?  You ain't seen nothing yet! :-)

I believe the most trouble came from `.map` and `.filter` and friends returning `iter`-ables,
and not the collections they came from. In OCaml it's fine to "pipe" the results of
`List.filter` to `List.map`... where as here you get really nasty error messages
 unless you `.collect()` , specify some kind of type for the result (e.g. `LinkedList<_>`),
and then `.iter()` all over again - because the collections (LinkedList, Vec, etc)
have no `.map` or `.filter` (what?! Why?)

I am probably missing something here.

Then again, I've only played with Rust in the last 24h
------------------------------------------------------
To be fair, though, I am a complete newbie in Rust - literally one day old. I am hoping
the experts in /r/rust will show me the error of my ways and help me shape this into something
a lot cleaner - and ideally, a lot closer (verbosity-wise) to
[the ML version](https://github.com/ttsiodras/Score4/blob/master/OCaml/score4_functional.ml#L38).

Patches most welcome! Best way to learn.

*EDIT, a week later: Received valuable feedback from kind people in /r/rust - updated the
code accordingly. Almost all the usize casts are now gone, and the use of early returns
avoids the rightward drift that naturally occurs if I only use `if` expressions.*

Correctness and speed - the good news...
----------------------------------------
- Rust does share a very good trait with OCaml... Just like my first OCaml code 4 years ago,
  once I managed to compile my Rust code, it run correctly - the first time I executed it.

- And... the generated binary is much faster than OCaml, since Rust has no garbage collection
  to waste CPU on. Rust also uses LLVM, so it re-uses 100s of man-years of optimizing
  know-how.

*EDIT, 2 days later: Two kind people in /r/rust (masklinn and clippy-rust) suggested I use
`Vec` instead of `LinkedList` (much-improved cache locality) and use `.iter().max()/min()`
instead of Fold-ing on `allData`. The result is a spectacular 2x, placing Rust close
enough to "touch" the speed of the imperative camp - very impressive.*

I must now find the time to update the OCaml implementations to use "compact" vectors
instead of lists - to make the comparison fair again :-)

Current results as of Nov 22, 2015 on my MBAir running ArchLinux:

    ======================
    = Running benchmarks =
    ======================
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
    ...

