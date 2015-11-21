Everybody talks about Rust these days, so I decided to dive in.
Here's my experience so far, after 24h with the language - porting
my Score4 AI engine.

Executive summary
-----------------

It works.

But...

...it's a lot less clear than my OCaml version - the one
I wrote 4 years ago when I started coding in functional style.

To see for yourself, here's
[the minimax function in OCaml](https://github.com/ttsiodras/Score4/blob/master/OCaml/score4_functional.ml#L38),
and here's
[the minimax function in Rust](https://github.com/ttsiodras/Score4/blob/master/Rust/src/main.rs#L109),

At some point I forgot what I was doing (i.e. the algorithm) and got lost
in the minutiae... with the Rust compiler barking about requiring stars, ampersands and
double ampersands (not joking, see the code)... I just followed blindly what the compiler was
complaining about: "I want an & here" - "OK, here's one"...

And 3h later, I ended up with double ampersands :-) 

Collect or die
--------------
Another highlight - try removing the `.collect`s - i.e. changing...

    let moves_and_boards: LinkedList<_> = valid_moves.iter().map(
        |column| (*column, drop_disk(board, *column, color))).collect();

to the way it looks in OCaml, i.e.

    let moves_and_boards = valid_moves.map(
        |column| (*column, drop_disk(board, *column, color)));

Do this in all the 5 places `filter`ing and `map`ing in the code ...and then marvel at
the errors spat out by the Rust compiler.

Who's afraid of C++ template errors?  You ain't seen nothing yet!

I believe the issue comes from `.map` and `.filter` and friends returning `iter`-ables,
and not the collections they came from - so in OCaml it's fine to "pipe" the results of
`List.filter` to `List.map`... where as here you get *Error Messages from Lord Voldemort (TM)*
 unless you `.collect()` , specify a `LinkedList<_>` type for the result, and then `.iter()`
all over again - because LinkedList has no `.map` or `.filter` (what?!)

I am surely missing something here.

usize forever
-------------
Converting to `usize` was also very annoying - search for `usize` in `src/main.rs` to see
how many times I had to do this. I am hoping, again, that I am missing something.

Never mind, I've only played with Rust in the last 24h
------------------------------------------------------
Then again, in all fairness, I am a complete newbie in Rust - literally one day old. I am hoping
the experts in /r/rust will show me the error of my ways and help me make this much closer to
[the ML version](https://github.com/ttsiodras/Score4/blob/master/OCaml/score4_functional.ml#L38).

Patches most welcome! Best way to learn.

P.S. The good news...
---------------------
Rust does share a trait with OCaml... Just like my first experiences with OCaml 4 years ago,
once I managed to compile my Rust code, it run correctly - the first time I executed it.
