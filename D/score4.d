// Compile with -debug too to see the debug printout.

import std.stdio, std.conv, std.typetuple, std.typecons, std.ascii;

// Manual optimization for missing DMD compiler loop unwinding.
// Probably not needed with LDC and GDC compilers.
template Range(int stop) {
    static if (stop <= 0)
        alias TypeTuple!() Range;
    else
        alias TypeTuple!(Range!(stop-1), stop-1) Range;
}

enum int noMove = -1;


struct Score4 {
    enum size_t width = 7;
    enum size_t height = 6;
    static assert(width >= 4 && height >= 4);
    enum int orangeWins = 1_000_000;
    enum int yellowWins = -orangeWins;
    enum int boardIsFullError = -1;
    enum int noMovePossible = -1;

    enum Cell : int { Empty = 0, Orange = 1, Yellow = -1 }

    const int maxDepth;

    // One padding Cell in rows gives a bit higher performance.
    Cell[width][height] board;


    this(in string[] args) nothrow {
        static assert(width <= 9 && height <= 9,
            "One digit isn't enough to represent row or column.");
        maxDepth = 7;

        // Load the board.
        foreach (i, arg; args[1 .. $]) {
            if (arg.length == 3 && (arg[0] == 'o' || arg[0] == 'y') &&
                isDigit(arg[1]) && isDigit(arg[2]))
                board[arg[1] - '0'][arg[2] - '0'] =
                    (arg[0] == 'o') ? Cell.Orange : Cell.Yellow;
            else if (arg == "-level" && i < (args.length - 2)) {
                try
                    maxDepth = to!int(args[i + 2]); // not yet pure @safe
                catch (Exception e) // ConvException
                    assert(0, "Invalid level value.");
            } else {
                puts("Invalid command line argument");
                puts("(This program doesn't support the -debug");
                puts("command line argument at run-time).");
                assert(0);
            }
        }
    }


    /// Compute the score of the current board.
    int computeBoardScore() const pure nothrow {
        int[9] counters; // index in [-4, 4]

        // Horizontal spans.
        foreach (y; 0 .. height) {
            int score = board[y][0] + board[y][1] + board[y][2];
            foreach (x; 3 .. width) {
                score += board[y][x];
                counters[score + 4]++;
                score -= board[y][x - 3];
            }
        }

        // Vertical spans.
        foreach (x; 0 .. width) {
            int score = board[0][x] + board[1][x] + board[2][x];
            foreach (y; 3 .. height) {
                score += board[y][x];
                counters[score + 4]++;
                score -= board[y - 3][x];
            }
        }

        // Down-right (and up-left) diagonals.
        foreach (y; 0 .. height - 3) {
            foreach (x; Range!(width - 3)) {
                int score = 0;
                foreach (i; Range!4)
                    score += board[y + i][x + i];
                counters[score + 4]++;
            }
        }

        // Up-right (and down-left) diagonals.
        foreach (y; 3 .. height) {
            foreach (x; Range!(width - 3)) {
                int score = 0;
                foreach (i; Range!4)
                    score += board[y - i][x + i];
                counters[score + 4]++;
            }
        }

        if (counters[0] != 0)
            return yellowWins;
        else if (counters[8] != 0)
            return orangeWins;
        else return      counters[5] + 2  * counters[6] + 5 * counters[7] +
                    10 * counters[8] -      counters[3] - 2 * counters[2] -
                     5 * counters[1] - 10 * counters[0];
    }


    /// Add (drop) a disk to a column of the board.
    int dropDisk(in int column, in Cell color) pure nothrow
    in {
        assert(column >= 0 && column < width);
    } out(y) {
        assert(y == boardIsFullError || (y >= 0 && y < height));
    } body {
        foreach_reverse (y; 0 .. height)
            if (board[y][column] == Cell.Empty) {
                board[y][column] = color;
                return cast(int)y;
            }
        return boardIsFullError;
    }


    Tuple!(int,"move", int,"score") abMinimax(in bool maximizeOrMinimize,
                                              in Cell color,
                                              in int depth) pure nothrow {
        if (depth <= 0)
            return typeof(return)(noMovePossible, computeBoardScore());

        int bestMove = noMovePossible;
        int bestScore = maximizeOrMinimize ? int.min : int.max;

        foreach (column; Range!width) {
            int rowFilled = dropDisk(column, color);
            if (rowFilled == boardIsFullError)
                continue;
            int score = computeBoardScore();

            if (score == (maximizeOrMinimize ? orangeWins : yellowWins)) {
                bestMove = column;
                bestScore = score;
                board[rowFilled][column] = Cell.Empty;
                break;
            }

	    if (depth>1) {
		auto res = abMinimax(!maximizeOrMinimize,
                                     color == Cell.Orange ? Cell.Yellow : Cell.Orange,
                                     depth - 1);
		score = res.score;
	    }
            board[rowFilled][column] = Cell.Empty;

            // when loss is certain, avoid forfeiting the match,
            // by shifting scores by depth...
            if (score == orangeWins || score == yellowWins)
                score -= depth * cast(int)color;

            if (depth == maxDepth) // DMD BUG 6319
                debug printf("Depth %d, placing on %d, score:%d\n",
                             depth, column, score);

            if (maximizeOrMinimize) {
                if (score >= bestScore) { // > is enough
                    bestScore = score;
                    bestMove = column;
                }
            } else {
                if (score <= bestScore) { // < is enough
                    bestScore = score;
                    bestMove = column;
                }
            }
        }

        return typeof(return)(bestMove, bestScore);
    }


    Tuple!(int,"move", string,"comment") play() pure nothrow {
        int scoreOrig = computeBoardScore();

        if (scoreOrig == orangeWins)
            return typeof(return)(noMove, "I win.");
        else if (scoreOrig == yellowWins)
            return typeof(return)(noMove, "You win.");

        auto result = abMinimax(/*do maximize*/ true,
                                Cell.Orange,
                                /*depth*/ maxDepth);
        if (result.move == noMovePossible)
            return typeof(return)(noMove, "No move possible.");
        else {
            dropDisk(result.move, Cell.Orange);
            scoreOrig = computeBoardScore();

            if (scoreOrig == orangeWins)
                return typeof(return)(result.move, "I win.");
            else if (scoreOrig == yellowWins)
                return typeof(return)(result.move, "You win.");
            else
                return typeof(return)(result.move, "");
        }
    }
}


void main(in string[] args) {
    auto s4 = Score4(args);
    auto result = s4.play();
    if (result.move != noMove)
        writeln(result.move);
    writeln(result.comment);
}
