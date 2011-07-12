import core.stdc.stdio, std.conv, std.typetuple, std.typecons;

enum int WIDTH = 7;
enum int HEIGHT = 6;
enum int ORANGE_WINS = 1_000_000;
enum int YELLOW_WINS = -ORANGE_WINS;

int g_maxDepth = 7;
bool g_debug = false;

// manual optimization for missing DMD loop unwinding
template Range(int stop) {
    static if (stop <= 0)
        alias TypeTuple!() Range;
    else
        alias TypeTuple!(Range!(stop-1), stop-1) Range;
}

enum MyCell : int {
    Barren = 0,
    Orange = 1,
    Yellow = -1
}

alias MyCell[WIDTH][HEIGHT] Board;

int scoreBoard(const ref Board board) nothrow {
    int[9] counters;

    // Horizontal spans
    foreach (y; 0 .. HEIGHT) {
        int score = board[y][0] + board[y][1] + board[y][2];
        foreach (x; 3 .. WIDTH) {
            score += board[y][x];
            counters[score + 4]++;
            score -= board[y][x - 3];
        }
    }

    // Vertical spans
    foreach (x; 0 .. WIDTH) {
        int score = board[0][x] + board[1][x] + board[2][x];
        foreach (y; 3 .. HEIGHT) {
            score += board[y][x];
            counters[score + 4]++;
            score -= board[y - 3][x];
        }
    }

    // Down-right (and up-left) diagonals
    foreach (y; 0 .. HEIGHT - 3) {
        foreach (x; Range!(WIDTH - 3)) {
            int score = 0;
            foreach (idx; Range!4) {
                const int yy = y + idx;
                const int xx = x + idx;
                score += board[yy][xx];
            }
            counters[score + 4]++;
        }
    }

    // up-right (and down-left) diagonals
    foreach (y; 3 .. HEIGHT) {
        foreach (x; Range!(WIDTH - 3)) {
            int score = 0;
            foreach (idx; Range!4) {
                const int yy = y - idx;
                const int xx = x + idx;
                score += board[yy][xx];
            }
            counters[score + 4]++;
        }
    }

    if (counters[0] != 0)
        return YELLOW_WINS;
    else if (counters[8] != 0)
        return ORANGE_WINS;
    else return counters[5] + 2 * counters[6] + 5 * counters[7] +
                10 * counters[8] - counters[3] - 2 * counters[2] -
                5 * counters[1] - 10 * counters[0];
}

int dropDisk(ref Board board, in int column, in MyCell color) pure nothrow {
    foreach_reverse (y; 0 .. HEIGHT)
        if (board[y][column] == MyCell.Barren) {
            board[y][column] = color;
            return y;
        }
    return -1;
}

Board loadBoard(in string[] args) {
    Board newBoard;

    foreach (i, arg; args[1 .. $]) {
        if (arg[0] == 'o' || arg[0] == 'y')
            newBoard[arg[1] - '0'][arg[2] - '0'] =
                (arg[0] == 'o') ? MyCell.Orange : MyCell.Yellow;
        else if (arg == "-debug")
            g_debug = true;
        else if (arg == "-level" && i < (args.length - 2))
            g_maxDepth = to!int(args[i + 2]);
    }

    return newBoard;
}

Tuple!(int,"move", int,"score") abMinimax(in bool maximizeOrMinimize, in MyCell color,
                                          in int depth, ref Board board) {
    if (depth == 0) {
        return typeof(return)(-1, scoreBoard(board));
    } else {
        int bestScore = maximizeOrMinimize ? -10_000_000 : 10_000_000;
        int bestMove = -1;
        foreach (column; Range!WIDTH) {
            if (board[0][column] != MyCell.Barren)
                continue;
            int rowFilled = dropDisk(board, column, color);
            if (rowFilled == -1)
                continue;
            int s = scoreBoard(board);
            if (s == (maximizeOrMinimize ? ORANGE_WINS : YELLOW_WINS)) {
                bestMove = column;
                bestScore = s;
                board[rowFilled][column] = MyCell.Barren;
                break;
            }

            auto res = abMinimax(!maximizeOrMinimize,
                                 color == MyCell.Orange ? MyCell.Yellow : MyCell.Orange,
                                 depth - 1, board);
            board[rowFilled][column] = MyCell.Barren;
            if (depth == g_maxDepth && g_debug)
                printf("Depth %d, placing on %d, score:%d\n", depth, column, res.score);
            if (maximizeOrMinimize) {
                if (res.score >= bestScore) {
                    bestScore = res.score;
                    bestMove = column;
                }
            } else {
                if (res.score <= bestScore) {
                    bestScore = res.score;
                    bestMove = column;
                }
            }
        }

        return typeof(return)(bestMove, bestScore);
    }
}

int main(string[] args) {
    Board board = loadBoard(args);
    int scoreOrig = scoreBoard(board);

    if (scoreOrig == ORANGE_WINS) {
        puts("I win.");
        return -1;
    } else if (scoreOrig == YELLOW_WINS) {
        puts("You win.");
        return -1;
    } else {
        auto res = abMinimax(true, MyCell.Orange, g_maxDepth, board);

        if (res.move != -1) {
            printf("%d\n", res.move);
            dropDisk(board, res.move, MyCell.Orange);
            scoreOrig = scoreBoard(board);
            if (scoreOrig == ORANGE_WINS) {
                puts("I win.");
                return -1;
            } else if (scoreOrig == YELLOW_WINS) {
                puts("You win.");
                return -1;
            } else
                return 0;
        } else {
            puts("No move possible.");
            return -1;
        }
    }
}
