#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>

const int width = 7;
const int height = 6;
const int orangeWins = 1000000;
const int yellowWins = -orangeWins;

int g_maxDepth = 7;

enum Mycell {
    Orange=1,
    Yellow=-1, // See starting comment below in ScoreBoard
    Barren=0
};

struct Board {
    Mycell _slots[height][width];
    Board() {
        memset(_slots, 0, sizeof(_slots));
    }
};

bool inside(int y, int x)
{
    return y>=0 && y<height && x>=0 && x<width;
}

int ScoreBoard(const Board& board)
{
    int counters[9] = {0,0,0,0,0,0,0,0,0};

    // C++ does not need a "translation stage", like this one:
    //int scores[height][width];
    //
    //for(int y=0; y<height; y++)
    //    for(int x=0; x<width; x++)
    //        switch(board._slots[y][x]) {
    //        case Orange:
    //            scores[y][x] = 1; break;
    //        case Yellow:
    //            scores[y][x] = -1; break;
    //        case Barren:
    //            scores[y][x] = 0; break;
    //        }
    //
    // Instead, enumerants can be accessed as integers:
    typedef Mycell EnumsAreIntegersInC[height][width];
    const EnumsAreIntegersInC& scores = board._slots;

    // Horizontal spans
    for(int y=0; y<height; y++) {
        int score = scores[y][0] + scores[y][1] + scores[y][2];
        for(int x=3; x<width; x++) {
            assert(inside(y,x));
            score += scores[y][x];
            counters[score+4]++;
            assert(inside(y,x-3));
            score -= scores[y][x-3];
        }
    }
    // Vertical spans
    for(int x=0; x<width; x++) {
        int score = scores[0][x] + scores[1][x] + scores[2][x];
        for(int y=3; y<height; y++) {
            assert(inside(y,x));
            score += scores[y][x];
            counters[score+4]++;
            assert(inside(y-3,x));
            score -= scores[y-3][x];
        }
    }
    // Down-right (and up-left) diagonals
    for(int y=0; y<height-3; y++) {
        for(int x=0; x<width-3; x++) {
            int score = 0;
            for(int idx=0; idx<4; idx++) {
                int yy = y + idx;
                int xx = x + idx;
                assert(inside(yy,xx));
                score += scores[yy][xx];
            }
            counters[score+4]++;
        }
    }
    // up-right (and down-left) diagonals
    for(int y=3; y<height; y++) {
        for(int x=0; x<width-3; x++) {
            int score = 0;
            for(int idx=0; idx<4; idx++) {
                int yy = y - idx;
                int xx = x + idx;
                assert(inside(yy,xx));
                score += scores[yy][xx];
            }
            counters[score+4]++;
        }
    }
/*
For down-right diagonals, I also tried this incremental version of the 
diagonal scores calculations... It is doing less computation than
the alternative above, but unfortunately, the use of the tuple array
makes the overall results worse in my Celeron E3400... I suspect
because the access to the array triggers cache misses.

    static const char dl[][2] = { {0,3},{0,4},{0,5},{0,6},{1,6},{2,6} };
    for(int idx=0; idx<6; idx++) {
        int y = dl[idx][0];
        int x = dl[idx][1];
        assert(inside(y,x));
        int score = scores[y][x] + scores[y+1][x-1] + scores[y+2][x-2];
        while (((y+3)<height) && (x-3)>=0) {
            assert(inside(y+3,x-3));
            score += scores[y+3][x-3];
            counters[score+4]++;
            score -= scores[y][x];
            y++; x--;
        }
    }
*/
    if (counters[0] != 0)
        return yellowWins;
    else if (counters[8] != 0)
        return orangeWins;
    else 
        return 
            counters[5] + 2*counters[6] + 5*counters[7] + 10*counters[8] -
            counters[3] - 2*counters[2] - 5*counters[1] - 10*counters[0];
}

int dropDisk(Board& board, int column, Mycell color)
{
    for (int y=height-1; y>=0; y--)
        if (board._slots[y][column] == Barren) {
            board._slots[y][column] = color;
            return y;
        }
    return -1;
}

int g_debug = 0;

Board loadBoard(int argc, char *argv[]) 
{
    Board newBoard;
    for(int i=1; i<argc; i++)
        if (argv[i][0] == 'o' || argv[i][0] == 'y')
            newBoard._slots[argv[i][1]-'0'][argv[i][2]-'0'] = (argv[i][0] == 'o')?Orange:Yellow;
        else if (!strcmp(argv[i], "-debug"))
            g_debug = 1;
        else if (!strcmp(argv[i], "-level"))
            g_maxDepth = atoi(argv[i+1]);
    return newBoard;
}

void abMinimax(bool maximizeOrMinimize, Mycell color, int depth, Board& board, int& move, int& score)
{
    int bestScore=maximizeOrMinimize?-10000000:10000000;
    int bestMove=-1;
    for (int column=0; column<width; column++) {
        if (board._slots[0][column]!=Barren) continue;
        int rowFilled = dropDisk(board, column, color);
        if (rowFilled == -1)
            continue;
        int s = ScoreBoard(board);
        if (s == (maximizeOrMinimize?orangeWins:yellowWins)) {
            bestMove = column;
            bestScore = s;
            board._slots[rowFilled][column] = Barren;
            break;
        }
        int moveInner, scoreInner;
        if (depth>1)
            abMinimax(!maximizeOrMinimize, color==Orange?Yellow:Orange, depth-1, board, moveInner, scoreInner);
        else {
            moveInner = -1;
            scoreInner = s;
        }
        board._slots[rowFilled][column] = Barren;
        /* when loss is certain, avoid forfeiting the match, by shifting scores by depth... */
        if (scoreInner == orangeWins || scoreInner == yellowWins)
            scoreInner -= depth * (int)color;
        if (depth == g_maxDepth && g_debug)
            printf("Depth %d, placing on %d, score:%d\n", depth, column, scoreInner);
        if (maximizeOrMinimize) {
            if (scoreInner>=bestScore) {
                bestScore = scoreInner;
                bestMove = column;
            }
        } else {
            if (scoreInner<=bestScore) {
                bestScore = scoreInner;
                bestMove = column;
            }
        }
    }
    move = bestMove;
    score = bestScore;
}

int main(int argc, char *argv[])
{
    Board board = loadBoard(argc, argv);
    int scoreOrig = ScoreBoard(board);
    if (scoreOrig == orangeWins) { puts("I win\n"); exit(-1); }
    else if (scoreOrig == yellowWins) { puts("You win\n"); exit(-1); }
    else {
        int move, score;
        abMinimax(true,Orange,g_maxDepth,board,move,score);
        if (move != -1) {
            printf("%d\n",move);
            dropDisk(board, move, Orange);
            scoreOrig = ScoreBoard(board);
            if (scoreOrig == orangeWins) { puts("I win\n"); exit(-1); }
            else if (scoreOrig == yellowWins) { puts("You win\n"); exit(-1); }
            else exit(0);
        } else {
            puts("No move possible");
            exit(-1);
        }
    }
    return 0;
}

// vim: set expandtab ts=8 sts=4 shiftwidth=4 
