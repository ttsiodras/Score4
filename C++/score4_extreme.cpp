#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unordered_map>
#include <string>

const int width = 7;
const int height = 6;
const int orangeWins = 1000000;
const int yellowWins = -orangeWins;

int g_maxDepth = 7;

#define Orange 1
#define Yellow -1
#define Barren 0

// The 3 values above need 2 bits to represet (00,01,10)
// 7*2 = 14bits, which fit in 16bits of unsigned short
typedef unsigned short BoardData[height];

inline int GetCell(const BoardData& slots, int y, int x)
{
    // binary "AND" to extract the 2 bits
    // shift them down
    // subtract 1 (to get -1,0,1)
    return (((int)((slots[y] & (3<<(2*x)))>>(2*x)))-1);
}

inline void SetCell(BoardData& slots, int y, int x, char color)
{
    // binary "AND" to clear the 2 bits
    // add 1 to color (0,1,2), and "OR" it in
    slots[y] &= ~(3<<(2*x));
    slots[y] |= (color+1)<<(2*x);
}

struct Board {
    BoardData _slots;
    // The constructor resets the board to empty
    // 0->1 therefore all the 2-bits are 01b
    // so all shorts are 01010101b => 0x55
    Board() { memset(_slots, 0x55, sizeof(_slots)); }
};

struct BoardHashFunction {
    ::std::size_t operator ()(const Board& data) const {
        // The 32-bit size_t will be formed from
        // 3 pairs of unsigned shorts
        const unsigned short *p = data._slots;
        std::size_t hash=*p++; hash^=(*p++ << 16);
        hash^=*p++; hash^=(*p++ << 16);
        hash^=*p++; hash^=(*p++ << 16);
        return hash;
    }
};

struct BoardEqual {
    bool operator ()(const Board& a, const Board& b) const {
        const unsigned *p1=(unsigned *)a._slots;
        const unsigned *p2=(unsigned *)b._slots;
        // Unrolled comparison of 6 shorts is faster than memcmp
        // Even better: 6 shorts = 3 unsigned
        if (*p1++ != *p2++) return false;
        if (*p1++ != *p2++) return false;
        if (*p1++ != *p2++) return false;
        return true;
    }
};

// Cache to store the calculated scores of a board
typedef std::unordered_map<Board,int,BoardHashFunction,BoardEqual> MyCache;
MyCache scoreCache;

// statistics - they show how important the cache is! (it is VERY important)
int hits=0,losses=0;

int ScoreBoard(const Board& board)
{
    // Check the cache first
    MyCache::iterator it = scoreCache.find(board);
    if (scoreCache.end() != it) {
        hits++;
        return it->second;
    }
    losses++;

    // Not found, we must calculate the score from scratch.
    int counters[9] = {0,0,0,0,0,0,0,0,0};

    const BoardData& scores = board._slots;

    // Horizontal spans
    for(int y=0; y<height; y++) {
        int score = GetCell(scores,y,0) + GetCell(scores,y,1) + GetCell(scores,y,2);
        for(int x=3; x<width; x++) {
            score += GetCell(scores,y,x);
            counters[score+4]++;
            score -= GetCell(scores,y,x-3);
        }
    }
    // Vertical spans
    for(int x=0; x<width; x++) {
        int score = GetCell(scores,0,x) + GetCell(scores,1,x) + GetCell(scores,2,x);
        for(int y=3; y<height; y++) {
            score += GetCell(scores,y,x);
            if (score<-4 || score>4)
                abort();
            counters[score+4]++;
            score -= GetCell(scores,y-3,x);
        }
    }
    // Down-right (and up-left) diagonals
    for(int y=0; y<height-3; y++) {
        for(int x=0; x<width-3; x++) {
            int score = 0;
            for(int idx=0; idx<4; idx++) {
                score += GetCell(scores,y+idx,x+idx);
            }
            counters[score+4]++;
        }
    }
    // up-right (and down-left) diagonals
    for(int y=3; y<height; y++) {
        for(int x=0; x<width-3; x++) {
            int score = 0;
            for(int idx=0; idx<4; idx++) {
                score += GetCell(scores,y-idx,x+idx);
            }
            counters[score+4]++;
        }
    }
    int finalScore;
    if (counters[0] != 0)
        finalScore = yellowWins;
    else if (counters[8] != 0)
        finalScore = orangeWins;
    else
        finalScore =
            counters[5] + 2*counters[6] + 5*counters[7] -
            counters[3] - 2*counters[2] - 5*counters[1];
    // Store in cache, so we never have to recalculate this board again
    scoreCache[board] = finalScore;
    return finalScore;
}

int dropDisk(Board& board, int column, char color)
{
    for (int y=height-1; y>=0; y--)
        if (GetCell(board._slots,y,column) == Barren) {
            SetCell(board._slots,y,column,color);
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
            SetCell(  newBoard._slots,
                    (argv[i][1]-'0'),
                    argv[i][2]-'0',
                    (argv[i][0] == 'o')?Orange:Yellow);
        else if (!strcmp(argv[i], "-debug"))
            g_debug = 1;
        else if (!strcmp(argv[i], "-level"))
            g_maxDepth = atoi(argv[i+1]);
    return newBoard;
}

void abMinimax(bool maximizeOrMinimize, char color, int depth, Board& board, int& move, int& score)
{
    int bestScore=maximizeOrMinimize?-10000000:10000000;
    int bestMove=-1;
    for (int column=0; column<width; column++) {
        if (GetCell(board._slots,0,column)!=Barren) continue;
        int rowFilled = dropDisk(board, column, color);
        if (rowFilled == -1)
            continue;
        int s = ScoreBoard(board);
        if (s == (maximizeOrMinimize?orangeWins:yellowWins)) {
            bestMove = column;
            bestScore = s;
            SetCell(board._slots,rowFilled,column,Barren);
            break;
        }
        int moveInner, scoreInner;
        if (depth>1)
            abMinimax(!maximizeOrMinimize, color==Orange?Yellow:Orange, depth-1, board, moveInner, scoreInner);
        else {
            moveInner = -1;
            scoreInner = s;
        }
        SetCell(board._slots,rowFilled,column, Barren);
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
        if (g_debug) printf("Cached:%d Processed:%d\n", hits, losses);
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
