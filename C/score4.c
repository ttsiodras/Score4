#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WIDTH 7
#define HEIGHT 6
#define ORANGE_WINS  1000000
#define YELLOW_WINS -1000000
#define ORANGE 1
#define YELLOW -1
#define BARREN 0

int g_maxDepth = 7;

int inside(int y, int x)
{
    return y>=0 && y<HEIGHT && x>=0 && x<WIDTH;
}

int ScoreBoard(int scores[][WIDTH])
{
    int counters[9] = {0,0,0,0,0,0,0,0,0};
    int x,y;

    // Horizontal spans
    for(y=0; y<HEIGHT; y++) {
        int score = scores[y][0] + scores[y][1] + scores[y][2];
        for(x=3; x<WIDTH; x++) {
            score += scores[y][x];
            counters[score+4]++;
            score -= scores[y][x-3];
        }
    }
    // Vertical spans
    for(x=0; x<WIDTH; x++) {
        int score = scores[0][x] + scores[1][x] + scores[2][x];
        for(y=3; y<HEIGHT; y++) {
            score += scores[y][x];
            counters[score+4]++;
            score -= scores[y-3][x];
        }
    }
    // Down-right (and up-left) diagonals
    for(y=0; y<HEIGHT-3; y++) {
        for(x=0; x<WIDTH-3; x++) {
            int score=0, idx=0;
            for(idx=0; idx<4; idx++) {
                score += scores[y+idx][x+idx];
            }
            counters[score+4]++;
        }
    }
    // up-right (and down-left) diagonals
    for(y=3; y<HEIGHT; y++) {
        for(x=0; x<WIDTH-3; x++) {
            int score=0, idx=0;
            for(idx=0; idx<4; idx++) {
                score += scores[y-idx][x+idx];
            }
            counters[score+4]++;
        }
    }
    if (counters[0] != 0)
        return YELLOW_WINS;
    else if (counters[8] != 0)
        return ORANGE_WINS;
    else 
        return 
            counters[5] + 2*counters[6] + 5*counters[7] + 10*counters[8] -
            counters[3] - 2*counters[2] - 5*counters[1] - 10*counters[0];
}

int dropDisk(int board[][WIDTH], int column, int color)
{
    int y;
    for (y=HEIGHT-1; y>=0; y--)
        if (board[y][column] == BARREN) {
            board[y][column] = color;
            return y;
        }
    return -1;
}

int g_debug = 0;

void loadBoard(int argc, char *argv[], int board[][WIDTH]) 
{
    int i;
    for(i=1; i<argc; i++)
        if (argv[i][0] == 'o' || argv[i][0] == 'y')
            board[argv[i][1]-'0'][argv[i][2]-'0'] = (argv[i][0] == 'o')?ORANGE:YELLOW;
        else if (!strcmp(argv[i], "-debug"))
            g_debug = 1;
        else if (!strcmp(argv[i], "-level"))
            g_maxDepth = atoi(argv[i+1]);
}

void abMinimax(int maximizeOrMinimize, int color, int depth, int board[][WIDTH], int* move, int* score)
{
    int bestScore=maximizeOrMinimize?-10000000:10000000;
    int bestMove=-1, column;
    for (column=0; column<WIDTH; column++) {
        if (board[0][column]!=BARREN) continue;
        int rowFilled = dropDisk(board, column, color);
        if (rowFilled == -1)
            continue;
        int s = ScoreBoard(board);
        if (s == (maximizeOrMinimize?ORANGE_WINS:YELLOW_WINS)) {
            bestMove = column;
            bestScore = s;
            board[rowFilled][column] = BARREN;
            break;
        }
        int moveInner, scoreInner;
        if (depth>1)
            abMinimax(!maximizeOrMinimize, color==ORANGE?YELLOW:ORANGE, depth-1, board, &moveInner, &scoreInner);
        else {
            moveInner = -1;
            scoreInner = s;
        }
        board[rowFilled][column] = BARREN;
        /* when loss is certain, avoid forfeiting the match, by shifting scores by depth... */
        if (scoreInner == ORANGE_WINS || scoreInner == YELLOW_WINS)
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
    *move = bestMove;
    *score = bestScore;
}

int main(int argc, char *argv[])
{
    int board[HEIGHT][WIDTH];
    memset(board, 0, sizeof(board));

    loadBoard(argc, argv, board);
    int scoreOrig = ScoreBoard(board);
    if (scoreOrig == ORANGE_WINS) { puts("I win\n"); exit(-1); }
    else if (scoreOrig == YELLOW_WINS) { puts("You win\n"); exit(-1); }
    else {
        int move, score;
        abMinimax(1,ORANGE,g_maxDepth,board,&move,&score);
        if (move != -1) {
            printf("%d\n",move);
            dropDisk(board, move, ORANGE);
            scoreOrig = ScoreBoard(board);
            if (scoreOrig == ORANGE_WINS) { puts("I win\n"); exit(-1); }
            else if (scoreOrig == YELLOW_WINS) { puts("You win\n"); exit(-1); }
            else exit(0);
        } else {
            puts("No move possible");
            exit(-1);
        }
    }
    return 0;
}

// vim: set expandtab ts=8 sts=4 shiftwidth=4 
