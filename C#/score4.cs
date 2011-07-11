using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace score4
{
    class Program
    {
        public const int width = 7;
        public const int height = 6;
        public const int maxDepth = 7;
        public const int orangeWins = 1000000;
        public const int yellowWins = -orangeWins;

        public enum Mycell {
            Orange=1,
            Yellow=2,
            Barren=0
        };

        public class Board {
	    // Initially, this was Mycell[,]
	    // Unfortunately, C# 2D arrays are a lot slower
	    // than simple arrays of arrays (Jagged arrays):
            public Mycell[][] _slots;
	    public Board()
	    {
	       _slots = new Mycell[height][];
	       for(int i=0; i<height; i++)
		   _slots[i] = new Mycell[width];
            }
        };

        // diagonal offsets, down-right
        static int[][] negativeSlope = new int[4][] {
	    new int[2]{ 0, 0 },
	    new int[2]{ 1, 1 },
	    new int[2]{ 2, 2 },
	    new int[2]{ 3, 3 }
	};

        // diagonal offsets, up-right
        static int[][] positiveSlope = new int[4][] {
	    new int [2] {  0, 0 },
	    new int [2] { -1, 1 },
	    new int [2] { -2, 2 },
	    new int [2] { -3, 3 }
	};

	// Jagged array, made static to avoid re-initializing on every ScoreBoard call
        static int[][] scores = new int[height][] {
                new int[width], new int[width], new int[width],
                new int[width], new int[width], new int[width]
            };

        public static int ScoreBoard(Board board)
        {
            int[] counters = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };

            for (int y = 0; y < height; y++)
                for (int x = 0; x < width; x++)
                    switch (board._slots[y][x])
                    {
                        case Mycell.Orange:
                            scores[y][x] = 1; break;
                        case Mycell.Yellow:
                            scores[y][x] = -1; break;
                        case Mycell.Barren:
                            scores[y][x] = 0; break;
                    }

            // Horizontal spans
            for (int y = 0; y < height; y++)
            {
                int score = scores[y][0] + scores[y][1] + scores[y][2];
                for (int x = 3; x < width; x++)
                {
                    score += scores[y][x];
                    counters[score + 4]++;
                    score -= scores[y][x - 3];
                }
            }
            // Vertical spans
            for (int x = 0; x < width; x++)
            {
                int score = scores[0][x] + scores[1][x] + scores[2][x];
                for (int y = 3; y < height; y++)
                {
                    score += scores[y][x];
                    counters[score + 4]++;
                    score -= scores[y - 3][x];
                }
            }
            // Down-right (and up-left) diagonals
            for (int y = 0; y < height - 3; y++)
            {
                for (int x = 0; x < width - 3; x++)
                {
                    int score = 0;
                    foreach (var cellOffset in negativeSlope)
                    {
                        int yy = y + cellOffset[0];
                        int xx = x + cellOffset[1];
                        score += scores[yy][xx];
                    }
                    counters[score + 4]++;
                }
            }
            // up-right (and down-left) diagonals
            for (int y = 3; y < height; y++)
            {
                for (int x = 0; x < width - 3; x++)
                {
                    int score = 0;
                    foreach (var cellOfset in positiveSlope)
                    {
                        int yy = y + cellOfset[0];
                        int xx = x + cellOfset[1];
                        score += scores[yy][xx];
                    }
                    counters[score + 4]++;
                }
            }

            if (counters[0] != 0)
                return yellowWins;
            else if (counters[8] != 0)
                return orangeWins;
            else
                return
                    counters[5] + 2 * counters[6] + 5 * counters[7] + 10 * counters[8] -
                    counters[3] - 2 * counters[2] - 5 * counters[1] - 10 * counters[0];
        }

        public static int dropDisk(Board board, int column, Mycell color)
        {
            for (int y=height-1; y>=0; y--)
                if (board._slots[y][column] == Mycell.Barren) {
                    board._slots[y][column] = color;
                    return y;
            }
            return -1;
        }

        public static bool g_debug = false;

        public static Board loadBoard(string[] args)
        {
            Board newBoard = new Board();
            foreach(var arg in args)
                if (arg[0] == 'o' || arg[0] == 'y')
                    newBoard._slots[arg[1]-'0'][arg[2]-'0'] = (arg[0] == 'o')?Mycell.Orange:Mycell.Yellow;
            else if (arg == "-debug")
                g_debug = true;
            return newBoard;
        }

        public static void abMinimax(bool maximizeOrMinimize, Mycell color, int depth, Board board, out int move, out int score)
        {
            if (0 == depth) {
                move = -1;
                score = ScoreBoard(board);
            } else {
                int bestScore=maximizeOrMinimize?-10000000:10000000;
                int bestMove=-1;
                for (int column=0; column<width; column++) {
                    if (board._slots[0][column]!=Mycell.Barren)
                        continue;
                    int rowFilled = dropDisk(board, column, color);
                    if (rowFilled == -1)
                        continue;
                    int s = ScoreBoard(board);
                    if (s == (maximizeOrMinimize?orangeWins:yellowWins)) {
                        bestMove = column;
                        bestScore = s;
                        board._slots[rowFilled][column] = Mycell.Barren;
                        break;
                    }
                    int moveInner, scoreInner;
                    abMinimax(!maximizeOrMinimize, color==Mycell.Orange?Mycell.Yellow:Mycell.Orange, depth-1, board, out moveInner, out scoreInner);
                    board._slots[rowFilled][column] = Mycell.Barren;
                    if (depth == maxDepth && g_debug)
                        Console.WriteLine("Depth {0}, placing on {1}, score:{2}", depth, column, scoreInner);
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
        }


        public static void Main(string[] args)
        {
            Board board = loadBoard(args);
            int scoreOrig = ScoreBoard(board);
            if (scoreOrig == orangeWins) { Console.WriteLine("I win"); Environment.Exit(-1); }
            else if (scoreOrig == yellowWins) { Console.WriteLine("You win"); Environment.Exit(-1); }
            else {
                int move, score;
                abMinimax(true, Mycell.Orange, maxDepth, board, out move, out score);
                if (move != -1) {
                    Console.WriteLine("{0}", move);
                    dropDisk(board, move, Mycell.Orange);
                    scoreOrig = ScoreBoard(board);
                    if (scoreOrig == orangeWins) { Console.WriteLine("I win"); Environment.Exit(-1); }
                    else if (scoreOrig == yellowWins) { Console.WriteLine("You win\n"); Environment.Exit(-1); }
                    else Environment.Exit(0);
                } else {
                    Console.WriteLine("No move possible");
                    Environment.Exit(-1);
                }
            }
            Environment.Exit(0);
        }
    }
}
