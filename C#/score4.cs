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
            Yellow=-1,
            Barren=0
        };

        public class Board {
            // Initially, this was Mycell[,]
            // Unfortunately, C# 2D arrays are a lot slower
            // than simple arrays of arrays (Jagged arrays): Mycell[][]
	    // BUT
	    // using a 1D array is EVEN faster:
	    //    _slots[width*Y + X]
	    // is much faster than
	    //    _slots[Y][X]
	    //
	    // (sigh) Oh well, C# is a VM-based language (specifically, .NET).
	    // Running fast is not the primary concern in VMs...
            public Mycell[] _slots;
            public Board()
            {
               _slots = new Mycell[height*width];
            }
        };

        public static int ScoreBoard(Board board)
        {
            int[] counters = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };

            // Horizontal spans
            for (int y = 0; y < height; y++)
            {
                int score = (int) board._slots[width*(y)+0] + (int) board._slots[width*(y)+1] + (int) board._slots[width*(y)+2];
                for (int x = 3; x < width; x++)
                {
                    score += (int) board._slots[width*(y)+x];
                    counters[score + 4]++;
                    score -= (int) board._slots[width*(y)+x - 3];
                }
            }
            // Vertical spans
            for (int x = 0; x < width; x++)
            {
                int score = (int) board._slots[width*(0)+x] + (int) board._slots[width*(1)+x] + (int) board._slots[width*(2)+x];
                for (int y = 3; y < height; y++)
                {
                    score += (int) board._slots[width*(y)+x];
                    counters[score + 4]++;
                    score -= (int) board._slots[width*(y - 3)+x];
                }
            }
            // Down-right (and up-left) diagonals
            for (int y = 0; y < height - 3; y++)
            {
                for (int x = 0; x < width - 3; x++)
                {
                    int score = 0;
                    for (int ofs=0; ofs<4; ofs++)
                    {
                        int yy = y + ofs;
                        int xx = x + ofs;
                        score += (int) board._slots[width*(yy)+xx];
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
                    for (int ofs=0; ofs<4; ofs++)
                    {
                        int yy = y - ofs;
                        int xx = x + ofs;
                        score += (int) board._slots[width*(yy)+xx];
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
                if (board._slots[width*(y)+column] == Mycell.Barren) {
                    board._slots[width*(y)+column] = color;
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
                    newBoard._slots[width*(arg[1]-'0')+arg[2]-'0'] = (arg[0] == 'o')?Mycell.Orange:Mycell.Yellow;
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
                    if (board._slots[width*(0)+column]!=Mycell.Barren)
                        continue;
                    int rowFilled = dropDisk(board, column, color);
                    if (rowFilled == -1)
                        continue;
                    int s = ScoreBoard(board);
                    if (s == (maximizeOrMinimize?orangeWins:yellowWins)) {
                        bestMove = column;
                        bestScore = s;
                        board._slots[width*(rowFilled)+column] = Mycell.Barren;
                        break;
                    }
                    int moveInner, scoreInner;
		    if (depth>1)
			abMinimax(!maximizeOrMinimize, color==Mycell.Orange?Mycell.Yellow:Mycell.Orange, depth-1, board, out moveInner, out scoreInner);
		    else {
			moveInner = -1;
			scoreInner = s;
		    }
                    board._slots[width*(rowFilled)+column] = Mycell.Barren;
                    /* when loss is certain, avoid forfeiting the match, by shifting scores by depth... */
                    if (scoreInner == orangeWins || scoreInner == yellowWins)
                        scoreInner -= depth * (int)color;
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
