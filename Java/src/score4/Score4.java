/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package score4;

import java.util.ArrayList;

/**
 *
 * @author sario
 */
public class Score4 {

    static final int width = 7;
    static final int height = 6;
    static final int orangeWins = 1000000;
    static final int yellowWins = -orangeWins;
    static int g_maxDepth = 7;
    static boolean g_debug = false;

    static enum Mycell {

        Orange,
        Yellow,
        Barren;
    }

    static class Board {

        final int width;
        final int height;
        Mycell[][] slots;

        Board(int width, int height) {
            slots = new Mycell[height][width];
            this.width = width;
            this.height = height;
            fill(Mycell.Barren);
        }
        
        private void fill(Mycell type) {
            for (int j = 0; j < this.height; j++) {
                for (int i = 0; i < this.width; i++) {
                    slots[j][i] = type;
                }
            }
        }

        boolean inside(int y, int x) {
            return y >= 0 && y < this.height && x >= 0 && x < this.width;
        }

        int dropDisk(int column, Mycell color) {
            for (int y = this.height - 1; y >= 0; y--) {
                if (slots[y][column] == Mycell.Barren) {
                    slots[y][column] = color;
                    return y;
                }
            }
            return -1;
        }

        void load(Integer[] x, Integer[] y, Mycell[] color) {
            assert ((x.length == y.length) && (x.length == color.length));
            for (int i = 0; i < x.length; i++) {
                slots[y[i]][x[i]] = color[i];
            }
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Board board = prepareBoard(args);

        int scoreOrig = scoreBoard(board);
        if (scoreOrig == orangeWins) {
            System.out.println("I win");
            System.exit(-1);
        } else if (scoreOrig == yellowWins) {
            System.out.println("You win");
            System.exit(-1);
        } else {
            ScoreMoveTuple smt = abMinimax(true, Mycell.Orange, g_maxDepth, board);
            if (smt.move != -1) {
                System.out.println(smt.move);
                board.dropDisk(smt.move, Mycell.Orange);
                scoreOrig = scoreBoard(board);
                if (scoreOrig == orangeWins) {
                    System.out.println("I win");
                    System.exit(-1);
                } else if (scoreOrig == yellowWins) {
                    System.out.println("You win");
                    System.exit(-1);
                } else {
                    System.exit(0);
                }
            } else {
                System.out.println("No move possible!");
                System.exit(-1);
            }
        }
    }

    private static Board prepareBoard(String[] args) {
        Board board = new Board(width, height);
        ArrayList<Mycell> colors = new ArrayList<Mycell>(9);
        ArrayList<Integer> x = new ArrayList<Integer>(9);
        ArrayList<Integer> y = new ArrayList<Integer>(9);
        int i = 0;
        for (String arg : args) {
            char[] cargs = arg.toCharArray();
            if (arg.length() == 3) {
                if (cargs[0] == 'o') {
                    colors.add(Mycell.Orange);
                    y.add(cargs[1] - '0');
                    x.add(cargs[2] - '0');
                } else if (cargs[0] == 'y') {
                    colors.add(Mycell.Yellow);
                    y.add(cargs[1] - '0');
                    x.add(cargs[2] - '0');
                }
            } else if (arg.equalsIgnoreCase("-debug")) {
                g_debug = true;
            } else if (arg.equalsIgnoreCase("-level")) {
                g_maxDepth = Integer.getInteger(args[i + 1]);
            }
            i++;
        }
        board.load(x.toArray(new Integer[x.size()]), y.toArray(new Integer[y.size()]), colors.toArray(new Mycell[colors.size()]));
        return board;
    }

    private static int scoreBoard(Board board) {
        int[][] scores = new int[height][width];
        int[] counters = {0, 0, 0, 0, 0, 0, 0, 0, 0};
        final int[][] negativeSlope = {{0, 0}, {1, 1}, {2, 2}, {3, 3}};
        final int[][] positiveSlope = {{0, 0}, {-1, 1}, {-2, 2}, {-3, 3}};

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                switch (board.slots[y][x]) {
                    case Orange:
                        scores[y][x] = 1;
                        break;
                    case Yellow:
                        scores[y][x] = -1;
                        break;
                    case Barren:
                        scores[y][x] = 0;
                        break;
                }
            }
        }
        // Horizontal spans
        for (int y = 0; y < height; y++) {
            int score = scores[y][0] + scores[y][1] + scores[y][2];
            for (int x = 3; x < width; x++) {
                assert (board.inside(y, x));
                score += scores[y][x];
                counters[score + 4]++;
                assert (board.inside(y, x - 3));
                score -= scores[y][x - 3];
            }
        }
        // Vertical spans
        for (int x = 0; x < width; x++) {
            int score = scores[0][x] + scores[1][x] + scores[2][x];
            for (int y = 3; y < height; y++) {
                assert (board.inside(y, x));
                score += scores[y][x];
                counters[score + 4]++;
                assert (board.inside(y - 3, x));
                score -= scores[y - 3][x];
            }
        }
        // Down-right (and up-left) diagonals
        for (int y = 0; y < height - 3; y++) {
            for (int x = 0; x < width - 3; x++) {
                int score = 0;
                for (int idx = 0; idx < 4; idx++) {
                    int yy = y + negativeSlope[idx][0];
                    int xx = x + negativeSlope[idx][1];
                    assert (board.inside(yy, xx));
                    score += scores[yy][xx];
                }
                counters[score + 4]++;
            }
        }
        // up-right (and down-left) diagonals
        for (int y = 3; y < height; y++) {
            for (int x = 0; x < width - 3; x++) {
                int score = 0;
                for (int idx = 0; idx < 4; idx++) {
                    int yy = y + positiveSlope[idx][0];
                    int xx = x + positiveSlope[idx][1];
                    assert (board.inside(yy, xx));
                    score += scores[yy][xx];
                }
                counters[score + 4]++;
            }
        }

        if (counters[0] != 0) {
            return yellowWins;
        } else if (counters[8] != 0) {
            return orangeWins;
        } else {
            return counters[5] + 2 * counters[6] + 5 * counters[7] + 10 * counters[8]
                    - counters[3] - 2 * counters[2] - 5 * counters[1] - 10 * counters[0];
        }
    }

    private static ScoreMoveTuple abMinimax(boolean maximizeOrMinimize, Mycell color, int depth, Board board) {

        if (0 == depth) {
            ScoreMoveTuple smt = new ScoreMoveTuple();
            smt.score = scoreBoard(board);
            smt.move = -1;
            return smt;
        } else {
            int bestScore = maximizeOrMinimize ? -10000000 : 10000000;
            int bestMove = -1;
            for (int column = 0; column < width; column++) {
                if (board.slots[0][column] != Mycell.Barren) {
                    continue;
                }
                int rowFilled = board.dropDisk(column, color);
                if (rowFilled == -1) {
                    continue;
                }
                int s = scoreBoard(board);
                if (s == (maximizeOrMinimize ? orangeWins : yellowWins)) {
                    bestMove = column;
                    bestScore = s;
                    board.slots[rowFilled][column] = Mycell.Barren;
                    break;
                }
                Integer moveInner, scoreInner;
                ScoreMoveTuple inner = abMinimax(!maximizeOrMinimize, color == Mycell.Orange ? Mycell.Yellow : Mycell.Orange, depth - 1, board);
                assert (inner != null);
                moveInner = inner.move;
                scoreInner = inner.score;
                board.slots[rowFilled][column] = Mycell.Barren;
                if ((depth == g_maxDepth) && g_debug) {
                    System.out.printf("Depth %d, placing on %d, score:%d\n", depth, column, scoreInner);
                }
                if (maximizeOrMinimize) {
                    if (scoreInner >= bestScore) {
                        bestScore = scoreInner;
                        bestMove = column;
                    }
                } else {
                    if (scoreInner <= bestScore) {
                        bestScore = scoreInner;
                        bestMove = column;
                    }
                }
            }
            ScoreMoveTuple smt = new ScoreMoveTuple();
            smt.score = bestScore;
            smt.move = bestMove;
            return smt;
        }
    }

    static class ScoreMoveTuple {

        public Integer score;
        public Integer move;
    }
}
