use std::env;
use std::process;
//use std::collections::Vec;

mod common;

// Import constants

use common::WIDTH;
use common::HEIGHT;
use common::ORANGE_WINS;
use common::YELLOW_WINS;
use common::MAX_DEPTH;

// Import types

use common::Board;

// Import functions

use common::score_board;
use common::other_color;

// Parse cmdline specs to create initial board

fn load_board(args: Vec<String>) -> Board {
    let mut board = [[0; WIDTH]; HEIGHT];
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            let orange = format!("o{}{}", y, x);
            let yellow = format!("y{}{}", y, x);
            if args.iter().any(|x| *x == orange) {
                board[y][x] = 1i32;
            } else if args.iter().any(|x| *x == yellow) {
                board[y][x] = -1i32;
            } else {
                board[y][x] = 0i32;
            }
        }
    }
    board
}

// Drop a chip, return the new board

fn drop_disk(board: &Board, column:u32, color:i32) -> Board {
    let mut board_new: Board = [[0; WIDTH]; HEIGHT];
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            board_new[y][x] = board[y][x];
        }
    }
    for y in (0..HEIGHT).rev() {
        if board_new[y][column as usize] == 0 {
            board_new[y][column as usize] = color;
            break;
        }
    }
    board_new
}

// Le Brain (TM) - in "we-spared-no-expenses" functional style (Hint - raptors lurking)
//
// Executive summary of porting this to Rust: The result works, but IMHO it is less clear than
// my OCaml version - the one I wrote 4 years ago, when I first dove into functional style...
//
//          https://goo.gl/Cz3kr3
//
// In Rust, at some point I forgot what I was doing (i.e. the algorithm) and got lost
// in the minutiae - with the Rust compiler barking about requiring stars, ampersands and
// double ampersands (not joking, see below)... I just followed what the compiler was
// complaining about "I want an & here" - "OK, here's one". And ended up with double ampersands
// (sigh)
//
// Another highlight - try removing the '.collect's - i.e. changing...
//
//     let moves_and_boards: LinkedList<_> = valid_moves.iter().map(
//         |column| (*column, drop_disk(board, *column, color))).collect();
//
// to the way it looks in OCaml, i.e.
//
//     let moves_and_boards = valid_moves.map(
//         |column| (*column, drop_disk(board, *column, color)));
//
// Do this in all the places below ...and then marvel at the errors spat out by the Rust compiler.
// Who's afraid of C++ template errors?  You ain't seen nothing yet!
//
// I believe the issue comes from .map and .filter and friends returning iter-ables, and not the
// collections they came from - so in OCaml it's fine to "pipe" the results of List.filter
// to List.map... where as here you get Error Messages from Lord Voldemort (TM) unless you
// .collect() , specify a LinkedList<_> type for the result, and then .iter() all over again
// (because LinkedList has no .map or .filter (what?!)
//
// Then again, in all fairness, I am a complete newbie in Rust - literally one day old. I am hoping
// the experts in /r/rust will show me the error of my ways and help me make this much closer to
// the ML version (again, here:   https://goo.gl/Cz3kr3 )
//
// P.S. The good news: once I managed to compile it, it run correctly the first time (a trait it
// shares with my corresponding efforts in OCaml, 4 years ago). And it's 2x faster than OCaml!

fn ab_minimax(maximize_or_minimize:bool, color:i32, depth:i32, board:&Board, debug:bool) -> (Option<u32>, i32) {
    let valid_moves: Vec<_> = (0..(WIDTH as u32)).filter(|&column| board[0][column as usize] == 0).collect();
    if valid_moves.is_empty() {
        return (None, score_board(board));
    }

    let moves_and_boards: Vec<_> = valid_moves
        .iter()
        .map(|column| (*column, drop_disk(board, *column, color)))
        .collect();
    let moves_and_scores: Vec<_> = moves_and_boards
        .iter()
        .map(|&(column,board)| (column, score_board(&board)))
        .collect();
    let target_score = if maximize_or_minimize { ORANGE_WINS } else { YELLOW_WINS };
    if let Some(killer_move) = moves_and_scores.iter().find(|& &(_,score)| score==target_score) {
        return (Some(killer_move.0), killer_move.1);
    }
    let best_scores: Vec<_> = match depth {
        1 => moves_and_scores.iter().map(|x| x.1).collect(),
        _ => moves_and_boards.iter().map(|x| &x.1)
            .map(|le_board| ab_minimax(!maximize_or_minimize, other_color(color), depth-1, le_board, debug))
            .map(|(_,bscore)|
                // when loss is certain, avoid forfeiting the match, by shifting scores by depth...
                match bscore {
                    ORANGE_WINS | YELLOW_WINS => bscore - depth*color,
                    _ => bscore
                })
            .collect()
    };
    let all_data: Vec<_> = best_scores.iter().zip(valid_moves).collect();
    if debug && depth == MAX_DEPTH {
        for &(score, column) in &all_data {
            println!("Depth {}, placing on {}, Score:{}", depth, column, score);
        }
    }
    let best = if maximize_or_minimize {
        all_data.iter().max()
    } else {
        all_data.iter().min()
    };
    match best {
        None => (None, 0),
        Some(&(&best_score, best_move)) => (Some(best_move),best_score)
    }
}

fn main() {
    let board = load_board(env::args().collect());
    let score_orig = score_board(&board);
    let debug = env::args().any(|x| x == "-debug");
    if debug {
        println!("Starting score: {}", score_orig);
    }
    if score_orig == ORANGE_WINS {
        println!("I win");
        process::exit(-1);
    } else if score_orig == YELLOW_WINS {
        println!("You win");
        process::exit(-1);
    } else {
        // let (mv,score) = ab_minimax(true, 1, MAX_DEPTH, &board);
        let (mv,_) = ab_minimax(true, 1, MAX_DEPTH, &board, debug);
        match mv {
            Some(column) => {
                println!("{}", column);
                process::exit(0);
            },
            _ => {
                println!("No move possible");
                process::exit(0);
            }
        }
    }
}

/* vim: set expandtab ts=8 sts=4 shiftwidth=4 */
