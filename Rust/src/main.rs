use std::env;
use std::process;
use std::collections::LinkedList;

mod common;

// Import constants

use common::WIDTH as WIDTH;
use common::HEIGHT as HEIGHT;
use common::ORANGE_WINS as ORANGE_WINS;
use common::YELLOW_WINS as YELLOW_WINS;
use common::MAX_DEPTH as MAX_DEPTH;

// Import types

use common::Board as Board;

// Import functions

use common::score_board as score_board;
use common::other_color as other_color;

// Parse cmdline specs to create initial board

fn load_board(args: Vec<String>) -> Board {
    let mut board = [[0; WIDTH as usize]; HEIGHT as usize];
    for yy in 0..HEIGHT {
        let y = yy as usize;
        for xx in 0..WIDTH {
            let x = xx as usize;
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
    let mut board_new: Board = [[0; WIDTH as usize]; HEIGHT as usize];
    for yy in 0..HEIGHT {
        let y = yy as usize;
        for xx in 0..WIDTH {
            let x = xx as usize;
            board_new[y][x] = board[y][x];
        }
    }
    for yy in (0..HEIGHT).rev() {
        let y = yy as usize;
        if board_new[y][column as usize] == 0 {
            board_new[y][column as usize] = color;
            break;
        }
    }
    board_new
}

// Le Brain (TM) - in "we-spared-no-expenses" functional style (Hint - raptors lurking)
//
// Executive summary of porting this to Rust: The result works, but it's a lot less clear than
// my OCaml version - the one I wrote 4 years ago, when I first dove into functional style...
//
//          https://goo.gl/Cz3kr3
//
// To be honest, at some point I forgot what I was doing (i.e. the algorithm) and got lost
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
// Converting to usize was also very annoying - search for usize in this file to see how many times
// I had to do this.
//
// Then again, in all fairness, I am a complete newbie in Rust - literally one day old. I am hoping
// the experts in /r/rust will show me the error of my ways and help me make this much closer to
// the ML version (again, here:   https://goo.gl/Cz3kr3 )
//
// P.S. The good news: once I managed to compile it, it run correctly the first time (a trait it
// shares with my corresponding efforts in OCaml, 4 years ago).

fn ab_minimax(maximize_or_minimize:bool, color:i32, depth:i32, board:&Board, debug:&bool) -> (Option<u32>, i32) {
    let valid_moves: LinkedList<_> = (0..WIDTH).filter(|column| board[0][*column as usize] == 0).collect();
    match valid_moves.is_empty() {
        true => (None, score_board(board)),
        _ => {
            let moves_and_boards: LinkedList<_> = valid_moves.iter().map(|column| (*column, drop_disk(board, *column, color))).collect();
            let movies_and_scores: LinkedList<_> = moves_and_boards.iter().map(|&(column,board)| (column, score_board(&board))).collect();
            let target_score = if maximize_or_minimize { ORANGE_WINS } else { YELLOW_WINS };
            let killer_moves: LinkedList<_> = movies_and_scores.iter().filter(|& &(_,score)| score==target_score).collect();
            match killer_moves.front() {
                Some(& &(killer_move,killer_score)) => (Some(killer_move), killer_score),
                None => {
                    let best_scores = match depth {
                        1 => {
                            let scores: LinkedList<_> = movies_and_scores.iter().map(|x| x.1).collect();
                            scores
                        },
                        _ => {
                            let scores: LinkedList<_> = moves_and_boards.iter().map(|x| &x.1)
                                .map(|&le_board| ab_minimax(!maximize_or_minimize, other_color(color), depth-1, &le_board, debug))
                                .map(|(_,bscore)|
                                    // when loss is certain, avoid forfeiting the match, by shifting scores by depth...
                                    match bscore {
                                        1000000 | -1000000 => bscore - depth*color,
                                        _ => bscore
                                    })
                                .collect();
                            scores
                        }
                    };
                    let mut all_data: LinkedList<_> = valid_moves.iter().zip(best_scores).collect();
                    if *debug && depth == MAX_DEPTH {
                        for (column, score) in all_data.clone() {
                            println!("Depth {}, placing on {}, Score:{}\n", depth, column, score);
                        }
                    }
                    let init = all_data.pop_front();
                    match init {
                        None => (None, 0),
                        Some(v) => {
                            let (best_move,best_score) = all_data.iter().fold(
                                v,
                                |l, r| {
                                    if maximize_or_minimize {
                                        if l.1 > r.1 { l } else { *r } 
                                    } else {
                                        if l.1 < r.1 { l } else { *r } 
                                    }
                                }
                            );
                            (Some(*best_move),best_score)
                        }
                    }
                }
            }
        }
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
        let (mv,_) = ab_minimax(true, 1, MAX_DEPTH, &board, &debug);
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
