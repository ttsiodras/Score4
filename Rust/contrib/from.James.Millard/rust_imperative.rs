use std::cmp;
use std::env;

const WIDTH: i32 = 7;
const HEIGHT: i32 = 6;
const ORANGE_WINS: i32 = 1000000;
const YELLOW_WINS: i32 = -ORANGE_WINS;

fn main() {
	let mut board = prepare_board(env::args().collect());

	if board.score == ORANGE_WINS {
		println!("I win");
	} else if board.score == YELLOW_WINS {
		println!("You win");
	} else {
		let old_score = board.score;
		let best_move = ab_minimax(1, board.max_depth, &mut board);
		//println!("{}", board.score);
		board.score = old_score;
		if best_move != -1 {
			println!("{}", best_move);
			drop_disk(best_move, 1, &mut board);
			if board.score == ORANGE_WINS {
				println!("I win");
			} else if board.score == YELLOW_WINS {
				println!("You win");
			}
		} else {
			println!("No move possible!");
		}
	}
}

struct Board {
	next_slot: [i32;WIDTH as usize],
	slots: [i32;(WIDTH*HEIGHT) as usize], // column major
	score: i32,
	debug: bool,
	max_depth: i32
}

impl Board {
	fn new()->Board{
		Board{
			next_slot: [0; WIDTH as usize],
			slots: [0; (HEIGHT*WIDTH) as usize],
			score: 0,
			debug: false,
			max_depth: 7
		}
	}
}

fn prepare_board(args: Vec<String>) -> Board{
	let mut board = Board::new();

	for (i, arg) in args.iter().enumerate(){
		if arg.len() == 3 {
			let int = arg[2..3].parse::<i32>().unwrap();
			if arg.starts_with("y"){
				drop_disk(int, -1, &mut board);
			} else if arg.starts_with("o"){
				drop_disk(int, 1, &mut board);
			}
		} else if arg == "-debug" {
			board.debug = true;
		} else if arg == "-level" {
			board.max_depth = args[i+1].parse::<i32>().unwrap();
		}
	}
	return board;
}

fn drop_disk(column: i32, color: i32, board: &mut Board) -> i32{

	let row_filled = board.next_slot[column as usize];
	if row_filled >= HEIGHT{
		return -1;
	}

	board.score += score_change(board, column, row_filled, color);
	board.slots[(row_filled + column*HEIGHT) as usize] = color;
	board.next_slot[column as usize] += 1;

	return row_filled;
}

fn score_change(board: &mut Board, x: i32, y: i32, color: i32) -> i32{
	let mut counters = [0; 9];

	let xs = cmp::max(x-3,  0);
	let xe = cmp::min(x+1, WIDTH-3); //exclusive
	let ys =  cmp::max(y-3, 0);
	let ye = cmp::min(y+1, HEIGHT-3); //exclusive

	let drs = cmp::max(xs - x, ys - y);
	let dre = cmp::min(xe - x, ye - y); //exclusive
	let dls = cmp::max(xs - x, y - ye - 2);
	let dle = cmp::min(xe-x, y-ys - 2); //exclusive

	partial_score_change(board, &mut counters, ye-ys, 1, ys + x*HEIGHT); // vertical
	partial_score_change(board, &mut counters, xe-xs, HEIGHT, y + xs*HEIGHT); // horizontal
	partial_score_change(board, &mut counters, dre-drs, HEIGHT+1, y+drs + (x+drs)*HEIGHT); // DR diagonal
	partial_score_change(board, &mut counters, dle-dls, HEIGHT-1, y-dls + (x+dls)*HEIGHT); // DL diagonal

	if counters[1] != 0 && color < 0 {
		return YELLOW_WINS - board.score;
	} else if counters[7] != 0 && color > 0 {
		return ORANGE_WINS - board.score;
	} else {
		let old: i32 = counters[5] + 2 * counters[6] + 5 * counters[7] - counters[3] - 2 * counters[2] - 5 * counters[1];
		let new_score: i32 = if color < 0 {
			counters[6] + 2 * counters[7] - counters[4] - 2 * counters[3] - 5 * counters[2]
		} else {
			counters[4] + 2 * counters[5] + 5 * counters[6] - counters[2] - 2 * counters[1]
		};

		return new_score - old;
	}

}

fn partial_score_change(board: &mut Board, counters: &mut [i32; 9], n: i32, stride: i32, index: i32) {
	if n <= 0 {return}
	let index: usize = index as usize;
	let stride = stride as usize;
	
	let mut span_score: i32 = 4 + board.slots[index] + board.slots[index + stride*1] + board.slots[index + stride*2] + board.slots[index + stride*3];
	counters[span_score as usize] += 1;
	
	for i in 0..(n - 1) as usize{
		span_score += -board.slots[index + i*stride] + board.slots[index + (4+i)*stride];
		counters[span_score as usize] += 1;
	}		
}

fn ab_minimax(color: i32, depth: i32, board: &mut Board) -> i32{
	if 0 == depth {return -1;}

	let maximize_score = color > 0;
	let target_score = if maximize_score {ORANGE_WINS} else {YELLOW_WINS};
	let mut best_score: i32 = -target_score;
	let mut best_move: i32 = -1;
	let old_score: i32 = board.score;

	for column in 0..WIDTH{

		let row_filled: i32 = drop_disk(column, color, board);
		if row_filled == -1 {continue;}

		if depth > 1 && board.score != target_score {
			ab_minimax(-color, depth - 1, board);
		}

		if (depth == board.max_depth) & board.debug {
			println!("Depth {0}, placing on {1}, score:{2}", depth, column, board.score);
		}

		if board.score == target_score { // Original algorithm keeps testing columns after a game ending move is found, re-test to avoid wasting the effort
			best_move = column;
			best_score = board.score;
			board.slots[(row_filled + column*HEIGHT) as usize] = 0;
			board.next_slot[column as usize] -= 1;
			break;
		}

		if ( maximize_score && (board.score >= best_score))
		|| (!maximize_score && (board.score <= best_score)){
			best_score = board.score;
			best_move = column;
		}

		board.slots[(row_filled + column*HEIGHT) as usize] = 0; //Reset board
		board.score = old_score;
		board.next_slot[column as usize] -= 1;
	}

	board.score = best_score;
	return best_move;
}
