let width = 7
let height = 6
let maxDepth = 7
let orangeWins = 1000000
let yellowWins = -orangeWins
let debug = ref true

(* July 28, 2012:
 *
 * I deviated from the blog post now (if you want to, checkout
 * earlier versions from GitHub to see the blog post version).
 *
 * Ocaml unfortunately doesn't support integer-based enums (like F#/C++).
 * I therefore removed this...
 *
 * type mycell =
 *     | Orange
 *     | Yellow
 *     | Barren
 *
 * ...and replaced it with direct usage of 1,0,-1
 * (wherever Orange,Barren,Yellow were used)
 *
 * This provided a speedup of 60% - just as integer-enums did for F# -
 * since there's no "mapping" required in scoreBoard anymore.
 *)

(* This emulates the [X .. Y] construct of F# *)
let (--) i j =
    let rec aux n acc =
        if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let ( |> ) x fn = fn x

(* This emulates the List.zip of F# *)
let myzip a b =
    let rec innermyzip a b accum =
        match a with
        | [] -> accum
        | _  ->
            let newList = (List.hd a,List.hd b)::accum in
            innermyzip (List.tl a) (List.tl b) newList in
    List.rev (innermyzip a b [])

let inside y x =
    y>=0 && y<height && x>=0 && x<width

let otherColor color = -color

(* diagonal, down-right *)
let negativeSlope = [| (0,0); (1,1);   (2,2);   (3,3)  |]

(* diagonal, up-right   *)
let positiveSlope = [| (0,0); (-1,1);  (-2,2);  (-3,3) |]

let scoreBoard board =
    let counts = [| 0;0;0;0;0;0;0;0;0 |] in

    let myincr arr idx = arr.(idx) <- arr.(idx) + 1 in

    (* Horizontal spans *)
    for y=0 to height-1 do
	let score = ref (board.(y).(0) + board.(y).(1) + board.(y).(2)) in
	for x=3 to width-1 do
	    score := !score + board.(y).(x);
	    myincr counts (!score+4) ;
	    score := !score - board.(y).(x-3)
	done
    done ;

    (* Vertical spans *)
    for x=0 to width-1 do
	let score = ref (board.(0).(x) + board.(1).(x) + board.(2).(x)) in
	for y=3 to height-1 do
	    score := !score + board.(y).(x);
	    myincr counts (!score+4);
	    score := !score - board.(y-3).(x);
	done
    done ;

    (* Down-right (and up-left) diagonals *)
    for y=0 to height-4 do
	for x=0 to width-4 do
	    let score = ref 0 in
	    for idx=0 to 3 do
                score := !score + board.(y+idx).(x+idx)
	    done ;
	    myincr counts (!score+4)
	done
    done ;

    (* up-right (and down-left) diagonals *)
    for y=3 to height-1 do
	for x=0 to width-4 do
	    let score = ref 0 in
	    for idx=0 to 3 do
                score := !score + board.(y-idx).(x+idx)
	    done ;
	    myincr counts (!score+4)
	done
    done ;

    if counts.(0) <> 0 then
        yellowWins
    else if counts.(8) <> 0 then
        orangeWins
    else
        counts.(5) + 2*counts.(6) + 5*counts.(7) + 10*counts.(8) -
            counts.(3) - 2*counts.(2) - 5*counts.(1) - 10*counts.(0)

exception NoMoreWork

let dropDisk board column color =
    let arrNew = Array.make_matrix height width 0 in
    for y=0 to height-1 do
        for x=0 to width-1 do
            arrNew.(y).(x) <- board.(y).(x)
        done
    done ;
    try begin
        for y=height-1 downto 0 do
            if arrNew.(y).(column) = 0 then begin
                arrNew.(y).(column) <- color ;
                raise NoMoreWork
            end
        done ;
        arrNew
    end with NoMoreWork -> arrNew

let rec abMinimax maximizeOrMinimize color depth board =
    let validMoves =
        0--(width-1) |> List.filter (fun column -> board.(0).(column) = 0) in
    match validMoves with
    | [] -> (None,scoreBoard board)
    | _  ->
        let movesAndBoards = validMoves |> List.map (fun column -> (column,dropDisk board column color)) in
        let movesAndScores = movesAndBoards |>  List.map (fun (column,board) -> (column,scoreBoard board)) in
        let killerMoves =
            let targetScore = if maximizeOrMinimize then orangeWins else yellowWins in
            movesAndScores |> List.filter (fun (_,score) -> score=targetScore) in
        match killerMoves with
        | (killerMove,killerScore)::rest -> (Some(killerMove), killerScore)
        | [] ->
            let bestScores =
                match depth with
                | 1 -> movesAndScores |> List.map snd
                | _ ->
                    movesAndBoards |> List.map snd |>
                    List.map (abMinimax (not maximizeOrMinimize) (otherColor color) (depth-1)) |>
                    (* when loss is certain, avoid forfeiting the match, by shifting scores by depth... *)
                    List.map (fun (bmove,bscore) ->
                        let shiftedScore =
                            match bscore with
                            | 1000000 | -1000000 -> bscore - depth*color
                            | _ -> bscore in
                        shiftedScore) in
            let allData = myzip validMoves bestScores in
            if !debug && depth = maxDepth then
                List.iter (fun (column,score) ->
                    Printf.printf "Depth %d, placing on %d, Score:%d\n%!" depth column score) allData ;
            let best  (_,s as l) (_,s' as r) = if s > s' then l else r
            and worst (_,s as l) (_,s' as r) = if s < s' then l else r in
            let bestMove,bestScore =
                List.fold_left (if maximizeOrMinimize then best else worst) (List.hd allData) (List.tl allData) in
            (Some(bestMove),bestScore)

(* let any = List.fold_left (||) false
 * ..is slower than ... *)
let rec any l =
    match l with
    | []        -> false
    | true::xs  -> true
    | false::xs -> any xs

let inArgs args str =
    any(Array.to_list (Array.map (fun x -> (x = str)) args))

let loadBoard args =
    let board = Array.make_matrix height width 0 in
    for y=0 to height-1 do
        for x=0 to width-1 do
            let orange = Printf.sprintf "o%d%d" y x in
            let yellow = Printf.sprintf "y%d%d" y x in
            if inArgs args orange then
                board.(y).(x) <- 1
            else if inArgs args yellow then
                board.(y).(x) <- -1
            else
                board.(y).(x) <- 0
        done
    done ;
    board

let _ =
    let board = loadBoard Sys.argv in
    let scoreOrig = scoreBoard board in
    debug := inArgs Sys.argv "-debug" ;
    if !debug then
        Printf.printf "Starting score: %d\n" scoreOrig ;
    if scoreOrig = orangeWins then begin
        Printf.printf "I win\n" ;
        (-1)
    end else if scoreOrig = yellowWins then begin
        Printf.printf "You win\n" ;
        (-1)
    end else
        let mv,score = abMinimax true 1 maxDepth board in
        match mv with
        | Some column -> Printf.printf "%d\n" column ; 0
        | _ -> failwith "No move possible"

(* vim: set expandtab ts=8 sts=4 shiftwidth=4 *)
