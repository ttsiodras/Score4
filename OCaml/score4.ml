let width = 7
let height = 6
let maxDepth = 7
let orangeWins = 1000000
let yellowWins = -orangeWins
let debug = ref true

type mycell =
    | Orange
    | Yellow
    | Barren

let ( |> ) x fn = fn x

let inside y x =
    y>=0 && y<height && x>=0 && x<width

let otherColor color =
    match color with
        | Orange -> Yellow
        | Yellow -> Orange
        | _      -> Barren

(* diagonal, down-right *)
let negativeSlope = [| (0,0); (1,1);   (2,2);   (3,3)  |]

(* diagonal, up-right   *)
let positiveSlope = [| (0,0); (-1,1);  (-2,2);  (-3,3) |]

let scoreBoard board =
    let rateCell = function
        | Orange -> 1
        | Yellow -> -1
        | Barren -> 0 in
    let counts = [| 0;0;0;0;0;0;0;0;0 |] in
    let scores = Array.make_matrix height width 0 in
    for y=0 to height-1 do
        for x=0 to width-1 do
            scores.(y).(x) <- rateCell board.(y).(x)
        done
    done ;

    let myincr arr idx = arr.(idx) <- arr.(idx) + 1 in

    (* Horizontal spans *)
    for y=0 to height-1 do
	let score = ref (scores.(y).(0) + scores.(y).(1) + scores.(y).(2)) in
	for x=3 to width-1 do
	    score := !score + scores.(y).(x);
	    myincr counts (!score+4) ;
	    score := !score - scores.(y).(x-3)
	done
    done ;

    (* Vertical spans *)
    for x=0 to width-1 do
	let score = ref (scores.(0).(x) + scores.(1).(x) + scores.(2).(x)) in
	for y=3 to height-1 do
	    score := !score + scores.(y).(x);
	    myincr counts (!score+4);
	    score := !score - scores.(y-3).(x);
	done
    done ;

    (* Down-right (and up-left) diagonals *)
    for y=0 to height-4 do
	for x=0 to width-4 do
	    let score = ref 0 in
	    for idx=0 to 3 do
		match negativeSlope.(idx) with
		| (yofs,xofs) ->
		    let yy = y+yofs in
		    let xx = x+xofs in
		    score := !score + scores.(yy).(xx)
	    done ;
	    myincr counts (!score+4)
	done
    done ;

    (* up-right (and down-left) diagonals *)
    for y=3 to height-1 do
	for x=0 to width-4 do
	    let score = ref 0 in
	    for idx=0 to 3 do
		match positiveSlope.(idx) with
		| (yofs,xofs) ->
		    let yy = y+yofs in
		    let xx = x+xofs in
		    score := !score + scores.(yy).(xx)
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

exception NoMoreWork of int

let dropDisk board column color =
    try begin
        for y=height-1 downto 0 do
            if board.(y).(column) = Barren then begin
                board.(y).(column) <- color ;
                raise (NoMoreWork y)
            end
        done ;
        -1
    end with NoMoreWork y -> y

exception FoundKillerMove of int*int  (* column to drop, score achieved *)

let rec abMinimax maximizeOrMinimize color depth board =
    match depth with
    | 0 -> (None,scoreBoard board)
    | _ ->
        try begin
            let startingScore = match maximizeOrMinimize with true -> -10000000 | false -> 10000000 in
            let bestScore = ref startingScore in
            let bestMove = ref (-1) in 
            let killerTarget = match maximizeOrMinimize with true -> orangeWins | false -> yellowWins in
            for column=0 to width-1 do 
                if board.(0).(column) = Barren then
                begin
                    let rowFilled = dropDisk board column color in
                    let s = scoreBoard board in
                    if s = killerTarget then begin
                        board.(rowFilled).(column) <- Barren ;
                        raise (FoundKillerMove (column,s))
                    end else match abMinimax (not maximizeOrMinimize) (otherColor color) (depth-1) board with
                        | (moveInner,scoreInner) ->
                            board.(rowFilled).(column) <- Barren ;
                            if depth = maxDepth && !debug then
                                Printf.printf "Depth %d, placing on %d, Score:%d\n%!" depth column scoreInner ;
                            if maximizeOrMinimize then 
                            begin
                                if scoreInner>= !bestScore then begin bestScore := scoreInner ; bestMove := column end
                            end
                            else
                            begin
                                if scoreInner<= !bestScore then begin bestScore := scoreInner ; bestMove := column end 
                            end
                    end
            done ;
            (Some(!bestMove),!bestScore)
        end
        with FoundKillerMove (move,score) -> (Some(move),score)

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
    let board = Array.make_matrix height width Barren in
    for y=0 to height-1 do
        for x=0 to width-1 do
            let orange = Printf.sprintf "o%d%d" y x in
            let yellow = Printf.sprintf "y%d%d" y x in
            if inArgs args orange then
                board.(y).(x) <- Orange
            else if inArgs args yellow then
                board.(y).(x) <- Yellow
            else
                board.(y).(x) <- Barren
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
        let mv,score = abMinimax true Orange maxDepth board in
        match mv with
        | Some column -> Printf.printf "%d\n" column ; 0
        | _ -> failwith "No move possible"

(* vim: set expandtab ts=8 sts=4 shiftwidth=4 *)
