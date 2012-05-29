open Common;;

exception NoMoreWork of int

let dropDisk board column color =
    try (
        for y=height-1 downto 0 do
            if board.(y).(column) = 0 then (
                board.(y).(column) <- color ;
                raise (NoMoreWork y)
            )
        done ;
        -1
    ) with NoMoreWork y -> y

exception FoundKillerMove of int*int  (* column to drop, score achieved *)

let rec abMinimax maximizeOrMinimize color depth board =
    try (
        let startingScore = match maximizeOrMinimize with true -> -10000000 | false -> 10000000 in
        let bestScore = ref startingScore in
        let bestMove = ref (-1) in
        let killerTarget = match maximizeOrMinimize with true -> orangeWins | false -> yellowWins in
        for column=0 to width-1 do
            if board.(0).(column) = 0 then
            (
                let rowFilled = dropDisk board column color in
                let s = scoreBoard board in
                if s = killerTarget then (
                    board.(rowFilled).(column) <- 0 ;
                    raise (FoundKillerMove (column,s))
                ) else (
                    let pair =
                        if depth == 1 then
                            (Some(-1),s)
                        else
                            abMinimax (not maximizeOrMinimize) (otherColor color) (depth-1) board in
                    match pair with
                    | (moveInner,scoreInner) ->
                        board.(rowFilled).(column) <- 0 ;
                        (* when loss is certain, avoid forfeiting the match, by shifting scores by depth... *)
                        let shiftedScore =
                            match scoreInner with
                            | 1000000 | -1000000 -> scoreInner - depth*color
                            | _ -> scoreInner in
                        if depth = maxDepth && !debug then
                            Printf.printf "Depth %d, placing on %d, Score:%d\n%!" depth column shiftedScore ;
                        if maximizeOrMinimize then
                        (
                            if shiftedScore>= !bestScore then (
                                bestScore := shiftedScore ; bestMove := column
                            )
                        )
                        else
                        (
                            if shiftedScore<= !bestScore then (
                                bestScore := shiftedScore ; bestMove := column
                            )
                        )
                )
            )
        done ;
        (Some(!bestMove),!bestScore)
    )
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
    if scoreOrig = orangeWins then (
        Printf.printf "I win\n" ;
        (-1)
    ) else if scoreOrig = yellowWins then (
        Printf.printf "You win\n" ;
        (-1)
    ) else
        let mv,score = abMinimax true 1 maxDepth board in
        match mv with
        | Some column -> Printf.printf "%d\n" column ; 0
        | _ -> failwith "No move possible"

(* vim: set expandtab ts=8 sts=4 shiftwidth=4 *)
