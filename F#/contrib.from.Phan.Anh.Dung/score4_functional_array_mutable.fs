module Score4.FunctionalArrayMutable

open System
open System.Collections.Generic

[<Literal>]
let WIDTH = 7
[<Literal>]
let HEIGHT = 6
[<Literal>]
let MAXDEPTH = 7
[<Literal>]
let ORANGEWINS = 1000000
[<Literal>]
let YELLOWWINS = -1000000
let mutable debug = true

type Cell =
    | Orange = 1
    | Yellow = -1
    | Barren = 0

let scoreBoard (board:Cell array) =    

    let counts = Array.create 9 0

    let scores = board

    let inline myincr (arr:int array) idx =
        arr.[idx] <- arr.[idx] + 1

    (* Horizontal spans *)
    for y=0 to HEIGHT-1 do
        let mutable score = int scores.[WIDTH*(y)+0] + int scores.[WIDTH*(y)+1] + int scores.[WIDTH*(y)+2]
        for x=3 to WIDTH-1 do
            score <- score + int scores.[WIDTH*(y)+x];
            myincr counts (score+4) ;
            score <- score - int scores.[WIDTH*(y)+x-3]

    (* Vertical spans *)
    for x=0 to WIDTH-1 do
        let mutable score = int scores.[WIDTH*(0)+x] + int scores.[WIDTH*(1)+x] + int scores.[WIDTH*(2)+x]
        for y=3 to HEIGHT-1 do
            score <- score + int scores.[WIDTH*(y)+x];
            myincr counts (score+4);
            score <- score - int scores.[WIDTH*(y-3)+x];

    (* Down-right (and up-left) diagonals *)
    for y=0 to HEIGHT-4 do
        for x=0 to WIDTH-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                let yy = y+idx in
                let xx = x+idx in
                score <- score + int scores.[WIDTH*(yy)+xx]
            myincr counts (score+4)

    (* up-right (and down-left) diagonals *)
    for y=3 to HEIGHT-1 do
        for x=0 to WIDTH-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                let yy = y-idx in
                let xx = x+idx in
                score <- score + int scores.[WIDTH*(yy)+xx]
            myincr counts (score+4)

    if counts.[0] <> 0 then
        YELLOWWINS
    else if counts.[8] <> 0 then
        ORANGEWINS
    else
        counts.[5] + 2*counts.[6] + 5*counts.[7] + 10*counts.[8] -
            counts.[3] - 2*counts.[2] - 5*counts.[1] - 10*counts.[0]

let inline dropDisk (board:Cell array) column color =
    let rec loop count =
        if count = -1 then None
        elif board.[WIDTH*count+column] = Cell.Barren then
            board.[WIDTH*count+column] <- color
            Some count
        else loop (count-1)
    loop (HEIGHT-1)

let pickupDisk (board:Cell array) column rowOption =
    match rowOption with
    | Some row -> board.[WIDTH*row+column] <- Cell.Barren
    | None -> ()

let rec abMinimax maximizeOrMinimize color depth (board:Cell array) =
    let validMoves = 
        Array.init WIDTH id |> Array.filter (fun move -> board.[WIDTH*(0)+move] = Cell.Barren)
    match validMoves with
    | [||] -> (None,scoreBoard board)
    | _  ->
        let scores = validMoves |> Array.map (fun column -> let row = dropDisk board column color // commit
                                                            let score = scoreBoard board
                                                            pickupDisk board column row // rollback
                                                            score ) in
        let killerMoveIndex =
            let targetScore = if maximizeOrMinimize then ORANGEWINS else YELLOWWINS in
            scores |> Array.tryFindIndex ((=) targetScore) in
        match killerMoveIndex with
        | Some i -> (Some(validMoves.[i]), scores.[i])
        | None ->
            let bestScores =
                match depth with
                | 1 -> scores
                | _ ->
                    validMoves |> Array.map (fun column ->
                                                    let row = dropDisk board column color // commit
                                                    let _, bscore = abMinimax (not maximizeOrMinimize) (enum (- int color)) (depth-1) board
                                                    pickupDisk board column row // rollback
                                                    match bscore with
                                                    | ORANGEWINS | YELLOWWINS -> bscore - depth*(int color)
                                                    | _ -> bscore)
            if debug && depth = MAXDEPTH then
                Array.iter2 (fun move score ->
                    Printf.printf "Depth %d, placing on %d, Score:%d\n" depth move score) validMoves bestScores ;
            let bestMove,bestScore =
                if maximizeOrMinimize 
                then Array.fold2 (fun (m0, s0) m s -> if s0 > s then m0, s0 else m, s) (0, Int32.MinValue) validMoves bestScores
                else Array.fold2 (fun (m0, s0) m s -> if s0 < s then m0, s0 else m, s) (0, Int32.MaxValue) validMoves bestScores
            (Some(bestMove),bestScore)

let inline inArgs str args =
    Array.exists ((=) str) args

let loadBoard args =
    let board = Array.zeroCreate (HEIGHT*WIDTH)
    for y=0 to HEIGHT-1 do
        for x=0 to WIDTH-1 do
            let orange = Printf.sprintf "o%d%d" y x
            let yellow = Printf.sprintf "y%d%d" y x
            if inArgs orange args then
                board.[WIDTH*(y)+x] <- Cell.Orange
            else if inArgs yellow args then
                board.[WIDTH*(y)+x] <- Cell.Yellow
            else
                board.[WIDTH*(y)+x] <- Cell.Barren
        done
    done ;
    board

#if INTERACTIVE
#else
[<EntryPoint>]
#endif
let main (args:string[]) =
    let board = loadBoard args
    let scoreOrig = scoreBoard board
    debug <- inArgs "-debug" args
    if scoreOrig = ORANGEWINS then
        printf "I win"
        -1
    elif scoreOrig = YELLOWWINS then
        printf "You win"
        -1
    else
        let mv,score = abMinimax true Cell.Orange MAXDEPTH board
        let msgWithColumnToPlaceOrange = 
            match mv with
            | Some column -> printfn "%A" column
            | _ -> printfn "No move possible"
        msgWithColumnToPlaceOrange
        0    