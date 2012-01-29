module Score4.Imperative
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

let rec any l =
    match l with
    | []        -> false
    | true::xs  -> true
    | false::xs -> any xs

let counts = Array.create 9 0

let scoreBoard (board:Cell array) =
    Array.fill counts 0 9 0

    (* No need to create a "stub" - by using enums we can just operate on the board!
    let scores = Array.zeroCreate HEIGHT
    for y=0 to HEIGHT-1 do
        scores.[y] <- Array.zeroCreate WIDTH
        for x=0 to WIDTH-1 do
            scores.[WIDTH*(y)+x] <- int board.[WIDTH*(y)+x] *)

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

let dropDisk (board:Cell array) column color =
    let mutable searching = true
    let mutable y = HEIGHT-1
    while searching && y>=0 do
        if board.[WIDTH*(y)+column] = Cell.Barren then
            board.[WIDTH*(y)+column] <- color
            searching <- false
        else
            y <- y - 1
    y

let rec abMinimax maximizeOrMinimize color depth board =
    match depth with
    | 0 -> (None,scoreBoard board)
    | _ ->
        let startingScore = match maximizeOrMinimize with true -> -10000000 | false -> 10000000
        let mutable bestScore = startingScore
        let mutable bestMove = -1
        let killerTarget = match maximizeOrMinimize with true -> ORANGEWINS | false -> YELLOWWINS
        let mutable column = -1
        let mutable foundKiller = false
        while not foundKiller && (column<WIDTH-1) do
            column <- column+1
            if board.[WIDTH*(0)+column] = Cell.Barren then
                let rowFilled = dropDisk board column color
                let s = scoreBoard board
                if s = killerTarget then
                    board.[WIDTH*(rowFilled)+column] <- Cell.Barren
                    bestScore <- s
                    bestMove <- column
                    foundKiller <- true
                else
                    let pair =
                        if depth = 1 then
                            (Some(-1),s)
                        else
                            abMinimax (not maximizeOrMinimize) (enum (- int color)) (depth-1) board
                    match pair with
                    | (moveInner,scoreInner) ->
                        board.[WIDTH*(rowFilled)+column] <- Cell.Barren
                        (* when loss is certain, avoid forfeiting the match, by shifting scores by depth... *)
                        let shiftedScore =
                            match scoreInner with
                            | ORANGEWINS | YELLOWWINS -> scoreInner - depth*(int color)
                            | _ -> scoreInner
                        if depth = MAXDEPTH && debug then
                            Printf.printf "Depth %d, placing on %d, Score:%d\n" depth column scoreInner ;
                        if maximizeOrMinimize then
                            if shiftedScore >= bestScore then
                                bestScore <- shiftedScore
                                bestMove <- column
                        else
                            if shiftedScore <= bestScore then
                                bestScore <- shiftedScore
                                bestMove <- column
        done ;
        (Some(bestMove),bestScore)

let inArgs str args =
    any(List.ofSeq(Array.map (fun x -> (x = str)) args))

let loadBoard args =
    let board = Array.zeroCreate (WIDTH*HEIGHT)
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

// vim: set expandtab ts=8 sts=4 shiftwidth=4
