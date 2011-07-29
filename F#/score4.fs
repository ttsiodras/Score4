open System.Collections.Generic

[<Literal>]
let width = 7
[<Literal>]
let height = 6
[<Literal>]
let maxDepth = 7
[<Literal>]
let orangeWins = 1000000
[<Literal>]
let yellowWins = -1000000
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

let scoreBoard (board:Cell array array) =
    Array.fill counts 0 9 0

    (* No need to create a "stub" - by using enums we can just operate on the board!
    let scores = Array.zeroCreate height
    for y=0 to height-1 do
        scores.[y] <- Array.zeroCreate width
        for x=0 to width-1 do
            scores.[y].[x] <- int board.[y].[x] *)

    let scores = board

    let inline myincr (arr:int array) idx =
        arr.[idx] <- arr.[idx] + 1

    (* Horizontal spans *)
    for y=0 to height-1 do
        let mutable score = int scores.[y].[0] + int scores.[y].[1] + int scores.[y].[2]
        for x=3 to width-1 do
            score <- score + int scores.[y].[x];
            myincr counts (score+4) ;
            score <- score - int scores.[y].[x-3]

    (* Vertical spans *)
    for x=0 to width-1 do
        let mutable score = int scores.[0].[x] + int scores.[1].[x] + int scores.[2].[x]
        for y=3 to height-1 do
            score <- score + int scores.[y].[x];
            myincr counts (score+4);
            score <- score - int scores.[y-3].[x];

    (* Down-right (and up-left) diagonals *)
    for y=0 to height-4 do
        for x=0 to width-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                let yy = y+idx in
                let xx = x+idx in
                score <- score + int scores.[yy].[xx]
            myincr counts (score+4)

    (* up-right (and down-left) diagonals *)
    for y=3 to height-1 do
        for x=0 to width-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                let yy = y-idx in
                let xx = x+idx in
                score <- score + int scores.[yy].[xx]
            myincr counts (score+4)

    if counts.[0] <> 0 then
        yellowWins
    else if counts.[8] <> 0 then
        orangeWins
    else
        counts.[5] + 2*counts.[6] + 5*counts.[7] + 10*counts.[8] -
            counts.[3] - 2*counts.[2] - 5*counts.[1] - 10*counts.[0]

let dropDisk (board:Cell array array) column color =
    let mutable searching = true
    let mutable y = height-1
    while searching && y>=0 do
        if board.[y].[column] = Cell.Barren then
            board.[y].[column] <- color
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
        let killerTarget = match maximizeOrMinimize with true -> orangeWins | false -> yellowWins
        let mutable column = -1
        let mutable foundKiller = false
        while not foundKiller && (column<width-1) do
            column <- column+1
            if board.[0].[column] = Cell.Barren then
                let rowFilled = dropDisk board column color
                let s = scoreBoard board
                if s = killerTarget then
                    board.[rowFilled].[column] <- Cell.Barren
                    bestScore <- s
                    bestMove <- column
                    foundKiller <- true
                else
                    match abMinimax (not maximizeOrMinimize) (enum (- int color)) (depth-1) board with
                    | (moveInner,scoreInner) ->
                        board.[rowFilled].[column] <- Cell.Barren
                        if depth = maxDepth && debug then
                            Printf.printf "Depth %d, placing on %d, Score:%d\n" depth column scoreInner ;
                        if maximizeOrMinimize then
                            if scoreInner>= bestScore then
                                bestScore <- scoreInner
                                bestMove <- column
                        else
                            if scoreInner<= bestScore then
                                bestScore <- scoreInner
                                bestMove <- column
        done ;
        (Some(bestMove),bestScore)

let inArgs str args =
    any(List.ofSeq(Array.map (fun x -> (x = str)) args))

let loadBoard args =
    let board = Array.zeroCreate height
    for y=0 to height-1 do
        board.[y] <- Array.zeroCreate width
        for x=0 to width-1 do
            let orange = Printf.sprintf "o%d%d" y x
            let yellow = Printf.sprintf "y%d%d" y x
            if inArgs orange args then
                board.[y].[x] <- Cell.Orange
            else if inArgs yellow args then
                board.[y].[x] <- Cell.Yellow
            else
                board.[y].[x] <- Cell.Barren
        done
    done ;
    board

[<EntryPoint>]
let main (args:string[]) =
    let board = loadBoard args
    let scoreOrig = scoreBoard board
    debug <- inArgs "-debug" args
    if scoreOrig = orangeWins then
        printf "I win"
        -1
    elif scoreOrig = yellowWins then
        printf "You win"
        -1
    else
        let mv,score = abMinimax true Cell.Orange maxDepth board
        let msgWithColumnToPlaceOrange = 
            match mv with
            | Some column -> printfn "%A" column
            | _ -> printfn "No move possible"
        msgWithColumnToPlaceOrange
        0

// vim: set expandtab ts=8 sts=4 shiftwidth=4
