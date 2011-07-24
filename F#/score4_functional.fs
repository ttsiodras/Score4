open System.Collections.Generic

let width = 7
let height = 6
let maxDepth = 7
let orangeWins = 1000000
let yellowWins = -orangeWins
let debug = ref true

type Cell =
    | Orange = 1
    | Yellow = -1
    | Barren = 0

let rec any l =
    match l with
    | []        -> false
    | true::xs  -> true
    | false::xs -> any xs

let otherColor color = 
    match color with
        | Cell.Orange -> Cell.Yellow
        | Cell.Yellow -> Cell.Orange
        | _      -> Cell.Barren

(* diagonal, down-right *)
let negativeSlope = [| (0,0); (1,1);   (2,2);   (3,3)  |]

(* diagonal, up-right   *)
let positiveSlope = [| (0,0); (-1,1);  (-2,2);  (-3,3) |]

let scoreBoard (board:Cell array array) =
    let counts = [| 0;0;0;0;0;0;0;0;0 |]
    let scores = Array.zeroCreate height
    for y=0 to height-1 do
        scores.[y] <- Array.zeroCreate width
        for x=0 to width-1 do
            scores.[y].[x] <- int board.[y].[x]

    let inline myincr (arr:int array) idx =
        arr.[idx] <- arr.[idx] + 1

    (* Horizontal spans *)
    for y=0 to height-1 do
        let mutable score = scores.[y].[0] + scores.[y].[1] + scores.[y].[2]
        for x=3 to width-1 do
            score <- score + scores.[y].[x];
            myincr counts (score+4) ;
            score <- score - scores.[y].[x-3]

    (* Vertical spans *)
    for x=0 to width-1 do
        let mutable score = scores.[0].[x] + scores.[1].[x] + scores.[2].[x]
        for y=3 to height-1 do
            score <- score + scores.[y].[x];
            myincr counts (score+4);
            score <- score - scores.[y-3].[x];

    (* Down-right (and up-left) diagonals *)
    for y=0 to height-4 do
        for x=0 to width-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                match negativeSlope.[idx] with
                | (yofs,xofs) ->
                    let yy = y+yofs in
                    let xx = x+xofs in
                    score <- score + scores.[yy].[xx]
            myincr counts (score+4)

    (* up-right (and down-left) diagonals *)
    for y=3 to height-1 do
        for x=0 to width-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                match positiveSlope.[idx] with
                | (yofs,xofs) ->
                    let yy = y+yofs in
                    let xx = x+xofs in
                    score <- score + scores.[yy].[xx]
            myincr counts (score+4)

    if counts.[0] <> 0 then
        yellowWins
    else if counts.[8] <> 0 then
        orangeWins
    else
        counts.[5] + 2*counts.[6] + 5*counts.[7] + 10*counts.[8] -
            counts.[3] - 2*counts.[2] - 5*counts.[1] - 10*counts.[0]

let dropDisk (board:Cell array array) column color =
    let newBoard = Array.zeroCreate height
    let searching = ref true
    for y=height-1 downto 0 do
        newBoard.[y] <- Array.copy board.[y]
        if !searching && newBoard.[y].[column] = Cell.Barren then
            searching := false
            newBoard.[y].[column] <- color
    newBoard

let rec abMinimax maximizeOrMinimize color depth board =
    match depth with
    | 0 -> (None,scoreBoard board)
    | _ ->
        let validMoves =
            [0 .. (width-1)] |> List.filter (fun move -> board.[0].[move] = Cell.Barren)
        match validMoves with
        | [] -> (None,scoreBoard board)
        | _  ->
            let validMovesAndBoards = validMoves |> List.map (fun move -> (move,dropDisk board move color))
            let killerMoves =
                let targetScore = if maximizeOrMinimize then orangeWins else yellowWins
                validMovesAndBoards |> List.map (fun (move,board) -> (move,scoreBoard board)) |>
                List.filter (fun (_,score) -> score = targetScore)
            match killerMoves with
            | (killerMove,killerScore)::rest -> (Some(killerMove), killerScore)
            | [] ->
                let validBoards = validMovesAndBoards |> List.map snd
                let bestScores = 
                    validBoards |>
                    List.map (abMinimax (not maximizeOrMinimize) (otherColor color) (depth-1)) |>
(* Use this to take advantage of SMP:
                    Array.ofList |>
                    (if depth>=7 then Array.Parallel.map else Array.map) (abMinimax (not maximizeOrMinimize) (otherColor color) (depth-1)) |>
                    List.ofArray |> *)
                    List.map snd
                let allData = List.zip validMoves bestScores
                if !debug && depth = maxDepth then
                    List.iter (fun (move,score) ->
                        Printf.printf "Depth %d, placing on %d, Score:%d\n" depth move score) allData ;
                let best  (_,s as l) (_,s' as r) = if s > s' then l else r
                let worst (_,s as l) (_,s' as r) = if s < s' then l else r
                let bestMove,bestScore =
                    List.fold (if maximizeOrMinimize then best else worst) (List.head allData) (List.tail allData)
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
    let debug = inArgs "-debug" args
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
