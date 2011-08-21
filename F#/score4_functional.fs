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

let scoreBoard (board:Cell array array) =
    Array.fill counts 0 9 0

    (* No need to create a "stub" - by using enums we can just operate on the board!
    let scores = Array.zeroCreate HEIGHT
    for y=0 to HEIGHT-1 do
        scores.[y] <- Array.zeroCreate WIDTH
        for x=0 to WIDTH-1 do
            scores.[y].[x] <- int board.[y].[x] *)

    let scores = board

    let inline myincr (arr:int array) idx =
        arr.[idx] <- arr.[idx] + 1

    (* Horizontal spans *)
    for y=0 to HEIGHT-1 do
        let mutable score = int scores.[y].[0] + int scores.[y].[1] + int scores.[y].[2]
        for x=3 to WIDTH-1 do
            score <- score + int scores.[y].[x];
            myincr counts (score+4) ;
            score <- score - int scores.[y].[x-3]

    (* Vertical spans *)
    for x=0 to WIDTH-1 do
        let mutable score = int scores.[0].[x] + int scores.[1].[x] + int scores.[2].[x]
        for y=3 to HEIGHT-1 do
            score <- score + int scores.[y].[x];
            myincr counts (score+4);
            score <- score - int scores.[y-3].[x];

    (* Down-right (and up-left) diagonals *)
    for y=0 to HEIGHT-4 do
        for x=0 to WIDTH-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                let yy = y+idx in
                let xx = x+idx in
                score <- score + int scores.[yy].[xx]
            myincr counts (score+4)

    (* up-right (and down-left) diagonals *)
    for y=3 to HEIGHT-1 do
        for x=0 to WIDTH-4 do
            let mutable score = 0 in
            for idx=0 to 3 do
                let yy = y-idx in
                let xx = x+idx in
                score <- score + int scores.[yy].[xx]
            myincr counts (score+4)

    if counts.[0] <> 0 then
        YELLOWWINS
    else if counts.[8] <> 0 then
        ORANGEWINS
    else
        counts.[5] + 2*counts.[6] + 5*counts.[7] + 10*counts.[8] -
            counts.[3] - 2*counts.[2] - 5*counts.[1] - 10*counts.[0]

let dropDisk (board:Cell array array) column color =
    let newBoard = Array.zeroCreate HEIGHT
    let searching = ref true
    for y=HEIGHT-1 downto 0 do
        newBoard.[y] <- Array.copy board.[y]
        if !searching && newBoard.[y].[column] = Cell.Barren then
            searching := false
            newBoard.[y].[column] <- color
    newBoard

let rec abMinimax maximizeOrMinimize color depth (board:Cell array array) =
(*
    match depth with
    | 0 -> (None,scoreBoard board)
    | _ ->
        let validMoves =
            [0 .. (WIDTH-1)] |> List.filter (fun move -> board.[0].[move] = Cell.Barren)
        match validMoves with
        | [] -> (None,scoreBoard board)
        | _  ->
            let validMovesAndBoards = validMoves |> List.map (fun move -> (move,dropDisk board move color))
            let killerMoves =
                let targetScore = if maximizeOrMinimize then ORANGEWINS else YELLOWWINS
                validMovesAndBoards |> List.map (fun (move,board) -> (move,scoreBoard board)) |>
                List.filter (fun (_,score) -> score = targetScore)
            match killerMoves with
            | (killerMove,killerScore)::rest -> (Some(killerMove), killerScore)
            | [] ->
                let validBoards = validMovesAndBoards |> List.map snd
                let bestScores = 
                    validBoards |>
                    List.map (abMinimax (not maximizeOrMinimize) (enum (- int color)) (depth-1)) |>
                    (* when loss is certain, avoid forfeiting the match, by shifting scores by depth... *)
                    List.map (fun (bmove,bscore) ->
                        let shiftedScore =
                            match bscore with
                            | ORANGEWINS | YELLOWWINS -> bscore - depth*(int color)
                            | _ -> bscore
                        (bmove,shiftedScore)) |>
(* Use this to take advantage of SMP:
                    Array.ofList |>
                    (if depth>=7 then Array.Parallel.map else Array.map) (abMinimax (not maximizeOrMinimize) (otherColor color) (depth-1)) |>
                    List.ofArray |> *)
                    List.map snd
                let allData = List.zip validMoves bestScores
                if debug && depth = MAXDEPTH then
                    List.iter (fun (move,score) ->
                        Printf.printf "Depth %d, placing on %d, Score:%d\n" depth move score) allData ;
                let best  (_,s as l) (_,s' as r) = if s > s' then l else r
                let worst (_,s as l) (_,s' as r) = if s < s' then l else r
                let bestMove,bestScore =
                    List.fold (if maximizeOrMinimize then best else worst) (List.head allData) (List.tail allData)
                (Some(bestMove),bestScore)
 *)
    let validMoves =
        [0 .. (WIDTH-1)] |> List.filter (fun move -> board.[0].[move] = Cell.Barren)
    match validMoves with
    | [] -> (None,scoreBoard board)
    | _  ->
        let movesAndBoards = validMoves |> List.map (fun column -> (column,dropDisk board column color)) in
        let movesAndScores = movesAndBoards |>  List.map (fun (column,board) -> (column,scoreBoard board)) in
        let killerMoves =
            let targetScore = if maximizeOrMinimize then ORANGEWINS else YELLOWWINS in
            movesAndScores |> List.filter (fun (_,score) -> score=targetScore) in
        match killerMoves with
        | (killerMove,killerScore)::rest -> (Some(killerMove), killerScore)
        | [] ->
            let bestScores =
                match depth with
                | 1 -> movesAndScores |> List.map snd
                | _ ->
                    movesAndBoards |> List.map snd |>
                    List.map (abMinimax (not maximizeOrMinimize) (enum (- int color)) (depth-1)) |>
                    (* when loss is certain, avoid forfeiting the match, by shifting scores by depth... *)
                    List.map (fun (bmove,bscore) ->
                        let shiftedScore =
                            match bscore with
                            | ORANGEWINS | YELLOWWINS -> bscore - depth*(int color)
                            | _ -> bscore in
                        shiftedScore) in
            let allData = List.zip validMoves bestScores in
            if debug && depth = MAXDEPTH then
                List.iter (fun (move,score) ->
                    Printf.printf "Depth %d, placing on %d, Score:%d\n" depth move score) allData ;
            let best  (_,s as l) (_,s' as r) = if s > s' then l else r
            let worst (_,s as l) (_,s' as r) = if s < s' then l else r
            let bestMove,bestScore =
                List.fold (if maximizeOrMinimize then best else worst) (List.head allData) (List.tail allData) in
            (Some(bestMove),bestScore)

let inArgs str args =
    any(List.ofSeq(Array.map (fun x -> (x = str)) args))

let loadBoard args =
    let board = Array.zeroCreate HEIGHT
    for y=0 to HEIGHT-1 do
        board.[y] <- Array.zeroCreate WIDTH
        for x=0 to WIDTH-1 do
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
