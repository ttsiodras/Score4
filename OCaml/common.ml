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

let ( |> ) x fn = fn x

let inside y x =
    y>=0 && y<height && x>=0 && x<width

let otherColor color = -color

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

(* 

For down-right and up-left diagonals, I also tried this incremental version
of the diagonal scores calculations... It is doing less computation than
the alternative above, but unfortunately, the use of the two tuple lists 
makes the overall results worse in my Celeron E3400... I suspect
because the access to the list triggers cache misses.

Outside, in global space:

    (* anchors to start calculating scores while moving down right *)
    let dr = [| (2,0);(1,0);(0,0);(0,1);(0,2);(0,3) |]
    (* anchors to start calculating scores while moving down left *)
    let dl = [| (0,3);(0,4);(0,5);(0,6);(1,6);(2,6) |]

And in this function, using the anchors to do the calculation incrementally,
just as we do for vertical and horizontal spaces:

    (* Down-right (and up-left) diagonals *)
    for idx=0 to 5 do
        let (yinit, xinit) = dr.(idx) in
        let y = ref yinit in
        let x = ref xinit in
        let score = ref (board.(!y).(!x) + board.(!y + 1).(!x + 1) + board.(!y + 2).(!x + 2)) in
        while !y+3<=height-1 && !x+3<=width-1 do
            score := !score + board.(!y+3).(!x+3) ;
            myincr counts (!score+4) ;
            score := !score - board.(!y).(!x) ;
            y := !y+1 ;
            x := !x+1 ;
        done
    done ;

    (* Down-left (and up-right) diagonals *)
    for idx=0 to 5 do
        let (yinit, xinit) = dl.(idx) in
        let y = ref yinit in
        let x = ref xinit in
        let score = ref (board.(!y).(!x) + board.(!y + 1).(!x - 1) + board.(!y + 2).(!x - 2)) in
        while !y+3<=height-1 && !x-3>=0 do
            score := !score + board.(!y+3).(!x-3) ;
            myincr counts (!score+4) ;
            score := !score - board.(!y).(!x) ;
            y := !y+1 ;
            x := !x-1 ;
        done
    done ;

*)

    if counts.(0) <> 0 then
        yellowWins
    else if counts.(8) <> 0 then
        orangeWins
    else
        counts.(5) + 2*counts.(6) + 5*counts.(7) -
            counts.(3) - 2*counts.(2) - 5*counts.(1)

