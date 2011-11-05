(defconstant width 7)
(defconstant height 6)
(defconstant orangeWins 1000000)
(defconstant yellowWins -1000000)
(defparameter *debug* t)
(defparameter *maxDepth* 7)

;SBCL specific compiler notes
;(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

; Give me speed!
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro at (y x)
  `(aref board ,y ,x))

(defmacro myincr ()
  `(incf (aref counts (+ 4 score))))

; My first *real* macro: it unrolls the loops done in
; the horizontal spans checking at compile-time!
;
; Mysteries: Both SBCL and CMUCL speed increases by more than 
; 15% because of this loop-unrolling - but for some weird
; reason, the declare fixnum for the score... makes SBCL
; 40% slower, so I have it commented out... which makes
; CMUCL complain during optimization. Go figure :-)
;
(defmacro horizontal-spans ()
  `(progn
    (let ((score 0))
    ;(declare (type fixnum score))
    ,@(loop for y fixnum from 0 to (1- height)
      collect `(setf score (+ (at ,y 0) (at ,y 1) (at ,y 2)))
      nconc (loop for x fixnum from 3 to (1- width)
        collect `(incf score (at ,y ,x))
        collect `(myincr)
        collect `(decf score (at ,y (- ,x 3)))
        )))))

; Mysteries continue - the horizontal-spans loop-unrolling
; improved the speed a lot, but the vertical-spans ...
; don't improve speed at ALL. I am guessing I am in cache-line
; phenomena... Oh, and the "declare fixnum" for the score
; doesn't impact SBCL speed here...
(defmacro vertical-spans ()
  `(progn
    (let ((score 0))
    (declare (type fixnum score))
    ,@(loop for x fixnum from 0 to (1- width)
      collect `(setf score (+ (at 0 ,x) (at 1 ,x) (at 2 ,x)))
      nconc (loop for y fixnum from 3 to (1- height)
        collect `(incf score (at ,y ,x))
        collect `(myincr)
        collect `(decf score (at (- ,y 3) ,x))
        )))))

(declaim (inline scoreBoard))
(defun scoreBoard (board)
  (declare (type (simple-array fixnum (6 7)) board))
  (let ((counts (make-array '(9) :initial-element 0 :element-type 'fixnum)))
    ; Horizontal spans - normal code
    ;(loop for y from 0 to (1- height) do
    ;  (let ((score (+ (at y 0)  (at y 1) (at y 2))))
    ;    (declare (type fixnum score))
    ;    (loop for x from 3 to (1- width) do
    ;      (incf score (at y x))
    ;      (myincr)
    ;      (decf score (at y (- x 3))))))
    
    ; Loop-unrolling done via this macro:
    (horizontal-spans)

    ; Vertical spans - loop unrolling macro makes code slower... :-(
    ;(loop for x from 0 to (1- width) do
    ;  (let ((score (+ (at 0 x) (at 1 x) (at 2 x))))
    ;    (declare (type fixnum score))
    ;    (loop for y from 3 to (1- height) do
    ;      (incf score (at y x))
    ;      (myincr)
    ;      (decf score (at (- y 3) x)))))
    ;
    ; Loop-unrolling done via this macro:
    (vertical-spans)

    ; Down-right (and up-left) diagonals
    (loop for y fixnum from 0 to (- height 4) do
      (loop for x fixnum from 0 to (- width 4) do
        (let ((score 0))
          (declare (type fixnum score))
          (loop for idx fixnum from 0 to 3 do
            (incf score (at (+ y idx) (+ x idx))))
          (myincr))))

    ; up-right (and down-left) diagonals
    (loop for y fixnum from 3 to (1- height) do
      (loop for x fixnum from 0 to (- width 4) do
        (let ((score 0))
          (declare (type fixnum score))
          (loop for idx fixnum from 0 to 3 do
            (incf score (at (- y idx) (+ x idx))))
          (myincr))))
;
;For down-right and up-left diagonals, I also tried this incremental version
;of the diagonal scores calculations... It is doing less computation than
;the alternative above, but unfortunately, the use of the two tuple lists
;makes the overall results worse in my Celeron E3400... I suspect
;because the access to the list triggers cache misses.
;
;Outside, in global space:
;
;    ; anchors to start calculating scores while moving down right *)
;    let dr = [| (2,0);(1,0);(0,0);(0,1);(0,2);(0,3) |]
;    ; anchors to start calculating scores while moving down left *)
;    let dl = [| (0,3);(0,4);(0,5);(0,6);(1,6);(2,6) |]
;
;And in this function, using the anchors to do the calculation incrementally,
;just as we do for vertical and horizontal spaces:
;
;    ; Down-right (and up-left) diagonals *)
;    for idx=0 to 5 do
;        let (yinit, xinit) = dr.(idx) in
;        let y = ref yinit in
;        let x = ref xinit in
;        let score = ref (board.(!y).(!x) + board.(!y + 1).(!x + 1) + board.(!y + 2).(!x + 2)) in
;        while !y+3<=height-1 && !x+3<=width-1 do
;            score := !score + board.(!y+3).(!x+3) ;
;            myincr counts (!score+4) ;
;            score := !score - board.(!y).(!x) ;
;            y := !y+1 ;
;            x := !x+1 ;
;        done
;    done ;
;
;    ; Down-left (and up-right) diagonals *)
;    for idx=0 to 5 do
;        let (yinit, xinit) = dl.(idx) in
;        let y = ref yinit in
;        let x = ref xinit in
;        let score = ref (board.(!y).(!x) + board.(!y + 1).(!x - 1) + board.(!y + 2).(!x - 2)) in
;        while !y+3<=height-1 && !x-3>=0 do
;            score := !score + board.(!y+3).(!x-3) ;
;            myincr counts (!score+4) ;
;            score := !score - board.(!y).(!x) ;
;            y := !y+1 ;
;            x := !x-1 ;
;        done
;    done ;

    (let ((result
	    (cond
	      ((/= (aref counts 0) 0) yellowWins)
	      ((/= (aref counts 8) 0) orangeWins)
	      (t
		(the fixnum (-
		  (+
		    (aref counts 5)
		    (the fixnum (* 2 (aref counts 6)))
		    (the fixnum (* 5 (aref counts 7)))
		    (the fixnum (* 10 (aref counts 8))))
		  (+
		    (aref counts 3)
		    (the fixnum (* 2 (aref counts 2)))
		    (the fixnum (* 5 (aref counts 1)))
		    (the fixnum (* 10 (aref counts 0))))))))))
      (declare (type fixnum result))
      result)))

(declaim (inline dropDisk))
(defun dropDisk (board column color)
  (declare (type (simple-array fixnum (6 7)) board) (type fixnum column color))
  (loop for y fixnum from (1- height) downto 0 do
    (cond
      ((= 0 (at y column))
        (progn
          (setf (at y column) color)
          (return-from dropDisk y)))))
  -1)

(defun minimax (maximizeOrMinimize color depth board)
  (declare (type fixnum color depth) (type (simple-array fixnum (6 7)) board))
  (let ((bestScore (cond (maximizeOrMinimize yellowWins) (t orangeWins)))
        (bestMove -1)
        (killerTarget (cond (maximizeOrMinimize orangeWins) (t yellowWins))))
    (loop for column fixnum from 0 to (1- width) do
      (if (= 0 (at 0 column))
        (let ((rowFilled (dropDisk board column color))
              (s (scoreBoard board)))
          (cond
            ((= s killerTarget) (progn
                                  (setf (at rowFilled column) 0)
                                  (return-from minimax (list column s))))
            (t (progn
                 (let* ((result (cond
                                   ((= depth 1) (list column s))
                                   (t (minimax (not maximizeOrMinimize) (- color) (1- depth) board))))
                           (scoreInner (cadr result))
                           (shiftedScore
                             ; when loss is certain, avoid forfeiting the match, by shifting scores by depth...
                             (if (or (= scoreInner orangeWins) (= scoreInner yellowWins))
                               (- scoreInner (the fixnum (* depth color)))
                               scoreInner)))
		      (declare (type fixnum scoreInner))
		      (declare (type fixnum shiftedScore))
		      (declare (type fixnum *maxDepth*))
                      (setf (at rowFilled column) 0)
                      (if (and *debug* (= depth *maxDepth*))
                        (format t "Depth ~A, placing on ~A, Score:~A~%" depth column shiftedScore))
                      (if maximizeOrMinimize
                        (if (>= shiftedScore bestScore)
                          (progn
                            (setf bestScore shiftedScore)
                            (setf bestMove column)))
                        (if (<= shiftedScore bestScore)
                          (progn
                            (setf bestScore shiftedScore)
                            (setf bestMove column)))))))))))
    (list (the fixnum bestMove) (the fixnum bestScore))))

;(* let any = List.fold_left (||) false
; * ..is slower than ... *)
;let rec any l =
;    match l with
;    | []        -> false
;    | true::xs  -> true
;    | false::xs -> any xs
;
;let inArgs args str =
;    any(Array.to_list (Array.map (fun x -> (x = str)) args))
;
;let loadBoard args =
;    let board = Array.make_matrix height width 0 in
;    for y=0 to height-1 do
;        for x=0 to width-1 do
;            let orange = Printf.sprintf "o%d%d" y x in
;            let yellow = Printf.sprintf "y%d%d" y x in
;            if inArgs args orange then
;                board.(y).(x) <- 1
;            else if inArgs args yellow then
;                board.(y).(x) <- -1
;            else
;                board.(y).(x) <- 0
;        done
;    done ;
;    board

;let _ =
;    let board = loadBoard Sys.argv in
;    let scoreOrig = scoreBoard board in
;    debug := inArgs Sys.argv "-debug" ;
;    if !debug then
;        Printf.printf "Starting score: %d\n" scoreOrig ;
;    if scoreOrig = orangeWins then begin
;        Printf.printf "I win\n" ;
;        (-1)
;    end else if scoreOrig = yellowWins then begin
;        Printf.printf "You win\n" ;
;        (-1)
;    end else
;        let mv,score = abMinimax true 1 maxDepth board in
;        match mv with
;        | Some column -> Printf.printf "%d\n" column ; 0
;        | _ -> failwith "No move possible"

(defun bench ()
  (let
    ((board (make-array '(6 7) :initial-element 0 :element-type 'fixnum)))
    (setf (at 5 3) 1)
    (setf (at 4 3) -1)
    (time (format t "~A" (minimax t 1 *maxDepth* board)))))

(bench)
(quit)

; vim: set expandtab ts=8 sts=2 shiftwidth=2
