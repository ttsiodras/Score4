(defconstant width 7)
(defconstant height 6)
(defconstant orangeWins 1000000)
(defconstant yellowWins -1000000)
(defparameter *debug* nil)
(defparameter *maxDepth* 7)

; Give me speed!
(declaim (optimize (speed 3) (safety 0) (debug 0)))

; in the same vein (speed) we need (in many places) to specify
; that the result of an operation fits in a fixnum
; so we macro (the fixnum (...))
(defmacro fast (&rest sexpr)
  `(,@(clone sexpr)))

(defmacro at (y x)
  ; we emulate a 6x7 board with a 6x7 = 42 one-dimensional one
  (let ((final (clone (list '+ (list '* 7 y) x))))
    `(aref board ,final)))

; The scoreBoard function adds the board values on 4 consecutive
; cells, and therefore the result spans from -4 to 4 (9 values)
; This macro updates the "counts" 1D array of these 9 possible
; values (cumulative frequencies of values seen)
(defmacro myincr ()
  `(incf (aref counts (+ 4 score))))

;
;; My first *real* macros: they unroll the loops done in
; the spans checking at compile-time!
;
; I finally understand why LISP macros are powerful.
; God, they really are... By unrolling the loops at
; compile time via the 4 "-spans" macros, speed is now
; better than OCaml!
;
(defmacro horizontal-spans ()
  ; normal code is...
  ;
  ;(loop for y fixnum from 0 to (1- height) do
  ;  (let ((score (+ (at y 0)  (at y 1) (at y 2))))
  ;    (declare (type fixnum score))
  ;    (loop for x fixnum from 3 to (1- width) do
  ;      (incf score (at y x))
  ;      (myincr)
  ;      (decf score (at y (- x 3))))))
  ;
  ; Loop-unrolling done via this macro:
  ;
  `(progn
    (let ((score 0))
      (declare (type fixnum score))
      ,@(loop for y fixnum from 0 to (1- height)
              ; first 3 of the total 4 cells
              collect `(setf score (+ (at ,y 0) (at ,y 1) (at ,y 2)))
              nconc (loop for x fixnum from 3 to (1- width)
                          ; add the 4th one
                          collect `(incf score (at ,y ,x))
                          ; update counts
                          collect `(myincr)
                          ; if we re still in bounds, remove 1st of the old 4
                          if (/= x (1- width))
                          collect `(decf score (at ,y ,(- x 3))))))))

(defmacro vertical-spans ()
  ; normal code is...
  ;
  ;(loop for x fixnum from 0 to (1- width) do
  ;  (let ((score (+ (at 0 x) (at 1 x) (at 2 x))))
  ;    (declare (type fixnum score))
  ;    (loop for y fixnum from 3 to (1- height) do
  ;      (incf score (at y x))
  ;      (myincr)
  ;      (decf score (at (- y 3) x)))))
  ;
  ; Loop-unrolling done via this macro:
  ;
  `(progn
     (let ((score 0))
       (declare (type fixnum score))
       ,@(loop for x fixnum from 0 to (1- width)
              ; first 3 of the total 4 cells
               collect `(setf score (+ (at 0 ,x) (at 1 ,x) (at 2 ,x)))
               nconc (loop for y fixnum from 3 to (1- height)
                           ; add the 4th one
                           collect `(incf score (at ,y ,x))
                           ; update counts
                           collect `(myincr)
                           ; if we re still in bounds, remove 1st of the old 4
                           if (/= y (1- height))
                           collect `(decf score (at ,(- y 3) ,x)))))))

(defmacro downright-spans ()
;
;  normal code is...
;
;    (loop for y fixnum from 0 to (- height 4) do
;      (loop for x fixnum from 0 to (- width 4) do
;        (let ((score 0))
;          (declare (type fixnum score))
;          (loop for idx fixnum from 0 to 3 do
;            (incf score (at (+ y idx) (+ x idx))))
;          (myincr))))
;
;   1st generation loop-unrolling done via this macro:
;
;  `(progn
;    (let ((score 0))
;    (declare (type fixnum score))
;    ,@(loop for y fixnum from 0 to (- height 4)
;      nconc (loop for x fixnum from 0 to (- width 4)
;        collect `(setf score 0)
;        nconc (loop for idx fixnum from 0 to 3
;        collect `(incf score (at ,(fast + y idx) ,(fast + x idx))))
;      collect `(myincr)
;      )))))
;
;  Final version: incrementally create the score4 values,
;  keeping score as you move down right
;
;  - accumulate first 4
;  - update counts
;  - remove the 1st of the 4 cells
;  - move down-right
;  - add the new cell
;  - update the counts
;  etc

  (let ((y 0)
        (x 0))
    (declare (type fixnum y x))
    `(progn
       (let ((score 0))
         (declare (type fixnum score))
         ; anchors to start calculating score4s while moving down right
         ,@(let ((dr '((2 0) (1 0) (0 0) (0 1) (0 2) (0 3))))
             (loop for startposTuple in dr
                   do (setf y (car startposTuple)) (setf x (cadr startposTuple))
                   ; first 3 of the total 4 cells
                   collect `(setf score (fast + (at ,y ,x) (at ,(fast 1+ y) ,(fast 1+ x)) (at ,(fast + y 2) ,(fast + x 2))))
                   nconc (loop while (and (<= (+ y 3) (1- height)) (<= (+ x 3) (1- width)))
                               ; add the 4th one
                               collect `(incf score (at ,(fast + y 3) ,(fast + x 3)))
                               ; update counts
                               collect `(myincr)
                               ; move to next cell
                               do (incf y) (incf x)
                               ; if we re still in bounds, remove 1st of the old 4
                               if (and (<= (+ y 3) (1- height)) (<= (+ x 3) (1- width)))
                               collect `(decf score (at ,(fast 1- y) ,(fast 1- x))))))))))

(defmacro downleft-spans ()
;
;  normal code is...
;
;    (loop for y fixnum from 3 to (1- height) do
;      (loop for x fixnum from 0 to (- width 4) do
;        (let ((score 0))
;          (declare (type fixnum score))
;          (loop for idx fixnum from 0 to 3 do
;            (incf score (at (- y idx) (+ x idx))))
;          (myincr))))
;
;  1st generation of loop-unrolling done via this macro:
;
;  `(progn
;    (let ((score 0))
;    (declare (type fixnum score))
;    ,@(loop for y fixnum from 3 to (1- height)
;      nconc (loop for x fixnum from 0 to (- width 4)
;        collect `(setf score 0)
;        nconc (loop for idx fixnum from 0 to 3
;        collect `(incf score (at ,(fast - y idx) ,(fast + x idx))))
;      collect `(myincr))))))
;
;  Final version: incrementally create the score4 values,
;  keeping score as you move down left
;
;  - accumulate first 4
;  - update counts
;  - remove the 1st of the 4 cells
;  - move down-left
;  - add the new cell
;  - update the counts
;  etc

  (let ((y 0)
        (x 0))
    (declare (type fixnum y x))
    `(progn
       (let ((score 0))
         (declare (type fixnum score))
         ; anchors to start calculating score4s while moving down left
         ,@(let ((dl '((0 3) (0 4) (0 5) (0 6) (1 6) (2 6))))
             (loop for startposTuple in dl
                   do (setf y (car startposTuple)) (setf x (cadr startposTuple))
                   ; first 3 of the total 4 cells
                   collect `(setf score (fast + (at ,y ,x) (at ,(fast 1+ y) ,(fast 1- x)) (at ,(fast + y 2) ,(fast - x 2))))
                   nconc (loop while (and (<= (+ y 3) (1- height)) (>= (- x 3) 0))
                               ; add the 4th one
                               collect `(incf score (at ,(fast + y 3) ,(fast - x 3)))
                               ; update counts
                               collect `(myincr)
                               ; move to next cell
                               do (incf y) (decf x)
                               ; if we re still in bounds, remove 1st of the old 4
                               if (and (<= (+ y 3) (1- height)) (>= (- x 3) 0))
                               collect `(decf score (at ,(fast 1- y) ,(fast 1+ x))))))))))

(declaim (inline scoreBoard))
(defun scoreBoard (board)
  (declare (type (simple-array fixnum (42)) board))
  (let ((counts (make-array '(9) :initial-element 0 :element-type 'fixnum)))

    ; we add the board values on 4 consecutive cells, and therefore
    ; get a result that can be from -4 to 4 (9 possible values).
    ; We then update the "counts" 1D array of these 9 possible
    ; values (cumulative frequencies of values seen).
    ;
    ; This is done via the following 4 macros,
    ; which unroll (at compile-time!) the computations necessary
    ; (use macroexpand to marvel at their glory...)
    ;
    (horizontal-spans)
    (vertical-spans)
    (downright-spans)
    (downleft-spans)

    (cond
      ((/= (aref counts 0) 0) yellowWins)
      ((/= (aref counts 8) 0) orangeWins)
      (t (let* ((forOrange (fast + (aref counts 5)
                              (* 2 (aref counts 6))
                              (* 5 (aref counts 7))))
                (forYellow (fast + (aref counts 3)
                              (* 2 (aref counts 2))
                              (* 5 (aref counts 1))))
                (result (fast - forOrange forYellow)))
           (declare (type fixnum forOrange forYellow result))
           result)))))

(declaim (inline dropDisk))
(defun dropDisk (board column color)
  (declare (type (simple-array fixnum (42)) board) (type fixnum column color))
  (loop for y fixnum from (1- height) downto 0
        do (cond ((= 0 (at y column))
                  (progn
                    (setf (at y column) color)
                    (return-from dropDisk y)))))
  -1)

(defun minimax (maximizeOrMinimize color depth board)
  (declare (type fixnum color depth) (type (simple-array fixnum (42)) board))
  (let ((bestScore (cond (maximizeOrMinimize yellowWins) (t orangeWins)))
        (bestMove -1)
        (killerTarget (cond (maximizeOrMinimize orangeWins) (t yellowWins))))
    (declare (type fixnum bestScore bestMove))
    (loop for column fixnum from 0 to (1- width)
          do (if (= 0 (at 0 column))
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
                                   (- scoreInner (fast * depth color))
                                   scoreInner)))
                          (declare (type fixnum scoreInner shiftedScore *maxDepth*))
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
    (list bestMove bestScore)))

(defun loadboard (args)
  (declare (type list args))
  (let ((board (make-array 42 :initial-element 0 :element-type 'fixnum)))
    (format t "~A~%" args)
    (loop for y fixnum from 0 to (1- height)
          do (loop for x fixnum from 0 to (1- width)
                   do (let ((orange (format nil "o~A~A" y x))
                            (yellow (format nil "y~A~A" y x)))
                        (if (find orange args :test #'equal)
                          (setf (at y x) 1))
                        (if (find yellow args :test #'equal)
                          (setf (at y x) -1)))))
    board))

(defun bench ()
  (let
    ; we emulate a 6x7 board with a 6x7 = 42 one-dimensional one
    ((board (make-array 42 :initial-element 0 :element-type 'fixnum)))
    (setf (at 5 3) 1)
    (setf (at 4 3) -1)
    (dotimes (n 10)
      (time (format t "~A" (minimax t 1 *maxDepth* board))))))

(defun my-command-line ()
  (or
    #+SBCL *posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    nil))

(defun main ()
  (let ((args (my-command-line))
        (exitCode 0))
    (declare (type list args))
    (cond
      ; no arguments?
      ((<= (length args) 1)
       ; then benchmark speed
           (progn
             (format t "Benchmarking...~%")
             (bench)))
      ; parse board from arguments...
      (t (let* ((board (loadboard args))
                (scoreOrig (scoreBoard board)))
           ; set debug mode if user requested it
           (if (find "-debug" args :test #'equal)
             (setf *debug* t))
           (if *debug* (format t "Starting score: ~A~%" scoreOrig))
           ; check if we are already in win/lose board
           (cond ((= scoreOrig orangeWins)
                  (progn
                    (print "I win")
                    (setf exitCode -1)))
                 ((= scoreOrig yellowWins)
                  (progn
                    (print "You win")
                    (setf exitCode -1)))
                 ; we're not win/lose, we need to minimax
                 (t (let ((result (minimax t 1 *maxDepth* board)))
                      (format t "~A~%" (car result))
                      (setf exitCode 0)))))))
    exitCode))

;(main)
;(or #+SBCL (quit))

; to create a standalone executable with SBCL, comment out the quit above,
; then...
;
; (load "score4.cl")
; (sb-ext:save-lisp-and-die "score4.exe" :executable t )
;
; Then, when you spawn "score4.exe",
; just invoke (main)
;
; vim: set expandtab ts=8 sts=2 shiftwidth=2
