(with-compilation-unit
  (:optimize
    '(optimize
      (speed 3) (safety 0) (debug 0) (space 0)))
  (compile-file 
    "compile-time-functions.cl"
    :Byte-Compile nil
    :load t
    :Block-Compile t
    :Byte-Compile nil
    :print nil
    :output-file "compile-time-functions.fasl")
  (compile-file
    "score4.cl"
    :Byte-Compile nil
    :load t
    :Block-Compile t
    :Byte-Compile nil
    :print nil
    :output-file "score4.fasl"))
(quit)
