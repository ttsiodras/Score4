
(defun operation-p (x)
  (member x '(+ 1+ - 1- * /)))

(defun clone (sexpr)
  (cond 
    ((listp sexpr)
     (if (null sexpr)
       ()
       (let ((hd (car sexpr))
	     (tl (cdr sexpr)))
	 ;(format t "hd:~A~%tl:~A~%op:~A~%~%" hd tl (operation-p hd))
	 (cond
	   ((listp hd) (append (list (clone hd)) (clone tl)))
	   ((operation-p hd) (list 'the 'fixnum (cons hd (clone tl))))
	   (t (cons hd (clone tl)))))))
    (t sexpr)))

