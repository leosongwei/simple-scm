(defun set-array (a shift v)
  (setf (aref a shift) v))

(defun insa ()
  (make-array 4 :initial-element 0))

(defun noarg (e tag-table)
  tag-table
  (let ((r (insa)))
    (set-array r 0 (name-to-code (car e)))
    r))

(progn
  (defvar *handler-table* nil)
  (setf *handler-table* (make-hash-table))
  
  (defmacro defass (name &rest body)
    `(setf (gethash (quote ,name) *handler-table*)
	   ,@body))
  
  (defass HALT #'noarg)
  (defass PUSH #'noarg)
  (defass POP #'noarg)
  (defass GET-GLOBAL #'noarg)

  (defass SET-GLOBAL
      (lambda (e tag-table)
	tag-table
	(let ((scode (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'set-global))
	  (set-array r 1 scode)
	  r)))
  
  (defass SET-FUNC #'noarg)

  (defass SET-STACK
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'SET-STACK))
	  (set-array r 1 n)
	  r)))

  (defass SET-ARGN
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'set-argn))
	  (set-array r 1 n)
	  r)))

  (defass GET-ARGN
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'get-argn))
	  (set-array r 1 n)
	  r)))

  (defass SET-ARI
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'set-ari))
	  (set-array r 1 n)
	  r)))

  (defass CALL #'noarg)
  (defass RETURN #'noarg)
  (defass SET-ARG #'noarg)
  (defass GET-ARG #'noarg)

  (defass SET-ADDR
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'set-addr))
	  (set-array r 1 n)
	  r)))

  (defass GET-ADDR
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'get-addr))
	  (set-array r 1 n)
	  r)))

  (defass CONSTANT
      (lambda (e tag-table)
	tag-table
	(let* ((r         (insa))
	       (type      (nth 1 e))
	       (type-code (type-to-code (nth 1 e)))
	       (value     (nth 2 e))
	       (value-code nil))
	  (if (eq type 'symbol)
	      (setf value-code (vm-intern-symbol value))
	      (setf value-code value))
	  (set-array r 0 (name-to-code 'constant))
	  (set-array r 1 type-code)
	  (set-array r 2 value-code)
	  r)))

  (defass GET-STACK
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'get-stack))
	  (set-array r 1 n)
	  r)))

  (defass GET-CLOSURE
      (lambda (e tag-table)
	tag-table
	(let ((r (insa))
	      (level (nth 1 e))
	      (n (nth 2 e)))
	  (set-array r 0 (name-to-code 'get-closure))
	  (set-array r 1 level)
	  (set-array r 2 n)
	  r)))

  (defass SET-CLOSURE
      (lambda (e tag-table)
	tag-table
	(let ((r (insa))
	      (level (nth 1 e))
	      (n (nth 2 e)))
	  (set-array r 0 (name-to-code 'set-closure))
	  (set-array r 1 level)
	  (set-array r 2 n))))

  (defass GET-CLOSURE-LOCAL
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'get-closure-local))
	  (set-array r 1 n)
	  r)))

  (defass FIX-CLOSURE
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'fix-closure))
	  (set-array r 1 n)
	  r)))

  (defass ADD1 #'noarg)
  (defass SUB1 #'noarg)
  (defass NEQ #'noarg)
  
  (defass JMPF
      (lambda (e tag-table)
	(let ((shift (gethash (nth 1 e) tag-table))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'jmpf))
	  (set-array r 1 shift)
	  r)))
  
  (defass JMPT
      (lambda (e tag-table)
	(let ((shift (gethash (nth 1 e) tag-table))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'jmpt))
	  (set-array r 1 shift)
	  r)))

  (defass SGOTO
      (lambda (e tag-table)
	(let ((shift (gethash (nth 1 e) tag-table))
	      (r     (insa)))
	  (set-array r 0 (name-to-code 'sgoto))
	  (set-array r 1 shift)
	  r))))

(defun assembly-list (list)
  (let ((code-length
	 (length (remove-if #'symbolp list)))
	(tag-table (make-hash-table)))
    (let ((index 0))
      (dolist (i list)
	(if (symbolp i)
	    (setf (gethash i tag-table) index)
	    (incf index))))
    (let ((a (make-array (* 4 code-length) :initial-element 0))
	  (index 0))
      (dolist (e list)
	(if (symbolp e)
	    nil
	    (let* ((name (car e))
		   (code-array (funcall (gethash name *handler-table*)
					e tag-table)))
	      (dotimes (i 4)
		(setf (aref a (+ index i)) (aref code-array i)))
	      (incf index 4))))
      a)))

(defun print-byte-code-nicely (array start end)
  (if (not (= (mod (- end start) 4)))
      (error "Invalid input")
      nil)
  (let ((length (/ (- end start) 4)))
    (dotimes (i length)
      (dotimes (j 4)
	(princ (aref array (+ (* 4 i) j)))
	(format t "~t"))
      (format t "~%"))))

(defun print-byte-code-array (a)
  (let* ((length (array-dimension a 0)))
    (print-byte-code-nicely a 0 length)))

(defun hard-code (target addr code)
  (let ((length (array-dimension code 0)))
    (dotimes (i length)
      (setf (aref target (+ addr i))
	    (aref code i)))
    'done))

(defun build-function (arity closure-length closure-map list)
  (let* ((a (assembly-list list))
	 (body-length (array-dimension a 0))
	 (total-length (+ 5 closure-length body-length))
	 (r (make-array total-length :initial-element 0))
	 (body-shift (+ 5 closure-length)))
    (setf (aref r 0) total-length)
    (setf (aref r 1) arity)
    (setf (aref r 2) 6) ;; stack-length

    ;; closure
    (setf (aref r 3) closure-length)
    (dotimes (i closure-length)
      (setf (aref r (+ 4 i)) (aref closure-map i)))

    ;; body
    (setf (aref r (+ 4 closure-length)) body-length)
    (dotimes (i body-length)
      (setf (aref r (+ body-shift i))
	    (aref a i)))
    r))

(defun byte-code-2-function (addr)
  (let* ((length         (get-heap addr 0))
	 (arity          (get-heap addr 1))
	 (stack-length   (get-heap addr 2))
	 (closure-length (get-heap addr 3))
	 (closure-map    (let ((a (make-array closure-length)))
			   (dotimes (i closure-length)
			     (setf (aref a i)
				   (get-heap addr (+ 4 i))))
			   a))
	 (body-length    (get-heap addr (+ 4 closure-length)))
	 (body-exact     (+ addr 5 closure-length))
	 (body           (let ((a (make-array body-length)))
			   (dotimes (i body-length)
			     (setf (aref a i)
				   (get-heap body-exact i)))
			   a)))
    (format t "ADDR: ~A~%" addr)
    (format t "LEN: ~A, ARITY:~A stack-length:~A.~%"
	    length arity stack-length)
    (format t "closure, length: ~A, map: ~A.~%"
	    closure-length closure-map)
    (format t "body, length: ~A~%" body-length)
    (dotimes (i (/ body-length 4))
      (format t "~A. " i)
      (dotimes (j 4)
	(format t "~A " (aref body (+ (* 4 i) j))))
      (format t "~%"))
    (format t "---")))
