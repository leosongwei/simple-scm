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
  (defass SET-FUNC #'noarg)

  (defass SET-ARGN
      (lambda (e tag-table)
	tag-table
	(let ((n (nth 1 e))
	      (r (insa)))
	  (set-array r 0 (name-to-code 'set-argn))
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
	(let ((r          (insa))
	      (type-code  (type-to-code (nth 1 e)))
	      (value-code (nth 2 e)))
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
	  (set-array r 2 n))))

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
	  (set-array r 0 (name-to-code 'get-closure-local))
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
    (setf (aref r 3) closure-length)
    (dotimes (i closure-length)
      (setf (aref r (+ 4 i)) (aref closure-map i)))
    (setf (aref a (+ 4 closure-length)) body-length)
    (dotimes (i body-length)
      (setf (aref r (+ body-shift i))
	    (aref a i)))
    r))
