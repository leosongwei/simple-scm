(defun make-simple-closure (arity list)
  "MAKE-SIMPLE-CLOSURE
   make closure that have no heap access, danger!"
  (let* ((binary (build-function arity 0 #() list))
	 (length (1+ (array-dimension binary 0)))
	 (closure-binary
	  (make-array length :initial-element 0)))
    ;; just leave the closure space pointer 0
    (dotimes (i (1- length))
      (setf (aref closure-binary i)
	    (aref binary i)))
    (let ((start (alloc-heap length)))
      (dotimes (i length)
	(setf (aref *heap* (+ start i))
	      (aref closure-binary i)))
      start)))

(defun def-internal-func (name arity list)
  (let* ((v-addr (alloc-heap 2))
	 (start (make-simple-closure arity list)))
    (setf (gethash (vm-intern-symbol name) *global-alist*) v-addr)
    (setf (aref *heap* v-addr) #.(type-to-code 'closure))
    (setf (aref *heap* (1+ v-addr)) start)
    (setf (gethash name *global-env*)
	  (make-vari :name name
		     :type 'global
		     :n nil))))

(progn
  (def-internal-func '+ 2
    '((get-argn 1)
      (push)
      (get-argn 2)
      (push)
      (pop)
      (set-argn 1)
      (pop)
      (add1)
      (return)))

  (def-internal-func '- 2
    '((get-argn 1)
      (push)
      (get-argn 2)
      (set-argn 1)
      (pop)
      (sub1)
      (return)))

  (def-internal-func '= 2
    '((get-argn 1)
      (push)
      (get-argn 2)
      (set-argn 1)
      (pop)
      (neq)
      (return))))
