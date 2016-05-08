;;;; =============== COMPILER ===================

;;;; struct: vari
;; name: symbol
;; type: stack, closure, global
(defstruct vari
  name
  type
  n)

(defstruct ref
  vari
  level)

(progn
  (defvar *global-env* nil)
  (setf *global-env* (make-hash-table)))

;;;; env-stack
;; env-stack is a stack made up by lists and vari structs
;; ((vari vari vari) (vari vari) (vari vari vari))
(defun extend-c (env-stack varnames)
  (let ((new-stack nil))
    (dolist (v varnames)
      (push (make-vari :name v
		       :type 'stack
		       :n nil)
	    new-stack))
    (cons (reverse new-stack) env-stack)))

(defun lookup (id env-stack)
  (let ((level 0)
	(vari  nil))
    (block exit
      (dolist (frame env-stack)
	(dolist (v frame)
	  (if (eq (vari-name v) id)
	      (progn (if (= 0 level)
			 nil
			 (setf (vari-type v) 'closure))
		     (setf vari v)
		     (return-from exit))
	      nil))
	(incf level)))
    (if vari
	(make-ref :vari vari :level level)
	(let ((global (gethash id *global-env*)))
	  (if global
	      (make-ref :vari global :level 0)
	      (error (format t "Compiler: id `~A' not found"
			     id)))))))
(defstruct func
  arity
  closure-length
  closure-map
  env-stack
  body)

(defun boolp (e)
  (or (eq e t)
      (eq e nil)))

(defun self-quote? (e)
  (or (numberp e) (boolp e)))


;; supress big warning
(defun compiler (e env-stack) e env-stack)
(defun compile-list (e env-stack) e env-stack)
;; -------------

(defun compile-lambda (e env-stack)
  (let* ((f (make-func))
	 (varnames (cadr e))
	 (body (cddr e))
	 (env-stack (extend-c env-stack varnames))
	 (env-top (car env-stack))
	 (closure-list nil)
	 (compiled-body (cons 'begin
			      (compile-list body env-stack))))
    (setf (func-arity f) (length varnames))
    (setf (func-closure-length f)
	  (let ((c 0))
	    (dolist (v env-top)
	      (if (eq (vari-type v) 'closure)
		  (progn (setf (vari-n v) c)
			 (push c closure-list)
			 (incf c))))
	    c))
    (setf (func-closure-map f)
	  (let ((i 0)
		(c 0)
		(m (make-array (length closure-list))))
	    (dolist (v env-top)
	      (if (eq (vari-type v) 'closure)
		  (progn (setf (aref m c) i)
			 (incf c))
		  nil)
	      (incf i))
	    m))
    (let ((i 0))
      (dolist (v env-top)
	(if (not (eq 'closure (vari-type v)))
	    (setf (vari-n v) i)
	    nil)
	(incf i)))
    (setf (func-env-stack f) env-stack)
    (setf (func-body f) compiled-body)
    f))

(defun compile-list (e env-stack)
  (let ((r nil))
    (dolist (i e)
      (push (compiler i env-stack) r))
    (reverse r)))

(defun compiler (e env-stack)
  (if (atom e)
      (cond ((self-quote? e) (list 'constant e))
	    ((symbolp e) (lookup e env-stack)))
      (case (car e)
	((quote) (list 'constant e))
	((if) (if (not (= (length e) 4))
		  (error (format nil "Compiler: malformed IF ~A" e))
		  (list 'if
			(compiler (cadr e) env-stack)
			(compiler (caddr e) env-stack)
			(compiler (cadddr e) env-stack))))
	((begin) (cons 'begin (compile-list (cdr e) env-stack)))
	((set!) (list 'set!
		      (lookup (cadr e) env-stack)
		      (compiler (caddr e) env-stack)))
	((lambda) (compile-lambda e env-stack))
	(t (list 'call
		 (compiler (car e) env-stack)
		 (compile-list (cdr e) env-stack))))))
