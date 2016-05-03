;;;; body-array
;; a: array
;; i: index and length
(defstruct ba
  a i)

(defun gen-ba ()
  (make-ba :a (make-array
	       10
	       :initial-element 0
	       :adjustable t)
	   :i 0))
			  
(defun add-to-ba (ba e)
  (let ((length (array-dimension (ba-a ba) 0)))
    (if (> (/ (ba-i ba) length) 0.9)
	(adjust-array (ba-a ba) (floor (* 1.6 length)))
	nil)
    (setf (aref (ba-a ba) (ba-i ba)) e)
    (incf (ba-i ba)))
  ba)

(defun ba2list (ba)
  (let ((r nil))
    (dotimes (i (ba-i ba))
      (push (aref (ba-a ba) i) r))
    (reverse r)))

(defun gen-code (func)
  "GENERATE-CODE
   return start of function addr on *HEAP*"
  (let* ((ba        (gen-ba))
	 (ass       nil)
	 (ass-len   nil)
	 (body      (func-body func))
	 (total-len nil)
	 (bin nil)
	 (addr nil))
    (linearlize ba body)
    (add-to-ba ba '(return))
    (setf ass (ba2list ba))
    (setf ass-len (length ass))
    (setf total-len (+ 5 (func-closure-length func)
		       (* 4 ass-len)))
    (setf addr (alloc-heap total-len))
    (setf bin
	  (build-function (func-arity func)
			  (func-closure-length func)
			  (func-closure-map func)
			  ass))
    (dotimes (i total-len)
      (setf (aref *heap* (+ i addr))
	    (aref bin i)))
    addr))    

(defun linearlize (ba e)
  (if (atom e)
      (cond ((ref-p e)
	     (let*  ((v (ref-vari e)))
	       (case (vari-type v)
		 ((stack)
		  (add-to-ba ba (list 'get-argn (1+ (vari-n v)))))
		 ((closure)
		  (add-to-ba ba
			     (if (= 0 (ref-level e))
				 (list 'get-closure-local
				       (vari-n v))
				 (list 'get-closure
				       (ref-level e)
				       (vari-n v)))))
		 ((global)
		  (progn
		    (add-to-ba ba (list 'constant 'symbol
					(vari-name v)))
		    (add-to-ba ba (list 'get-global)))))))
	    ((func-p e)
	     (let ((start (gen-code e)))
	       (add-to-ba
		ba
		(list 'fix-closure start)))))
      (case (car e)
	((constant)
	 (let ((value (cadr e)))
	   (cond ((symbolp value)
		  (add-to-ba
		   ba (list 'constant 'symbol value)))
		 ((numberp value)
		  (add-to-ba
		   ba (list 'constant 'integer value))))))
	((if)
	 (let ((flag (gensym)))
	   (linearlize ba (cadr e))
	   (add-to-ba ba (list 'jmpt flag))
	   (linearlize ba (cadddr e))
	   (add-to-ba ba flag) ;; true flag
	   (linearlize ba (caddr e))))
	((call)
	 (let ((arity (length (nth 2 e)))
	       (args (nth 2 e))
	       (f (nth 1 e)))
	   (linearlize ba f)
	   (add-to-ba ba '(push))
	   (dolist (i args)
	     (linearlize ba i)
	     (add-to-ba ba '(push)))
	   (dotimes (i arity)
	     (add-to-ba ba '(pop))
	     (add-to-ba ba (list 'set-argn (- arity i))))
	   (add-to-ba ba '(pop))
	   (add-to-ba ba (list 'set-ari arity))
	   (add-to-ba ba '(call)))))))

	  
