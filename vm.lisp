;;;; ================== RUNTIME ======================

(progn
  (defvar *heap* nil)
  (setf *heap* (make-array 10000))
  
  (defvar *pointer-heap* 1)
  (setf *pointer-heap* 1)

  (defvar *stack* nil)
  (setf *stack* (make-array 10000))
  
  (defvar *PC* 0)
  (defvar *PS* 0)
  (defvar *PSB* 0)
  (defvar *PCL* 0)
  (defvar *FUNC* 0)
  (defvar *VAL* (cons 0 0))
  (defvar *ARG1* (cons 0 0))
  (defvar *ARG2* (cons 0 0))
  (defvar *ARG3* (cons 0 0))
  (defvar *ARG4* (cons 0 0))
  (defvar *ARG5* (cons 0 0))
  (defvar *ARGL* (cons 0 0))

  (setf *PC* 0)
  (setf *PS* 0)
  (setf *PSB* 0)
  (setf *PCL* 0)
  (setf *FUNC* 0)
  (setf *VAL* (cons 0 0))
  (setf *ARG1* (cons 0 0))
  (setf *ARG2* (cons 0 0))
  (setf *ARG3* (cons 0 0))
  (setf *ARG4* (cons 0 0))
  (setf *ARG5* (cons 0 0))
  (setf *ARGL* (cons 0 0))
  
  (defvar *ins-table* nil)
  (setf *ins-table* (make-array 200))
  (defvar *ins-table-index* 0)
  (setf *ins-table-index* 0)

  (defvar *type-table* nil)
  (setf *type-table* (make-array 20))
  (defvar *type-table-index* 0)
  (setf *type-table-index* 0)
  )

(progn
  (defvar *symbol-table* nil)
  (defvar *symbol-table-index* 0)
  (setf *symbol-table-index* 0)
  (setf *symbol-table* (make-array 1000 :initial-element 0
				   :adjustable t))

  (defun vm-get-symbol (code)
    (let ((s (aref *symbol-table* code)))
      (if (symbolp s)
	  s
	  (error "Unknown symbol code"))))
  
  (defun vm-find-symbol (sym)
    (let ((result nil))
      (dotimes (i 1000)
	(let ((s (aref *symbol-table* i)))
	  (if (eq s sym)
	      (setf result i)
	      nil)))
      result))

  (defun vm-intern-symbol (sym)
    (if (vm-find-symbol sym)
	nil
	(let ((i *symbol-table-index*))
	  (setf (aref *symbol-table* i) sym)
	  (incf *symbol-table-index*)
	  i)))

  (vm-intern-symbol 't)
  (vm-intern-symbol 'nil))

(defmacro make-type (name)
  `(progn
     (setf (aref *type-table* *type-table-index*) (quote ,name))
     (incf *type-table-index*)))

(progn
  (make-type pre-compiled-function)
  (make-type closure)
  (make-type integer)
  (make-type cons)
  (make-type array)
  (make-type symbol)
  (make-type continuation)
  (make-type macro))

(defun code-to-type (code)
  (let ((type (aref *type-table* code)))
    (if (symbolp type)
	type
	(error "CODE-TO-TYPE: Unknown type"))))

(defun type-to-code (name)
  (let ((result nil))
    (dotimes (i 20)
      (let ((type (aref *type-table* i)))
	(if (eq name type)
	    (progn (setf result i)
		   (return))
	    nil)))
    (if result
	result
	(error "TYPE-TO-CODE: Unknown type"))))

(defun ins-arg (n)
  "INS-ARG
   0 indexed"
  (aref *heap* (+ *PC* 1 n)))

(defstruct instruction
  code
  name
  doc
  lambda)

(defmacro defins (name doc &rest body)
  `(let ((new-instruction
	  (make-instruction :code *ins-table-index*
			    :name (quote ,name)
			    :doc ,doc
			    :lambda (lambda ()
				      ,@body))))
     (setf (aref *ins-table* *ins-table-index*) new-instruction)
     (incf *ins-table-index*)))

(defmacro set-pair-target (A B)
  `(progn
     (setf (car ,A) (car ,B))
     (setf (cdr ,A) (cdr ,B))))

;;;; `register` here can only be
;; PC, PS, PSB, PCL, FUNC.
;; can not be those regesters which stores value.
(defmacro push-single (register)
  `(progn
     (setf (aref *stack* *PS*) ,register)
     (incf *PS*)))
(defmacro pop-single (register)
  `(progn
     (decf *PS*)
     (setf ,register (aref *stack* *PS*))))

(defun get-heap (object shift)
  (aref *heap* (+ object shift)))

(defun alloc-heap (size)
  (let ((start *pointer-heap*))
    (incf *pointer-heap* size)
    start))

(defun code-to-name (code)
  (instruction-name (aref *ins-table* code)))

(defun name-to-code (name)
  (dotimes (i 200)
    (let ((ins (aref *ins-table* i)))
      (if (instruction-p ins)
	  (if (eq (instruction-name ins) name)
	      (return i)
	      nil)
	  (error "NAME-TO-CODE: Can NOT find instruction code.")))))

(defmacro assign-vreg (target type value)
  `(progn (setf (car ,target) (type-to-code ,type))
	  (setf (cdr ,target) ,value)))

(defun print-instruction-set (&optional docp)
  (format t "---- INSTRUCTION SET ----~%")
  (dotimes (i 200)
    (let ((ins (aref *ins-table* i)))
      (if (instruction-p ins)
	  (progn (format t "* ~A, ~A~%"
			 (instruction-code ins)
			 (instruction-name ins))
		 (if docp
		     (format t "~A~%~%" (instruction-doc ins))))
	  (return)))))

(progn
  (setf *ins-table* (make-array 200))
  (setf *ins-table-index* 0)

  (defins HALT "Stop interpreting bytecode."
    'do-nothing)
  
  (defins PUSH ""
    (setf (aref *stack* *PS*) (car *val*))
    (incf *PS*)
    (setf (aref *stack* *PS*) (cdr *val*))
    (incf *PS*))
  
  (defins POP ""
    (decf *PS*)
    (setf (cdr *VAL*) (aref *stack* *PS*))
    (decf *PS*)
    (setf (car *VAL*) (aref *stack* *ps*)))

  (defins SET-FUNC ""
    (setf *func* (cdr *val*)))

  (defins SET-ARGN ""
    (let ((target (ins-arg 0)))
      (cond ((= 1 target)
	     (set-pair-target *ARG1* *VAL*))
	    ((= 2 target)
	     (set-pair-target *ARG2* *VAL*))
	    ((= 3 target)
	     (set-pair-target *ARG3* *VAL*))
	    ((= 4 target)
	     (set-pair-target *ARG4* *VAL*))
	    ((= 5 target)
	     (set-pair-target *ARG5* *VAL*)))))

  (defins CALL "(FUNC ARG1 ARG2 ARG3 ARG4 ARG5 ARGL)"
    (push-single *PS*)
    (push-single *PSB*)
    (push-single *PC*)
    (push-single *PCL*)

    (let ((stack-length (get-heap *FUNC* 1)))
      (setf *PSB* *PS*)
      (setf *PS* (+ *PSB* 1 stack-length))
      
      (let* ((closure-length      (get-heap *FUNC* 2))
	     (closure-space-start 0)
	     (closure-map-start   (+ *FUNC* 3)))
	(setf closure-space-start (alloc-heap (+ closure-length 2)))

	;;save real length to structure
	(setf (aref *heap* closure-space-start) (+ closure-length 2))
	;; save upper closure to structure
	(setf (aref *heap* (+ closure-space-start 1)) *PCL*) 
	;; use new closure
	(setf *PCL* closure-space-start)
	
	(dolist (i closure-length)
	  (setf (aref *heap* (+ closure-space-start 2 i))
		(aref *stack*
		      (+ *PSB* 1
			 (aref *heap* (+ closure-map-start i))))))
	(setf *PC* (+ *FUNC* 4 closure-length)))))

  (defins RETURN
      "Abandon current stack frame, and use after result is in *VAL*."
    (setf *PS* *PSB*)
    (pop-single *PCL*)
    (pop-single *PC*)
    (pop-single *PSB*)
    (pop-single *PS*))

  (defins SET-ARG "ARG1 is address"
    (let ((addr *ARG1*))
      (setf (aref *heap* addr) (car *val*))
      (setf (aref *heap* (1+ addr)) (cdr *val*))))
  (defins GET-ARG ""
    (let ((addr *ARG1*))
      (setf (car *VAL*) (get-heap addr 0))
      (setf (cdr *VAL*) (get-heap addr 1))))
  
  (defins SET-ADDR "SET-ADDR ADDR"
    (let ((addr (ins-arg 0)))
      (setf (aref *heap* addr) (car *val*))
      (setf (aref *heap* (1+ addr)) (cdr *val*))))
  (defins GET-ADDR ""
    (setf (car *val*) (aref *heap* (ins-arg 0)))
    (setf (cdr *val*) (aref *heap* (1+ (ins-arg 0)))))

  (defins CONSTANT "CONSTANT TYPE VALUE"
    (setf (car *val*) (ins-arg 0))
    (setf (cdr *val*) (ins-arg 1)))

  (defins GET-STACK "GET-STACK SHIFT"
    (let ((shift (+ *PSB* 1 (* 2 (ins-arg 0)))))
      (setf (car *val*) (aref *stack* shift))
      (setf (cdr *val*) (aref *stack* (1+ shift)))))

  (defins GET-CLOSURE
      "GET-CLOSURE
       Fake instruction, filled when running FIX-CLOSURE."
    (error "VM Error, running fake instruction: GET-CLOSURE"))

  (defins GET-CLOSURE-LOCAL
      "GET-CLOSURE-LOCAL SHIFT
       get local closure value by PCL[2 + SHIFT]."
    (setf (car *VAL*) (aref *heap* (+ *PCL* 2 (* 2 (ins-arg 0)))))
    (setf (cdr *VAL*) (aref *heap* (+ *PCL* 3 (* 2 (ins-arg 0))))))

  (defins SET-CLOSURE
      "SET-CLOSURE
       Fake instruction, see GET-CLOSURE."
    (error "VM Error, running fake instruction: SET-CLOSURE"))

  (defun get-closure-ref (closure-array-addr level n)
    (if (= level 0)
	(+ closure-array-addr 2 n)
	(get-closure-ref (get-heap closure-array-addr 1)
			 (1- level) n)))
  
  (defins FIX-CLOSURE
      "FIX-CLOSURE FUNCTION
       copy a `function', and render every closure reference into
       address based accessing."
    (let* ((func-addr    (ins-arg 0))
	   (func-len     (get-heap func-addr 0))
	   (closure-len  (get-heap func-addr 2))
	   (body-len     (get-heap func-addr 4))
	   (closure-addr 0))
      (setf closure-addr (alloc-heap func-len))
      (dotimes (i func-len)
	(setf (aref *heap* (+ closure-addr i)) 0) ;; fill with 0
	(setf (aref *heap* (+ closure-addr i))
	      (get-heap func-addr i)))
      (dotimes (i (/ body-len 4))
	(let* ((index    (+ 5 (* i 4)))
	       (ins      (get-heap closure-addr index))
	       (level    (get-heap closure-addr (1+ index)))
	       (n        (get-heap closure-addr (+ 2 index)))
	       (ins-name (code-to-name ins)))
	  (if (or (eq 'SET-CLOSURE ins-name)
		  (eq 'GET-CLOSURE ins-name))
	      (progn
		(dotimes (i 4)
		  (setf (aref *heap* (+ closure-addr index i)) 0))
		(setf (aref *heap* (+ closure-addr index))
		      (name-to-code (if (eq ins-name 'SET-CLOSURE)
					'SET-ADDR
					'GET-ADDR)))
		(setf (aref *heap* (+ closure-addr index 1))
		      (get-closure-ref *PCL* (1- level) n))))))
      (setf (car *val*) (type-to-code 'closure))
      (setf (cdr *val*) closure-addr)))

  (defins ADD1 "VAL + ARG1 -> VAL"
    (if (and (eq 'integer (code-to-type (car *VAL*)))
	     (eq 'integer (code-to-type (car *ARG1*))))
	(setf (cdr *VAL*)
	      (+ (cdr *VAL*) (cdr *ARG1*)))
	(error "TYPE-ERROR")))

  (defins SUB1 "VAL - ARG1 -> VAL"
    (if (and (eq 'integer (code-to-type (car *VAL*)))
	     (eq 'integer (code-to-type (car *ARG1*))))
	(setf (cdr *VAL*)
	      (- (cdr *VAL*) (cdr *ARG1*)))
	(error "TYPE-ERROR")))

  (defins NEQ "if *VAL* == *ARG1*."
    (if (and (eq 'integer (code-to-type (car *VAL*)))
	     (eq 'integer (code-to-type (car *ARG1*))))
	(assign-vreg *VAL* 'symbol
		     (if (= (car *VAL*) (cdr *ARG1*))
			 #.(vm-find-symbol 't)
			 #.(vm-find-symbol 'nil)))))
  
  (defins JMPF
      "JMPF: Jump when get a `nil'."
    (let ((addr (ins-arg 0)))
      (if (eq 'symbol (code-to-type (car *VAL*)))
	  (if (= #.(vm-find-symbol 'nil) (cdr *VAL*))
	      (setf *PC* addr)
	      nil)
	  nil))))
 

