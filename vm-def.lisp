;;;; definition

;; pre-compiled-function in HEAP
;; 0 total-length
;; 1 arity
;; 2 stack-length
;; 3 closure-length
;; 4 closure-map
;; (4+closure-length) body-length
;; (5+closure-length) body
;; (5 + closure-length + body-length) closure-array-pointer (closure)

(defun func-total-len (array func-addr)
  (aref array func-addr))
(defun func-arity (array func-addr)
  (aref array (+ func-addr 1)))
(defun func-stack-len (array func-addr)
  (aref array (+ func-addr 2)))
(defun func-closure-len (array func-addr)
  (aref array (+ func-addr 3)))
(defun func-closure-map (array func-addr index)
  (aref array (+ func-addr 4 index)))
(defun func-closure-map-addr (func-addr)
  (+ func-addr 4))
(defun func-body-len (array func-addr)
  (aref array (+ func-addr 4 (func-closure-len array
					       func-addr))))
(defun func-body-addr (array func-addr)
  (aref array (+ func-addr 5 (func-closure-len array func-addr))))
(defun closure-array-pointer (array closure-addr)
  (aref array (+ closure-addr 5
		 (func-closure-len array closure-addr)
		 (func-body-len array closure-addr))))

;; closure-array
;; 0 total-length
;; 1 upper-array-pointer
;; 2 value-array
(defun closure-array-len (array closure-array-pointer)
  (aref array closure-array-pointer))
(defun closure-array-upper-pointer (array closure-array-pointer)
  (aref array (+ closure-array-pointer 1)))
(defun closure-array-value (array closure-array-pointer index)
  (aref array (+ closure-array-pointer 2 (* 2 index))))
(defun closure-array-value-addr (closure-array-pointer index)
  (+ closure-array-pointer 2 (* 2 index)))

