(defun tleval (sexp)
  (let* ((mess `(lambda () ,sexp))
	 (begin (gen-code
		 (compiler mess nil)))
	 (loader `((fix-closure ,begin)
		   (set-func)
		   (set-ari 0)
		   (call)
		   (halt))))
    (hard-code *heap* 500 (assembly-list loader))
    (run-vm 500)))

