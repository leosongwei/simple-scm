(defun reloading ()
  (progn
    (load "vm.lisp")
    (load "assembly.lisp")
    (load "compiler.lisp")
    (load "internal_functions.lisp")
    (load "linearlizer.lisp")
    ))

