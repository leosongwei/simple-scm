(defun reloading ()
  (progn
    (load "vm-def.lisp")
    (load "vm.lisp")
    (load "assembler.lisp")
    (load "compiler.lisp")
    (load "internal_functions.lisp")
    (load "linearlizer.lisp")
    ))

(reloading)
