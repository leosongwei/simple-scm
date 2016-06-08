(load "main.lisp")

(defun print-test (r)
  (format t "result:~A~%" r))

(print-test (tleval
              '(begin
                 (set! a 10)
                 (set! b 15)
                 (+ a b))))

(print-test (tleval
              '(begin
                 (set! f (lambda (a b)
                           (+ a b)))
                 (f 200 33))))

(print-test (tleval
              '(begin
                 (set! fib (lambda (x)
                             (if (= 0 x)
                               0
                               (if (= 1 x)
                                 1
                                 (+ (fib (- x 1))
                                    (fib (- x 2)))))))
                 (fib 7))))
; 0, 1, 1, 2, 3, 5, 8, 13, 21

(print-test (tleval
              '((lambda (f a b)
                  (f f a b))
                (lambda (f a b)
                  (if (= 0 a)
                    b
                    (f f
                       (- a 1)
                       (+ b 1))))
                10000 10000)))

(sb-ext:exit)
