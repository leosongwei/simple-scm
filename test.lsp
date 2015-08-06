
(evaluate
  '(if nil 1 2)
  env.init)

(evaluate
  '((lambda (a)
      ((lambda (b)
         (list a b))
       (+ 2 a)))
    1)
  env.init)

(evaluate
  '(((lambda (a)
       (lambda (b)
         (list a b)))
     1)
    2)
  env.init)

(evaluate
  '(begin (display (= 1 0))
          (display (= 1 1)))
  env.init)

(time
  (evaluate
    '((lambda (c a b)
        (c c a b))
      (lambda (c a b)
        (if (= a 0)
          b
          (c c (- a 1) (+ b 1))))
      10000 10000)
    env.init))
