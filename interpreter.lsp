(defun boolp (e)
  (if (or (eq e t)
          (eq e nil))
    t nil))

(defun symbol? (e)
  (if (boolp e)
    nil
    (symbolp e)))

(defun evaluate (e env)
  (if (atom e)
    (cond ((symbol? e) (lookup e env))
          ((or (numberp e) (stringp e) (boolp e)
               (characterp e))
           e)
          (t (error (format nil "Cannot evaluate: ~A" e))))
    (case (car e)
      ((quote) (cadr e))
      ((if) (if (evaluate (cadr e) env)
              (evaluate (caddr e) env)
              (evaluate (cadddr e) env)))
      ((begin) (eprogn (cdr e) env))
      ((set!) (update! (cadr e) env (evaluate (caddr e) env)))
      ((lambda) (make-function (cadr e) (cddr e) env))
      (t (invoke (evaluate (car e) env)
                 (evlis (cdr e) env))))))

(defun eprogn (exps env)
  (if (consp exps)
    (if (consp (cdr exps))
      (progn (evaluate (car exps) env)
             (eprogn (cdr exps) env))
      (evaluate (car exps) env))
    '()))

(defun evlis (exps env)
  (if (consp exps)
    (cons (evaluate (car exps) env)
          (evlis (cdr exps) env))
    '()))

(defun lookup (id env)
  (if (car env)
    (multiple-value-bind (val status)
      (gethash id (car env))
      (if status
        val
        (lookup id (cdr env))))
    (error (format nil "No such binding: ~A" id))))

(defun update! (id env value)
  (if (car env)
    (multiple-value-bind (val status)
      (gethash id (car env))
      (if status
        (progn (setf (gethash id (car env)) value)
               value)
        (update! id (cdr env) value)))
    (error (format nil "No such binding: ~A" id))))

(defun extend (env variables values)
  (let ((new-env (make-hash-table)))
    (if (not (= (length variables)
                (length values)))
      (error "Number of variables and values unmatched"))
    (do ((vars variables (cdr vars))
         (vals values (cdr vals)))
      ((null vars))
      (setf (gethash (car vars) new-env) (car vals)))
    (cons new-env env)))

(defun invoke (fn args)
  (if (functionp fn)
    (funcall fn args)
    (error (format nil "Not a function: ~A" fn))))

(defun make-function (variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))

(defparameter env.init (list (make-hash-table)))

(defmacro bind-mother-lang (name arity lmd)
  `(progn
     (setf (gethash ,name (car env.init)) nil)
     (update! ,name env.init
              (lambda (values)
                (if (= ,arity (length values))
                  (apply ,lmd values)
                  (error (format nil "Incorrect arity:~A"
                                 (list ,name values))))))))

(defmacro bind-mother-lang-r (name lmd)
  `(progn
     (setf (gethash ,name (car env.init)) nil)
     (update! ,name env.init ,lmd)))

(bind-mother-lang '+ 2 (lambda (a b) (+ a b)))
(bind-mother-lang '- 2 #'-)
(bind-mother-lang '* 2 (lambda (a b) (* a b)))
(bind-mother-lang 'display 1 (lambda (a) (princ a)))
(bind-mother-lang 'car 1 #'car)
(bind-mother-lang 'cdr 1 #'cdr)
(bind-mother-lang-r 'list (lambda (values) values))
(bind-mother-lang '= 2 #'=)
