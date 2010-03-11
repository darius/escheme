;;; Environments

(defvar escm-unbound (make-symbol "#<unbound>"))

(defun escm-env-make ()
  (make-hash-table :test 'eq :weakness 'key))

(defun escm-env-extend (env vars vals)
  (list* vars vals env))

(defun escm-env-extend-recursively (env vars exps)
  (let* ((vals (mapcar (lambda (_) escm-unbound) vars))
         (result (escm-env-extend env vars vals)))
    (do ((vals vals (cdr vals))
         (exps exps (cdr exps)))
        ((null vals) result)
      (setcar vals (escm-ev (car exps) result)))))

(defun escm-env-get (env var)
  (if (hash-table-p env)
      (let ((val (gethash var env escm-unbound)))
        (if (eq val escm-unbound)
            (cond ((boundp var) (symbol-value var))
                  ((fboundp var) (symbol-function var))
                  (t (error "Unbound variable: %s" var)))
          val))
    (destructuring-bind (vars vals . parent-env) env
      (let ((i (escm-list-index vars var)))
        (if i (nth i vals) (escm-env-get parent-env var))))))

(defun escm-env-set (env var val)
  (if (hash-table-p env)
      (puthash var val env)
    (destructuring-bind (vars vals . parent-env) env
      (let ((i (escm-list-index vars var)))
        (if i
            (setcar (escm-nthcdr i vals) val)
          (escm-env-set parent-env var val))))))

(defun escm-list-index (xs key)
  (let ((i 0))
    (dolist (x xs nil)
      (when (eq x key)
        (return i))
      (setq i (1+ i)))))

(defun escm-nthcdr (n xs)
  (do ((i 0 (1+ i))
       (xs xs (cdr xs)))
      ((= i n) xs)))

(defun escm-define (env var val)
  (puthash var val env))

(defvar escm-root-env (escm-env-make))

;;; Evaluation

(defun escm-eval (exp &optional env)
  (escm-ev (escm-expand exp)
           (or env escm-root-env)))

(defvar escm-macroexpanders (escm-env-make))

(defun escm-install-macro (name expander)
  (escm-env-set escm-macroexpanders name expander))

(defun escm-macro-get (name)
  (gethash name escm-macroexpanders))

(defmacro def-escm-macro (name parameters &rest body)
  `(escm-install-macro ',name (list* 'lambda ',parameters ',body)))

(defun escm-expand (exp)
  (cond
   ((symbolp exp) exp)
   ((atom exp) `',exp)
   ((consp exp)
    (if (escm-macro-get (car exp))
        (escm-expand (apply (escm-macro-get (car exp)) (cdr exp)))
      (case (car exp)
        ((quote)
         (destructuring-bind (_ datum) exp
           exp))
        ((lambda)
         (destructuring-bind (_ vars . exps) exp
           `(lambda ,vars ,(escm-expand-exps exps))))
        ((setq)
         (destructuring-bind (_ var exp1) exp
           `(setq ,var ,(escm-expand exp1))))
        ((begin)
         (escm-expand-exps (cdr exp)))
        ((if)
         (when (not (memq (length exp) '(3 4)))
           (error "Bad syntax" exp))
         `(if ,(escm-expand (cadr exp))
              ,(escm-expand (caddr exp))
            ,(escm-expand (cadddr exp))))
        ((define)
         (if (symbolp (cadr exp))
             (destructuring-bind (_ var exp1) exp
               `(define ,var ,(escm-expand exp1)))
           (destructuring-bind (_ (var . vars) . exps) exp
             `(define ,var (lambda ,vars . ,exps)))))
        ((letrec)
         (destructuring-bind (_ pairs . exps) exp
           `(letrec ,(mapcar (lambda (pair)
                               (destructuring-bind (var init) pair
                                 `(,var ,(escm-expand init))))
                             pairs)
              ,(escm-expand-exps exps))))
        (t (mapcar 'escm-expand exp)))))
    (t (error "Unknown expression type" exp))))

(defun escm-expand-exps (exps)
  (escm-expand-begin (mapcar 'escm-expand exps)))

(defun escm-expand-begin (exps)
  (cond ((null exps) nil)
        ((null (cdr exps)) (car exps))
        (t `((lambda (first rest) (rest))
             ,(car exps)
             (lambda () ,(escm-expand-begin (cdr exps)))))))

(defvar escm-tag:more (make-symbol "#<tag:more>"))

(defun escm-ev (exp env)
  (let ((value escm-tag:more))
    (while (eq value escm-tag:more)
      (if (symbolp exp)
          (setq value (escm-env-get env exp))
        (case (car exp)
          ((quote)
           (setq value (cadr exp)))
          ((lambda)
           (setq value (escm-enclose exp env)))
          ((setq)
           (setq value (escm-env-set env
                                     (cadr exp)
                                     (escm-ev (caddr exp) env))))
          ((if)
           (setq exp (if (escm-ev (cadr exp) env)
                         (caddr exp)
                       (cadddr exp))))
          ((define)
           (setq value (escm-define env
                                    (cadr exp)
                                    (escm-ev (caddr exp) env))))
          ((letrec)
           (setq env (escm-env-extend-recursively
                      env
                      (mapcar 'car (cadr exp))
                      (mapcar 'cadr (cadr exp)))
                 exp (caddr exp)))
          (t (let ((proc (escm-ev (car exp) env))
                   (args (escm-evlis (cdr exp) env)))
               ;; (apply proc args) coded inline here for the sake of
               ;; proper tail recursion:
               (if (escm-procedure-p proc)
                   (destructuring-bind
                       (_ (_ _)
                          (_ (_ (new-env vars body)) _))
                       proc
                     (setq exp body
                           env (escm-env-extend new-env vars args)))
                 (setq value (apply proc args))))))))
    value))

(defvar escm-params-symbol (make-symbol "#<escm-params>"))

(defun escm-enclose (lambda-exp env)
  "Return a new closure"
  `(lambda (&rest ,escm-params-symbol)
     (escm-eval-closure ',(cons env (cdr lambda-exp))
                        ,escm-params-symbol)))

(defun escm-eval-closure (data arguments)
  (destructuring-bind (env vars body) data
    (escm-ev body 
             (escm-env-extend env vars arguments))))

(defun escm-procedure-p (x)
  (and (consp x)
       (eq (cadadr x) escm-params-symbol)))

(defun escm-evlis (exps env)
  (if (null exps)
      '()
    (cons (escm-ev (car exps) env)
          (escm-evlis (cdr exps) env))))

;;; Test suite

(defun escm-expect (expected exp &rest bindings)
  "Assert that evaluating EXP yields EXPECTED."
  (let ((env (escm-env-extend escm-root-env
                              (mapcar 'car bindings)
                              (mapcar 'cadr bindings))))
    (let ((result (escm-eval exp env)))
      (unless (equal result expected)
        (error "Eval: %S => %S, expected %S" exp result expected)))))

(escm-expect 42         '42)
(escm-expect "yay"      '"yay")
(escm-expect t 		't)
(escm-expect 32 	'x '(x 32))
(escm-expect '(a b)     ''(a b))
(assert (escm-procedure-p (escm-eval '(lambda (x) x))))
(escm-expect 5          '(+ 2 3))
(escm-expect -12        '(* (+ 1 2) (- 4)))
(escm-expect 'no        '(if (= 2 3) 'yes 'no))
(escm-expect 'yes       '(if (= 2 2) 'yes 'no))
(escm-expect 15         '(((lambda (m) (lambda (n) (- m n)))
                           20)
                          5))
(escm-expect 8          '(letrec ((fib
                                   (lambda (n)
                                     (if (< n 2)
                                         1
                                         (+ (fib (- n 1)) (fib (- n 2)))))))
                           (fib 5)))

(escm-expect 12         '((if nil + *) 3 4))

(escm-expect '(10 9)    '((lambda (x)
                            (list x
                                  (begin
                                    (setq x (- x 1))
                                    x)))
                          (+ 3 7)))

(escm-expect '(1 2 1 3 2) '(letrec ((make-counter
                                     (lambda ()
                                       (letrec ((n 0))
                                         (lambda ()
                                           (setq n (+ n 1))
                                           n)))))
                             (letrec ((g (make-counter))
                                      (h (make-counter)))
                               (list (g) (g) (h) (g) (h)))))

(escm-expect nil        '(letrec ((f (lambda () (if (eq f g) 'f 'both)))
                                  (g (lambda () (if (eq f g) 'g 'both))))
                           (eq f g)))

(provide 'escm-kernel)
