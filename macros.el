(require 'wok-match)
(require 'escm)

;; pattern syntax:
;; _                  wildcard
;; v                  variable
;; (: <p> predicate)  qualified pattern
;; 'x                 constant (equal)
;; (<p1> . <p2>)      compound pattern

(defmacro wok-match-p (pattern subject)
  `(wok-match ,subject
     (,pattern t)
     (_ nil)))

(defmacro def-escm-syntax (name &rest clauses)
  (let ((args (gensym)))
    `(def-escm-macro ,name (&rest ,args)
       (wok-match (cons ',name ,args)
         ,@clauses))))

(def-escm-syntax and
  ((_) t)
  ((_ e) e)
  ((_ e . es) `(if ,e (and ,@es) nil)))

(def-escm-syntax or
  ((_) nil)
  ((_ e) e)
  ((_ e . es) `((lambda (v rest) (if v v (rest)))
                ,e
                (lambda () (or ,@es)))))

(defun escm-let-decls-p (x)
  (and (listp x)
       (every (lambda (decl) (wok-match-p ((: _ symbolp) _) decl))
              x)))

(def-escm-syntax let
  ((_ (: name symbolp) (: decls escm-let-decls-p) . exps)
   `((letrec ((,name (lambda ,(mapcar 'car decls) ,@exps)))
       ,name)
     ,@(mapcar 'cadr decls)))
  ((_ (: decls escm-let-decls-p) . exps)
   `((lambda ,(mapcar 'car decls) ,@exps)
     ,@(mapcar 'cadr decls))))

(def-escm-syntax let*
  ((_ () . exps)
   `(begin ,@exps))
  ((_ (decl . decls)
      . exps)
   `(let (,decl)
      (let* ,decls
        ,@exps))))

(def-escm-syntax cond
  ((_) nil)
  ((_ ('else . exps))
   `(begin ,@exps))
  ((_ ('else . exps) . clauses)
   (error "ELSE clause is not last in COND"))
  ((_ (exp) . clauses)
   `(or ,exp (cond ,@clauses)))
  ((_ (exp '=> receiver) . clauses)
   `((lambda (subject receiver otherwise)
       (if subject (receiver subject) (otherwise)))
     ,exp
     ,receiver
     (lambda () (cond ,@clauses))))
  ((_ (exp . exps) . clauses)
   `(if ,exp
        (begin ,@exps)
        (cond ,@clauses))))

(def-escm-syntax case
  ((_ subject . clauses)
   (let ((var (gensym)))
     `(let ((,var ,subject))
        (__case__ ,var ,@clauses)))))

(def-escm-syntax __case__
  ((_ v ('else . exps)) 
   `(begin ,@exps))
  ((_ v ((: values consp) . exps) . clauses)
   `(cond ((member ,v ',values) ,@exps)
          (else (__case__ ,v ,@clauses))))
  ((_ v (value . exps) . clauses)
   `(cond ((equal ,v ',value) ,@exps)
          (else (__case__ ,v ,@clauses)))))


(def-escm-syntax do
  ((_ var-clauses test-clause . body-exps)
   (dolist (var-clause var-clauses)
     (assert (and (consp var-clause)
                  (symbolp (car var-clause))
                  (and (<= 1 (length var-clause))
                       (<= (length var-clause) 3)))))
   (let ((loop-fn (gensym))
         (variables (mapcar 'car var-clauses))
         (inits (mapcar 'cadr var-clauses))
         (steps (mapcar 'caddr var-clauses))
         (steps (mapcar (lambda (clause)
                          (if (null (cddr clause))
                              (car clause) ;default step leaves var unchanged
                            (caddr clause)))
                        var-clauses)))
     `(letrec ((,loop-fn 
                (lambda ,variables
                  (cond ,test-clause
                        (else
                         ,@body-exps
                         (,loop-fn . ,steps))))))
        (,loop-fn . ,inits)))))


(def-escm-syntax elisp-defuns
  ((_ . names)
   `(for-each fset ',names (list ,@names))))

(defun for-each (fn lst &rest lsts)
  (if (null lsts)                       ; special case for speed
      (mapc fn lst)
    (do ((lsts (cons lst lsts) (mapcar 'cdr lsts)))
        ((null (car lsts)))
      (apply fn (mapcar 'car lsts)))))


;;; Test suite
;;; Derived from Aubrey Jaffer's test.scm

(escm-expect 3          '(let ((reverse-subtract
                                (lambda (x y) (- y x))))
                           (reverse-subtract 7 10)))

(escm-expect 'greater   '(cond ((> 3 2) 'greater)
                               ((< 3 2) 'less)))
(escm-expect 'equal     '(cond ((> 3 3) 'greater)
                               ((< 3 3) 'less)
                               (else 'equal)))
(escm-expect 2          '(cond ((assq 'b '((a 1) (b 2))) => cadr)
                               (else nil)))

(escm-expect 'composite '(case (* 2 3)
                           ((2 3 5 7) 'prime)
                           ((1 4 6 8 9) 'composite)))
(escm-expect 'consonant '(case (car '(c d))
                           ((a e i o u) 'vowel)
                           ((w y) 'semivowel)
                           (else 'consonant)))

(escm-expect t          '(and (= 2 2) (> 2 1)))
(escm-expect nil        '(and (= 2 2) (< 2 1)))
(escm-expect '(f g)     '(and 1 2 'c '(f g)))
(escm-expect t          '(and))

(escm-expect t          '(or (= 2 2) (> 2 1)))
(escm-expect t          '(or (= 2 2) (< 2 1)))
(escm-expect nil        '(or nil nil nil))
(escm-expect nil        '(or))
(escm-expect '(b c)     '(or (memq 'b '(a b c)) (+ 3 0)))

(escm-expect 6          '(let ((x 2) (y 3))
                           (* x y)))
(escm-expect 35         '(let ((x 2) (y 3))
                           (let ((x 7) (z (+ x y)))
                             (* z x))))

(escm-expect 70         '(let ((x 2) (y 3))
                           (let* ((x 7) (z (+ x y)))
                             (* z x))))

(escm-expect t '(letrec ((even?
                          (lambda (n) (if (zerop n) t (odd? (- n 1)))))
                         (odd?
                          (lambda (n) (if (zerop n) nil (even? (- n 1))))))
                  (even? 12)))

(escm-expect '[0 1 2 3 4] '(do ((vec (make-vector 5 nil))
                                (i 0 (+ i 1)))
                               ((= i 5) vec)
                             (aset vec i i)))
