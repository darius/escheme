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


'(def-escm-syntax do
  )
