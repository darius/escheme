(defvar escm-unbound (make-symbol "#<unbound>"))
(defvar escm-tag:procedure (make-symbol "#<tag:procedure>"))

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
                  (t (error "Unbound variable" var)))
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
   ((atom exp) exp)
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

(defun escm-ev (exp env)
  (cond ((symbolp exp) (escm-env-get env exp))
        ((atom exp) exp)
        ((consp exp)
         (case (car exp)
           (quote (cadr exp))
           (lambda (list* escm-tag:procedure (cdr exp) env))
           (setq (escm-env-set env
                               (cadr exp)
                               (escm-ev (caddr exp) env)))
           (if (if (escm-ev (cadr exp) env)
                   (escm-ev (caddr exp) env)
                 (escm-ev (cadddr exp) env)))
           (define (escm-define env
                                (cadr exp)
                                (escm-ev (caddr exp) env)))
           (letrec (escm-ev (caddr exp)
                              (escm-env-extend-recursively
                               env
                               (mapcar 'car (cadr exp))
                               (mapcar 'cadr (cadr exp)))))
           (t (let ((proc (escm-ev (car exp) env))
                    (args (escm-evlis (cdr exp) env)))
                ;; (escm-apply proc args) coded inline here for the sake
                ;; of proper tail recursion:
                (cond ((functionp proc) (apply proc args))
                      ((consp proc)
                       (cond ((eq escm-tag:procedure (car proc))
                              (destructuring-bind ((vars exp) . env) (cdr proc)
                                (escm-ev exp (escm-env-extend env vars args))))
                             (t (error "Unknown procedure type" proc))))
                      (t (error "Unknown procedure type" proc)))))))
        (t (error "Unknown expression type" exp))))

(defun escm-evlis (exps env)
  (if (null exps)
      '()
    (cons (escm-ev (car exps) env)
          (escm-evlis (cdr exps) env))))

(defun escm-apply (proc args)
  (escm-ev (mapcar (lambda (x) (list 'quote x)) (cons proc args))
           nil))

(provide 'escm)
