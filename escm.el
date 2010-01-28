(defvar escm-unspecified-symbol (make-symbol "#<unspecified>"))
(defvar escm-unbound-symbol (make-symbol "#<unbound>"))
(defvar escm-tag:procedure (make-symbol "#<tag:procedure>"))

(defun escm-env-make ()
  (make-hash-table :test 'eq :weakness 'key))

(defun escm-env-extend (env vars vals)
  (list* vars vals env))

(defun escm-env-extend-recursively (env vars exps)
  (let* ((vals (mapcar (lambda (_) escm-unbound-symbol) vars))
         (result (escm-env-extend env vars vals)))
    (do ((vals vals (cdr vals))
         (exps exps (cdr exps)))
        ((null vals) result)
      (setcar vals (escm-eval (car exps) result)))))

(defun escm-env-get (env var)
  (if (hash-table-p env)
      (let ((val (gethash var env escm-unbound-symbol)))
        (if (eq val escm-unbound-symbol)
            (if (fboundp var)
                var 
              (error "Unbound variable" var))
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

(escm-define escm-root-env 't 't)
(escm-define escm-root-env 'nil 'nil)

;; XXX need syntax checking, parameters/arguments checking, etc.
;; XXX tail calls
;; XXX raise an error on unbound var ref/set

(defun escm-eval (exp env)
  (cond ((symbolp exp) (escm-env-get env exp))
        ((atom exp) exp)
        ((consp exp)
         (case (car exp)
           (quote (cadr exp))            
           (lambda (list* escm-tag:procedure (cdr exp) env))
           (setq (escm-env-set env
                               (cadr exp)
                               (escm-eval (caddr exp) env)))
           (if (if (escm-eval (cadr exp) env)
                   (escm-eval (caddr exp) env)
                 (escm-eval (cadddr exp) env)))
           (define (escm-define env
                                (cadr exp)
                                (escm-eval (caddr exp) env)))
           (letrec (escm-eval (caddr exp)
                              (escm-env-extend-recursively
                               env
                               (mapcar 'car (cadr exp))
                               (mapcar 'cadr (cadr exp)))))
           (t (escm-apply (escm-eval (car exp) env)
                          (escm-evlis (cdr exp) env)))))
        (t (error "Unknown expression type" exp))))

(defun escm-evlis (exps env)
  (if (null exps)
      '()
    (cons (escm-eval (car exps) env)
          (escm-evlis (cdr exps) env))))

(defun escm-apply (proc args)
  (cond ((symbolp proc) (apply proc args))
        ((consp proc)
         (cond ((eq escm-tag:procedure (car proc))
                (destructuring-bind ((vars exp) . env) (cdr proc)
                  (escm-eval exp (escm-env-extend env vars args))))
               (t (error "Unknown procedure type" proc))))
        (t (error "unimplemented"))))

(provide 'escm)
