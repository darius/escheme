;;; From http://ip9.org/munro/elisp/wok-match.el
;;; Hacked by Darius Bacon to support qualified matches
;;; with syntax: (: pattern predicate-symbol)

;;; Wok Match library 0.1
;;; (c) 2008 Thomas Munro <munro@ip9.org>
;;; 
;;;
;;; This is my cheap immitation of real pattern matching systems like 
;;; match-s48, Erlang, Haskell etc.  A real pattern matching macro would
;;; expand to efficient inline decision code, but this isn't one of those
;;; systems, it just tests each pattern sequentially.
;;;
;;; Usage:
;;;
;;; (wok-match <object>
;;;   (<pattern>
;;;    <body> ...)
;;;   (<pattern>
;;;    <body> ...)
;;;   ...)
;;;
;;; Patterns are composed of the following objects:
;;;
;;; * the symbol _ matches and ignores any object
;;; * quoted symbols match equal symbols
;;; * strings match equal strings
;;; * numbers match equal numbers
;;; * lists match lists of the same size, if their elements match
;;; * improper lists allow the tail of a list to be matched 
;;; * unquoted symbols match any object and are bound to the value in the body
;;;
;;; TODO - support equality testing, when a name is used more than once?

(defun wok-match-quoted-symbol-p (object)
  "Tests is an object is a quoted symbol."
  (and (consp object)
       (eq (car object) 'quote)
       (consp (cdr object))
       (symbolp (cadr object))))

(defun wok-match-test (pattern candidate)
  "Tests if an object consisting of nested lists and atoms matches
   a pattern."
  (cond ((null pattern)
         (null candidate))
        ((wok-match-quoted-symbol-p pattern)
         (eq (cadr pattern) candidate))
        ((consp pattern)
         (if (eq (car pattern) ':)
             (and (wok-match-test (cadr pattern) candidate)
                  (funcall (caddr pattern) candidate))
           (and (consp candidate)
                (wok-match-test (car pattern) (car candidate))
                (wok-match-test (cdr pattern) (cdr candidate)))))
        ((symbolp pattern) t)
        ((stringp pattern)
         (and (stringp candidate)
              (string= pattern candidate)))
        ((numberp pattern)
         (and (numberp candidate)
              (= pattern candidate)))
        (t nil)))

(assert (wok-match-test 42 42))
(assert (wok-match-test "hello" "hello"))
(assert (wok-match-test ''hello 'hello))
(assert (wok-match-test 'hello 'goodbye)) 
(assert (wok-match-test '(a b) '(a b)))
(assert (wok-match-test '(_ _) '(a b)))
(assert (wok-match-test '() '()))
(assert (wok-match-test '(_ _ (_ _)) '(a b (c d))))
(assert (wok-match-test '(_ _ _) '(a b (c d))))
(assert (wok-match-test '(_ _ x) '(a b (c d))))
(assert (wok-match-test '(_ . _) '(a b (c d))))
(assert (wok-match-test '(_ _ . _) '(a b (c d))))
(assert (wok-match-test '(_ _ _ . _) '(a b (c d))))
(assert (not (wok-match-test 42 999)))
(assert (not (wok-match-test "hello" "goodbye")))
(assert (not (wok-match-test ''hello 'goodbye)))
(assert (not (wok-match-test '(a b c) '(a b))))
(assert (not (wok-match-test '(a) '(a b))))
(assert (not (wok-match-test '(_ _ (_ _)) '(a b (c)))))
(assert (not (wok-match-test '(_ _ _ _ . _) '(a b (c d)))))

(defun wok-match-make-path (pattern name path)
  "Given a nested pattern and a name, returns a path expression that can be 
   used to access that element of the nested structure.  For example, the 
   pattern (a (b 42 c)) produces the following results for each name:
     a -> (car object) 
     b -> (car (car (cdr object))) 
     c -> (car (car (cdr (cdr (cdr (car (cdr object)))))))."
  (cond ((null pattern) nil)
        ((eq pattern name) path)
        ((wok-match-quoted-symbol-p pattern) nil)
        ((consp pattern)
         (if (eq (car pattern) ':)
             (wok-match-make-path (cadr pattern) name path)
           (let ((first (wok-match-make-path (car pattern) 
                                             name 
                                             (list 'car path))))
             (if first
                 first
               (wok-match-make-path (cdr pattern) 
                                    name
                                    (list 'cdr path))))))
        (t nil)))

(assert (null (wok-match-make-path '(a (b 42 c)) 'z 'object)))
(assert (equal (wok-match-make-path '(a (b 42 c)) 'a 'object) '(car object)))
(assert (equal (wok-match-make-path '(a (b 42 c)) 'b 'object)
               '(car (car (cdr object)))))
(assert (equal (wok-match-make-path '(a b (c d)) 'd 'object)
               '(car (cdr (car (cdr (cdr object)))))))
(assert (equal (wok-match-make-path '(a . b) 'b 'object)
               '(cdr object)))
(assert (equal (wok-match-make-path '(a b . c) 'c 'object)
               '(cdr (cdr object))))

(defun wok-match-find-names (pattern)
  "Returns a list of capturing names (unquoted symbols excluding _) found in 
   a nested pattern structure."
  (cond ((null pattern) 
         nil)
        ((wok-match-quoted-symbol-p pattern)
         nil)
        ((consp pattern) 
         (if (eq (car pattern) ':)
             (wok-match-find-names (cadr pattern))
           (append (wok-match-find-names (car pattern))
                   (wok-match-find-names (cdr pattern)))))
        ((symbolp pattern)
         (if (eq pattern '_)
             nil
           (list pattern)))
        (t nil)))

(assert (equal (wok-match-find-names '(a b (c d (e f)))) '(a b c d e f)))
(assert (equal (wok-match-find-names '(a _ "hello" 'x 42 b)) '(a b)))
(assert (equal (wok-match-find-names '(a b (c d (e . f)))) '(a b c d e f)))
(assert (equal (wok-match-find-names '(a b (c d . (e . f)))) '(a b c d e f)))

(defmacro wok-match* (object &rest clauses)
  "The guts of the recursive pattern matcher.  Use via wok-match if you don't
   want <object> to be evaluated repeatedly."
  (cond ((null clauses) 'nil)
        ((consp clauses)
         (let ((head (car clauses)))
           (if (consp head)
               (let ((pattern (car head))
                     (body (cdr head)))
                 `(if (wok-match-test (quote ,pattern) ,object)
                      (let ,(mapcar 
                             (lambda (name)
                               (list name (wok-match-make-path pattern 
                                                               name 
                                                               object)))
                             (wok-match-find-names pattern))
                        ,@body)
                    (wok-match* ,object ,@(cdr clauses))))
             (error "bad wok-match clause, expected (pattern body ...)"))))
        (t (error "wok-match expected at least one clause"))))

(defmacro wok-match (object &rest clauses)
  "Evaluates the body associated with the first pattern matching <object>.
   Clauses are of the form (<pattern> <body> ...).  Object is evaluated once."
  (let ((name (gensym)))
    `(let ((,name ,object))
       (wok-match* ,name ,@clauses))))

(assert (equal (wok-match '(hello foo)
                 (('hello name)
                  (format "hi there %s" name))
                 (_ "non capisco"))
               "hi there foo"))
(assert (null (wok-match '(zarp "scott" "tiger")
                (('logoff)
                 (format "goodbye"))
                (('logon username password)
                 (format "hi there %s" username)))))
(assert (equal (wok-match '(a b c)
                 ((x . xs) (format "head = %s tail = %s" x xs)))
               "head = a tail = (b c)"))

(provide 'wok-match)
