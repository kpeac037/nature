;;;; Pattern Matching Utility
;;;; Originally written in chapter 5 of Peter Norvig's "Paradigms of Artificial
;;;; Intellgence Programming". Adapted for mathematical utilities by me.
;;;;
;;;; I'll make it its own project if I end up extending it enough.

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

  
(defun rule-assoc (rule list)
  (first
   (loop for i in list
      when (equal rule (car i))
      collect i)))


(defun get-binding (var bindings)
  (rule-assoc var bindings))

(defun binding-val (binding &key (quoted nil))
  (let ((result (cdr binding)))
    (if (consp result)
        (if (eq quoted t)
            `(quote ,(car result))
            (car result))
        result)))

(defun lookup (var binding)
  (binding-val (get-binding var binding)))

(defun extend-bindings (var val bindings)
  (let ((val (if (consp val) (list val) val)))
    (cons (cons var val)
          (if (eq bindings no-bindings)
              nil
              bindings))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun expression-p (expr)
  (and (symbolp expr) (equal (char (symbol-name expr) 0) #\-)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond
      ((not binding) (extend-bindings var input bindings))
      ((equal input (binding-val binding)) bindings)
      (t fail))))

(defun match-expression (expr input bindings)
  (let ((binding (get-binding expr bindings)))
    (cond
      ((not binding) (extend-bindings expr input bindings))
      ((equal input (binding-val binding)) bindings)
      (t fail))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond
    ((eq bindings fail) fail)
    ((variable-p pattern) (match-variable pattern input bindings))
    ((expression-p pattern) (match-expression pattern input bindings))
    ((eql pattern input) bindings)
    ((and (consp pattern) (consp input)) ; Don't forget to extend this to expressions first
     (pat-match (rest pattern) (rest input)
                (pat-match (first pattern) (first input)
                           bindings)))
    (t fail)))

(defun test ()
  (let ((expr '(* x x)))
    (when-match expr '(* ?x ?x) '(setf expr (format nil "Yeah it works. ~a" ?x)))))



;;; This disgusting system of macros allows us to refer to variables after defining them at runtime.
;;; I hate myself for writing any of these.
(defmacro when-match (expr vars &rest eval)
  `(let ,(mapcar #'(lambda (x) `(,(car x) ,(binding-val x :quoted t))) (pat-match vars expr))
     (eval ,@eval)))
