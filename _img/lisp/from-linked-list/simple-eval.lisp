(defpackage :simple-eval
  (:use :cl))

(in-package :simple-eval)

(defun simple-eval (expr binding)
  "Simple EVAL function."
  (cond ((atom expr)              (if (numberp expr)
                                      expr
                                      (cdr (assoc expr  binding))))
        ((atom (car expr))        (eval-rule   (car expr)
                                               (cdr expr)
                                               binding))
        ((eq (caar expr) 'lambda) (eval-lambda (car expr)
                                               (mapcar (simple-eval-at binding)
                                                       (cdr expr))
                                               binding))
        ((eq (caar expr) 'label)  (eval-label   (car expr)
                                                (mapcar (simple-eval-at binding)
                                                        (cdr expr))
                                                binding))))

(defun simple-eval-at (binding)
  "Helper function for making SIMPLE-EVAL function."
  (lambda (expr) (simple-eval expr binding)))

(defun eval-rule (sym args binding)
  (cond ((eq sym 'quote) (first args))
        ((eq sym 'atom)  (atom (simple-eval (first args) binding)))
        ((eq sym 'eq)    (eq (simple-eval (first args)  binding)
                             (simple-eval (second args) binding)))
        ((eq sym 'cond)  (eval-cond args binding))
        ((eq sym 'cdr)   (cdr (simple-eval (first args) binding)))
        ((eq sym 'cons)  (cons (simple-eval (first args)  binding)
                               (simple-eval (second args) binding)))
        ;; Additional Functions, these are just for easy valiation.
        ((eq sym 'add)   (reduce (lambda (sum inc) (+ sum (simple-eval inc binding)))
                                 args :initial-value 0))
        ((eq sym 'sub)   (reduce (lambda (sum inc) (- sum (simple-eval inc binding)))
                                 (rest args)
                                 :initial-value (simple-eval (first args) binding)))
        ((eq sym 'mul)   (reduce (lambda (sum inc) (* sum (simple-eval inc binding)))
                                 args :initial-value 1))
        ((eq sym 'div)   (reduce (lambda (sum inc) (/ sum (simple-eval inc binding)))
                                 (rest args)
                                 :initial-value (simple-eval (first args) binding)))
        ((eq sym 'less)  (< (simple-eval (first  args) binding)
                            (simple-eval (second args) binding)))
        ;; For those not in matched rules, they shall be found in the binding.
        (T (simple-eval (cons (simple-eval sym binding) args) binding))))

(defun eval-cond (conditions binding)
  "Eval condition test for ((p1 e1) (p2 e2) ...) with BINDING."
  (if (null conditions)
      NIL
      (let ((pairs (first conditions)))
        (if (simple-eval (first pairs) binding)
            (simple-eval (second pairs) binding)
            (eval-cond (rest conditions) binding)))))

(defun eval-lambda (lambda-expr args binding)
  (let ((formal-args (second lambda-expr))
        (body        (third  lambda-expr)))
    (simple-eval body (append (mapcar #'cons formal-args args) binding))))

(defun eval-label (label-expr args binding)
  (let ((sym         (second label-expr))
        (formal-args (third  label-expr))
        (body        (fourth label-expr)))
    (simple-eval body
                 (append (mapcar #'cons formal-args args)
                         (list (cons sym `(lambda ,formal-args ,body)))
                         binding))))
