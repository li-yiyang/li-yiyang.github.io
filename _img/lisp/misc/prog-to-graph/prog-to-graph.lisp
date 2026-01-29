(defpackage prog-to-graph
  (:use :cl :eazy-graphviz)
  (:import-from :alexandria
                :with-gensyms)
  (:export :prog->graph)
  (:documentation "Make graph of a program."))

(in-package prog-to-graph)

(declare-dot-function draw-assign draw-if draw-env draw-seq draw-code
                      draw-normal-function)

;;; render into the following form:
;;;
;;;     (in-node)
;;;  .______|______(assign)
;;;  |  key ← value |
;;;  |  key ← value |
;;;  +------+-------+
;;;         |
;;;     (out-node)
(defdot draw-assign (key-value-plist assign out-node)
  (node assign
        :label (format nil "~{~a ← ~a~^\\n~}" key-value-plist)
        :shape :rect)
  (arc assign out-node :label ""))

;;; render into the following form:
;;;
;;;     (in-node)
;;;        |
;;;   < condition >
;;;     T/    \F     (if-node)
;;;  (true)  (false)
;;;      \    /
;;;    (out-node)
(defdot draw-if (condition true false if-node out-node)
  (with-gensyms (t-node f-node)
    ;; TODO: in the future sould support more complex condition graph expand
    (node if-node :label (format nil "~a" condition) :shape :diamond)
    ;; arcs to branch
    (arc if-node t-node :label "T")
    (arc if-node f-node :label "F")
    ;; true / false prog
    (draw-seq (list true)  t-node out-node)
    (draw-seq (list false) f-node out-node)))

;;; render into subgraph
;;;  draw-assign -> draw-seq
(defdot draw-env (key-value-alist body env-node out-node)
  (with-gensyms (env-name body-in-node)
    (subgraph
     (env-name :cluster :true :style :dashed)
     ;; 
     (draw-assign (loop for (key value) in key-value-alist
                        collect key collect value)
                  env-node body-in-node)
     ;; subenv prog
     (draw-seq body body-in-node out-node))))

(defdot draw-seq (program seq-node out-node)
  (if (null program)
      (progn
        ;; in-node -- seq-node -- out-node
        ;;   [ ]   --     *    --   [ ]
        (node seq-node :label "" :shape :point)
        ;; finish arc
        (arc seq-node out-node :label ""))
      (let* ((code (car program))
             (endp (= 1 (length program)))
             (out  (if endp out-node (gensym "OUT"))))
        (if (atom code)
            (draw-assign (list "" code) seq-node out)
            ;; (method . args)
            (draw-code code seq-node out))
        (unless endp
          (draw-seq (cdr program) out out-node)))))

(defdot draw-code (code code-node out-node)
  (case (car code)
    ;; (values ... ... ...)
    (values
     (draw-assign (list "" (format nil "~{~a~^ ~}" (cdr code))) code-node out-node))
    ;; (return-from name value)
    (return-from (draw-assign (cdr code) code-node out-node))
    ;; (setf/setq [key value])
    ((setf setq) (draw-assign (cdr code) code-node out-node))
    ;; (if condition true false)
    (if (draw-if (second code) (third code) (fourth code) code-node out-node))
    ;; (let [binding] body)
    ((let let*) (draw-env (second code) (cddr code) code-node out-node))
    ;; (progn body)
    (progn (draw-seq (cdr code) code-node out-node))
    ;; normal code
    (otherwise
     (if (not (and (symbolp (car code)) (macro-function (car code))))
         (draw-normal-function code code-node out-node)))))

(defun static-args (code-form)
  "Return precalculated code and dummy static args in `code-form'."
  (loop for arg in (cdr code-form)
        for dummy = (if (and (listp arg) (not (null arg))) (gensym "ARG") arg)
        if (and (listp arg) (not (null arg)))
          collect (list dummy arg) into pre-code-pair
        collect dummy into dummy-args
        finally (return (values pre-code-pair dummy-args))))

;;; The normal function (func arg) shall be draw like
;;;
;;;         (in-node)  [dummy-arg <- pre-code]
;;;             |      /
;;;    +--------+---------+
;;;    | func(dummy-args) |
;;;    +--------+---------+
;;;             |
;;;         (out-node)
(defdot draw-normal-function (func-code func-node out-node)
  (multiple-value-bind (pre-code-pair dummy-args)
      (static-args func-code)
    (if (null pre-code-pair)
        (node func-node
              :label (format nil "~a(~{~a~^, ~})" (car func-code) dummy-args)
              :shape :rect)
        (subgraph
         ((gensym "SUBGRAPH") :style :dashed :cluster :true)
         (node func-node
               :label (format nil "~a(~{~a~^, ~})" (car func-code) dummy-args)
               :shape :rect)
         (loop for (dummy code) in pre-code-pair
               for index below (length pre-code-pair)
               do (draw-code (cons (format nil "~a ← ~a" dummy (car code))
                                   (cdr code))
                             dummy func-node))))

    (arc func-node out-node :label "")))

(defmacro prog->graph ((output &key (type :svg) (debug nil)) &body body)
  "Trun a program into digraph and plot it out."
  (with-gensyms (reset in out seq-node)
    `(with-dot (,output :debug ,debug :type ,type :diredp t :splines :ortho
                        :nodesep 1.0 :fontname :Arial :forcelabels :true)
       (with-gensyms (,reset ,in ,out ,seq-node)
         (set :node :fontname :Arial :fontcolor :black)
         (set :edge :fontname :monospace :fontcolor :black)
         (node ,reset :label "RESET" :shape :plain)
         (node ,in  :label "" :shape :point)
         (node ,out :label "" :shape :point)
         (arc ,reset ,in :label "")
         (arc ,in ,seq-node :label "")
         (draw-seq ',body ,seq-node ,out)
         (arc ,out ,in :label "" :constraint :false)))))
