(defpackage eazy-graphviz
  (:use :cl)
  (:export #:with-dot
           #:defdot
           #:declare-dot-function
           #:delete-dot-function
           #:test-dot-function
           #:*dot-namespace*)
  (:import-from :alexandria :with-gensyms)
  (:documentation "This is a wrapper for simple Graphviz usage."))

(in-package eazy-graphviz)

(defun run-dot (input output &key (type :svg) &allow-other-keys)
  "Run dot program.

The `input' and `output' should follow `uiop:run-program' flavor.
The `type' could be `:svg', `:png', and so on, it would be overwritten
if the output is a path name indicating its file type."
  (let ((graph-type (format nil "-T~A" (if (or (pathnamep output)
                                               (stringp output))
                                           (pathname-type output)
                                           type))))
    (uiop:run-program `("dot" ,graph-type)
                      :input input
                      :output (if (stringp output) (pathname output) output))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; `plist-remove-key' is used to avoid naming conflicts in keys
  (defun plist-remove-key (plist &rest keys)
    "Remove `keys' in `plist'."
    (loop for (k v) on plist by #'cddr
          if (not (member k keys))
            collect k and collect v))

  ;; `dot-args' is used to transform common lisp plist into dot args
  (defun dot-args (args)
    "Generate graphviz arg brackets."
    (if (null args) "" (format nil " [~{~a=\"~a\"~^, ~}]" args)))

  ;; `escape-dot-expr' is used to add the missing output dot stream
  ;; mainly for `with-dot' and `defdot' two macros.
  (defparameter *dot-namespace* '()
    "All the dot methods name should be within `*dot-namespace*'.")
  
  (defparameter *dot-alias*
    '((node . %dot-node)
      (arc  . %dot-arc)
      (set  . %dot-set)
      (subgraph . %dot-subgraph))
    "Alias of dot commands")
  
  (defun ->dot-sym (symbol)
    "Make `symbol' under `eazy-graphviz' namespace."
    (intern (symbol-name symbol) :eazy-graphviz))
  
  (defun escape-dot-expr (stream diredp expr)
    "The `expr' should be escaped if the method is within `*dot-namespace*'.
  
    For example:
  
      (method name &rest keys) --> (method stream name &rest keys)
    "
    (if (listp expr)
        (let ((method (car expr))
              (args   (cdr expr)))
          (if (atom method)
              (if (assoc method *dot-namespace*)
                  ;; Expand dot functions
                  `(funcall (cdr (assoc ',method *dot-namespace*))
                            ,stream ,diredp ,@args)
                  (if (assoc (->dot-sym method) *dot-alias*)
                      ;; Replace dot alias and expand the function
                      `(,(cdr (assoc (->dot-sym method) *dot-alias*))
                        ,stream ,diredp ,@args)
                      (cons method (mapcar (lambda (expr)
                                             (escape-dot-expr stream diredp expr))
                                           args))))
              (cons (escape-dot-expr stream diredp method)
                    (mapcar (lambda (expr) (escape-dot-expr stream diredp expr))
                            args))))
        expr))
  )

(defun %dot-node (stream diredp name &rest args &key &allow-other-keys)
  "Make a node with `name'."
  (declare (ignore diredp))
  (format stream "~&\"~a\"~a;" name (dot-args args)))

(defun %dot-arc (stream diredp from to &rest args &key &allow-other-keys)
  "Make a arc `from' and `to', digraph arc if `diredp' otherwise graph arc."
  (format stream "~&\"~a\" ~a \"~a\"~a;"
          from (if diredp "->" "--") to (dot-args args)))

(defun %dot-set (stream diredp slot &rest args &key &allow-other-keys)
  "Set graphviz `slot' properties by `args'."
  (declare (ignore diredp))
  (format stream "~&~a~a;" slot (dot-args args)))

(defmacro %dot-subgraph (stream diredp (name &rest args &key &allow-other-keys)
                         &body body)
  "Expand graphviz subgraph."
  `(progn
     (format ,stream "~&subgraph \"~a\" {" ,name)
     (format ,stream "~{~&~a=~a;~}" (list ,@args))
     (progn ,@(mapcar (lambda (expr) (escape-dot-expr stream diredp expr)) body))
     (format ,stream "~&}")))

;;; declare names of dot function before defining it
(defmacro declare-dot-function (&rest names)
  "Declare `names' are dot function."
  `(progn
     ,@ (mapcar (lambda (name)
                  (if (assoc name *dot-namespace*)
                      ;; warn user of redefining dot function
                      `(unless (null (cdr (assoc ',name *dot-namespace*)))
                         (warn ,(format nil "~a is already a dot function." name)))
                      `(push (cons ',name nil) *dot-namespace*)))
                names)))

;;; delete names/all-names in dot namespace
(defun delete-dot-function (&rest names)
  "Delete `names' of dot function, if `names' is nil, delete all dot functions."
  (if (null names)
      (setf *dot-namespace* '())
      (delete-if (lambda (pair) (member (car pair) names)) *dot-namespace*)))

;;; test function output
(defmacro test-dot-function ((&key (stream *standard-output*) (diredp t)
                              &allow-other-keys)
                             &body body)
  "Test dot function within `body', default output `stream' is `*standard-output*'."
  `(progn ,@ (mapcar (lambda (expr) (escape-dot-expr stream diredp expr)) body)))

(defmacro defdot (dot-name lambda-list &body body)
  "Define a graphviz function."
  (with-gensyms (stream diredp)
    (let ((name dot-name))
      (if (assoc name *dot-alias*)
          ;; Avoid user to define function conflicts with `*dot-alias*'.
          `(warn (format nil "~a conflicts with *dot-alias*." ,name))
          `(progn
             ;; Warn user of redefining dot function
             ;; make sure user won't overwrite the preserved function
             ,(if (member name '(%dot-node %dot-arc %dot-subgraph %dot-set))
                  `(warn ,(format nil "~a is preserved" name))
                  `(progn
                     (declare-dot-function ,name)
                     (setf (cdr (assoc ',name *dot-namespace*))
                           (lambda (,stream ,diredp ,@lambda-list)
                             (declare (ignorable ,stream ,diredp))
                             ,@ (mapcar (lambda (expr)
                                          (escape-dot-expr stream diredp expr))
                                        body)))))
             ',dot-name)))))

(defmacro with-dot ((output &rest keys &key (diredp t) debug &allow-other-keys)
                    &body body)
  "Expand with `run-dot' to draw a graph.

To define a graph, the following command could be used:

  (set property &rest definations)
  (node name &rest definations)
  (arc from to &rest definations)
  (subgraph (name &rest definations)
    ...)

For example:

  (with-dot (output-path :type :svg)
    (set node :shape :rect)
    (node :a :label \"start\"))
"
  (with-gensyms (stream in)
    `(let ((,stream (make-string-output-stream)))
       (format ,stream ,(if diredp "digraph {" "graph {"))
       (format ,stream "~&~{~a=~a;~}"
               ',(plist-remove-key keys :diredp :debug))
       (progn ,@(mapcar (lambda (expr)
                          (escape-dot-expr stream diredp expr))
                        body))
       (format ,stream "~&}")
       (with-input-from-string
           (,in ,(if debug
                     `(print (get-output-stream-string ,stream) *error-output*)
                     `(get-output-stream-string ,stream)))
         (run-dot ,in ,output ,@keys)))))
