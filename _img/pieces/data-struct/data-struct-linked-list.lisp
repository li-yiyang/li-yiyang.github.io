;;; Data Struct
(defpackage :data-struct
  (:use :cl))

(in-package :data-struct)

(defun eq-to (value)
  "Generate a compare func to test if equal to VALUE."
  (lambda (another) (eq value another)))

(defun pick-elem-of (lst &optional (rule 'random))
  "Pick elem of LST by RULE."
  (if (null lst)
      NIL
      (cond ((eq rule 'first)  (first lst))
            ((eq rule 'last)   (car (last lst)))
            ((eq rule 'middle) (pick-elem-of 2))
            ((eq rule 'random) (nth (random (length lst)) lst))
            ((integerp rule)   (nth (floor (/ (length lst) rule)) lst))
            (T (pick-elem-of lst 'random)))))

(defun filter (func lst &key (map NIL))
  "Filter a LST by FUNC."
  (loop for elem in lst
        if (funcall func elem)
          collect (if map (funcall map elem) elem) into success
        else
          collect (if map (funcall map elem) elem) into fail
        finally (return (values success fail))))

(defun join (lst &optional (spliter "~%"))
  "Join elements in LST, splitted by SPLITER."
  (cond ((null lst) "")
        ((= (length lst) 1) (format nil "~A" (first lst)))
        (T (format nil "~A~A~A"
              (first lst)
              (format nil spliter)
              (join (cdr lst) spliter)))))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

(defmacro at (nested &rest indexs)
  "Nested nth for NESTED via INDEXS. 
For example: (at matrix row col) -> (nth col (nth row matrix))"
  (if (null indexs)
      nested
      (cons 'at (cons `(nth ,(car indexs) ,nested) (cdr indexs)))))

(defun leafp (elem)
  "Test if an ELEM is a leaf."
  (atom elem))

(defun tree-node (elem)
  "Read node info from ELEM."
  (if (leafp elem)
      elem
      (first elem)))

(defun tree-leaf (elem)
  "Read leaves of ELEM."
  (if (leafp elem)
      NIL
      (rest elem)))

(defun pre-order (tree func &optional (parent NIL))
  "Pre-order iterate the TREE via FUNC."
  (let ((node (tree-node tree))
        (leaf (tree-leaf tree)))
    (cons
     (funcall func node parent)
     (loop for elem in leaf
           if (leafp elem)
             collect (funcall func elem node)
           else
             collect (pre-order elem func node)))))

(defun post-order (tree func &optional (parent NIL))
  "Post-order iterate the TREE via FUNC."
  (let* ((node (tree-node tree))
         (leaf (tree-leaf tree))
         (leaves (loop for elem in leaf
                       if (leafp elem)
                         collect (funcall func elem node)
                       else
                         collect (post-order elem func node))))
    (cons (funcall func node parent) leaves)))

(defun draw-tree (tree &key
                         (labelfn NIL)
                         (iter #'pre-order)
                         (caller NIL)
                         (embedding NIL)
                         (headers '("node [shape=\"circle\"];")))
  "Turn Tree to Graphviz code."
  (labels ((node-name (node)
             (if (atom node) node (join node "_"))))
    (when embedding (format t "digraph {~%"))
    (format t "~A" (join headers))
    (funcall iter tree
             (lambda (node parent)
               (format t "node_~A [label=\"~A\"];~%"
                       (node-name node)
                       (if labelfn (funcall labelfn node) (node-name node)))
               (when parent
                 (format t "node_~A -> node_~A;~%"
                         (node-name parent) (node-name node)))
               (when caller
                 (funcall caller node parent))))
    (when embedding (format t "}"))))

(defun search-m-M-tree (value tree)
  "Search VALUE in TREE (m-M-tree)."
  (let ((node (first tree))
        (idx  0))
    (if (null node)
        NIL                             ; Not Found
        (if (find value node)
            value
            (progn
              (loop while (and (nth idx node)
                               (< (nth idx node) value))
                    do (incf idx))
              (search-m-M-tree value (nth (1+ idx) tree)))))))

(defun draw-matrix-graph (graph-matrix &key
                                         (embedding NIL)
                                         (headers '("layout=fdp;"
                                                    "node [shape=\"circle\"];")))
  "Trun GRAPH-MATRIX into digraph."
  (when embedding (format t "digraph {~%"))
  (format t "~A~%" (join headers))
  (let ((nodes (rest (first graph-matrix))))
    (loop for node in nodes do
      (format t "node_~A [label=\"~A\"];~%" node node))
    (loop for from-node in nodes
          for row from 1 do
            (loop for to-node in nodes
                  for col from 1 do
                  (when (at graph-matrix row col)
                    (format t "node_~A -> node_~A;~%"
                             from-node to-node)))))
  (when embedding (format t "}")))

(defun draw-arc-graph (arc-graph &key
                                   (embedding NIL)
                                   (headers '("layout=fdp;"
                                              "node [shape=\"circle\"];")))
  "Trun ARC-GRAPH into digraph."
  (when embedding (format t "digraph{~%"))
  (format t "~A~%" (join headers))
  (mapcar (lambda (node) (format t "node_~A [label=\"~A\"];~%" node node))
          (getf arc-graph :nodes))
  (mapcar (lambda (arcs) (format t "node_~A -> node_~A [label=\"~A\"];~%"
                                 (first arcs) (second arcs)
                                 (if (nth 2 arcs) (nth 2 arcs) "")))
          (getf arc-graph :arcs))
  (when embedding (format t "}")))

(defun nodes-of-graph (graph &key (type 'arc-graph))
  "Find all nodes of GRAPH, in TYPE."
  (cond ((eq type 'arc-graph)    (getf graph :nodes))
        ((eq type 'matrix-graph) (rest (first graph)))
        (T NIL)))

(defun next-arcs-of (graph node &key (type 'arc-graph))
  "Find all next arcs of NODE in GRAPH, in TYPE."
  (cond ((eq type 'arc-graph)    (let ((arcs (getf graph :arcs)))
                                   (remove-if-not (eq-to node) arcs :key #'first)))
        ((eq type 'matrix-graph) (let ((header (rest (first graph)))
                                       (filter (rest (assoc node (rest graph)))))
                                   (loop for next in header
                                         for weight in filter
                                         if weight
                                           collect (list node next weight))))
        (T NIL)))

(defun depth-first-iter (func graph &key
                                      (arc-func NIL)
                                      (start NIL)
                                      (type 'arc-graph))
  "Depth first iteration, apply FUNC to GRAPH nodes."
  (let* ((nodes (copy-list (nodes-of-graph graph :type type)))
         (begin (if start start (first nodes)))
         (collection '()))
    (labels ((depth-first (node)
               (when (find node nodes)
                 ;; Eval node
                 (push (funcall func node) collection)
                 (setq nodes (remove node nodes))
                 ;; Search next immediately
                 (loop for arc in (next-arcs-of graph node :type type) do
                   (progn
                     ;; (arc-func arc back-arc?)
                     (if arc-func (funcall arc-func arc (find (second arc) nodes)))
                     (depth-first (second arc)))))))
      ;; If remain nodes, keeping searching.
      (loop while (not (null nodes))
            do (let ((node (if (find begin nodes) begin (first nodes))))
                 (depth-first node)))
      collection)))

(defun breadth-first-iter (func graph &key
                                        (arc-func NIL)
                                        (start NIL)
                                        (type 'arc-graph))
  "Breadth first iteration, apply FUNC to GRAPH nodes."
  (let* ((nodes (copy-list (nodes-of-graph graph :type type)))
         (begin (if start start (first nodes)))
         (collection '()))
    (labels ((breadth-first (&rest to-search)
               (let ((nexts '()))
                 ;; Next to search
                 (loop for node in to-search
                       if (find node nodes) do
                         (progn
                           ;; Eval nodes
                           (push (funcall func node) collection)
                           (setq nodes (remove node nodes))
                           ;; Add next to search nodes
                           (loop for arc in (next-arcs-of graph node :type type)
                                 do (progn
                                      (when arc-func
                                        (funcall arc-func
                                                 arc (find (second arc) nodes)))
                                      (when (find (second arc) nodes)
                                        (push (second arc) nexts))) )))
                 ;; If there's nodes next to search
                 (if nexts (apply #'breadth-first nexts)))))
      (loop while (not (null nodes))
            do (let ((node (if (find begin nodes) begin (first nodes))))
                 (breadth-first node)))
      collection)))

(defun iter-generate-tree (graph &key
                                   (iter #'depth-first-iter)
                                   (type 'arc-graph))
  "Generate a tree of GRAPH by DEPTH-FIRST-ITER or BREADTH-FIRST-ITER."
  (let ((nodes (nodes-of-graph graph :type type))
        (back-arcs '())
        (arcs '()))
    (funcall iter #'identity graph
             :type type
             :arc-func (lambda (arc back?)
                         (if back?
                             (push arc arcs)
                             (push arc back-arcs))))
    (values (list :nodes nodes :arcs arcs)
            back-arcs)))

(defun count-ring-of (graph &key
                              (iter #'depth-first-iter)
                              (type 'arc-graph))
  "Count ring number of a GRAPH by DEPTH-FIRST-ITER or BREADTH-FIRST-ITER."
  (let ((count 0))
    (funcall iter #'identity graph
             :type type
             :arc-func (lambda (- back?)
                         (when (not back?) (incf count))))
    count))

(defun insert-sort (lst &key
                          (key #'identity)
                          (compare #'<))
  "Insert sort."
  (labels ((iter (sorted unsort)
             (if (null unsort)
                 sorted
                 (multiple-value-bind (pick rest)
                     (select-most unsort :key key :compare compare)
                   (iter (append sorted (list pick)) rest)))))
    (iter '() lst)))

(defun select-most (lst &key
                          (key #'identity)
                          (compare #'<))
  "Select the most element in LST."
  (let ((most NIL)
        (most-elem NIL)
        (rest '()))
    (loop for elem in lst do
      (let ((value (funcall key elem)))
        (if (not most)
            (setq most value
                  most-elem elem)
            (if (funcall compare value most)
                (progn (push most-elem rest)
                       (setq most value
                             most-elem elem))
                (push elem rest)))))
    (values most-elem (reverse rest))))

(defun quick-sort (lst &key
                         (pivot 'random)
                         (key #'identity)
                         (compare #'<))
  "Quick sort."
  (if (or (<= (length lst) 1)
          (eq (first lst) (car (last lst))))
      lst
      (let ((pivot-value (pick-elem-of lst pivot)))
        (multiple-value-bind (less greater)
            (filter (lambda (elem)
                      (funcall compare (funcall key elem) pivot-value))
                    lst)
          (append
           (quick-sort less :pivot pivot :key key :compare compare)
           (quick-sort greater :pivot pivot :key key :compare compare))))))

(defun bucket-sort (lst &key
                          (key #'identity)
                          (compare #'<))
  "Bucket Sort."
  (let ((bucket '()))
    (labels ((add-to-bucket (elem)
               (let ((value (funcall key elem)))
                 (if (assoc value bucket)
                     (push elem (cdr (assoc value bucket)))
                     (push (cons value (list elem)) bucket)))))
      (loop for elem in lst do (add-to-bucket elem))
      (apply #'append (mapcar #'rest (sort bucket compare :key #'first))))))

(defun heap-parent-idx (idx)
  "Get parent idex relavent to IDX."
  (floor (/ (1- idx) 2)))

(defun heap-left-child-idx (idx)
  "Get left child relavent to IDX."
  (+ (* 2 idx) 1))

(defun heap-right-child-idx (idx)
  "Get right child relavent to IDX."
  (+ (* 2 idx) 2))

(defmacro swap (a b)
  "Swap A and B."
  `(let ((temp ,a))
     (setf ,a ,b
           ,b temp)))

(defun make-heap-from (lst &key (compare #'<))
  "Make heap from LST."
  (let ((heap (copy-list lst))
        (change-p T))
    (loop while change-p do
      (progn
        (setq change-p NIL)
        (loop for idx from 1 upto (1- (length lst)) do
          (when (funcall compare (nth (heap-parent-idx idx) heap) (nth idx heap))
            (setq change-p T)
            (swap (nth idx heap) (nth (heap-parent-idx idx) heap))))))
    heap))

(defun draw-heap (heap &key
                         (headers '("node [shape=\"circle\"];")))
  (format t "~A~%" (join headers))
  (loop for idx from 0 to (1- (length heap)) do
      (let ((node (nth idx heap))
            (lchild (nth (heap-left-child-idx idx) heap))
            (rchild (nth (heap-right-child-idx idx) heap)))
        (format t "node_~A [label=\"~A\"];~%" node node)
        (when lchild
          (format t "node_~A -> node_~A;~%" node lchild))
        (when rchild
          (format t "node_~A -> node_~A;~%" node rchild)))))

(defun heap-sort (lst &key (compare #'<))
  (let ((heap (make-heap-from lst :compare compare)))
    (if (<= (length heap) 1)
        heap
        (let* ((last (car (last heap)))
               (new-heap (cons last (subseq heap 1 (1- (length heap))))))
          (cons (first heap) (heap-sort new-heap))))))
