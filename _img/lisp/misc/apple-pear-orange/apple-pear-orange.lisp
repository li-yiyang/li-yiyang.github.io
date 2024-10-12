(defun z-mod-ring-add (m)
  "得到一个简单的 Z mod m 环上的加法函数."
  (lambda (&rest lst)
    (reduce (lambda (r1 r2) (mod (+ r1 r2) m))
            lst)))

(defun z-mod-m-ring-patterns (m)
  "苹果, 梨, 橘子按照 Z mod m 结果组合为 m^3 种.

返回的结果为一个元素为 (苹果个数 梨个数 橘子个数) 的列表."
  (let ((upper (1- m))
        (res   '()))
    (loop for apple from 0 upto upper do
          (loop for pear from 0 upto upper do
                (loop for orange from 0 upto upper do
                      (push (list apple pear orange) res))))
    res))

(defun modp (number divider)
  "测试 NUMBER 是否能够被 DIVIDER 整除."
  (eq 0 (mod number divider)))

(defun pick-odd-patterns-mod (m)
  "得到一个测试 PATTERN 是否不能够被 M 整除. 
若不能被整除的, 则返回 T 的函数.

如果 M 为一个 list, 则将会按照规则一一对 PATTERN 整除, 
比如说 PATTERN 为 (p1 p2 p3), M 为 (m1 m2 m3) 则会变成 pj mod mj."
  (lambda (pattern)
    (let ((mlst (if (atom m)
                    (make-list (length pattern)
                               :initial-element m)
                    m)))
      (not (reduce (lambda (a b) (and a b))
                   (mapcar #'modp pattern mlst))))))

(defun pick-matched-pattern-mod (m)
  "得到一个测试 PATTERN 是否都能够被 M 整除, 若都能整除则返回 T 的函数."
  (lambda (pattern)
    (let ((mlst (if (atom m)
                    (make-list (length pattern)
                               :initial-element m)
                    m)))
      (reduce (lambda (a b) (and a b))
              (mapcar #'modp pattern mlst)))))

(defun pick-odd-merge-pattern (m)
  "选择两两合并后, 是不能被 M 整除的合并函数."
  (lambda (patterns)
    (let ((upper  (1- (length patterns)))
          (adder  (z-mod-ring-add m))
          (picker (pick-odd-patterns-mod m))
          (res    '()))
      (loop for g1 from 0 upto upper do
            (loop for g2 from g1 upto upper do
                  (let* ((group1 (nth g1 patterns))
                         (group2 (nth g2 patterns))
                         (merged (mapcar adder group1 group2)))
                    (when (funcall picker merged)
                      (push (list g1 g2) res)))))
      res)))

(defun pick-out-patterns (m patterns &key (odd T))
  "选择 PATTERNS 中两两相加后, 满足条件的两两序号对. 
其中:
+ PATTERNS 为元素为 (a1 a2 ...) 形式的模式列表
+ 若 ODD 为 T, 则选择不能都满足的对, 若 NIL 选择能都满足的对."
  (let ((adder  (lambda (pat1 pat2)
                  (mapcar (z-mod-ring-add m) pat1 pat2)))
        (picker (if odd
                    (pick-odd-patterns-mod m)
                    (pick-matched-pattern-mod m)))
        (size   (length patterns))
        (pairs  '()))
    (loop for i below size do
      (loop for j from i below size
            if (funcall picker (funcall adder (nth i patterns)
                                        (nth j patterns)))
              do (push (list i j) pairs)))
    pairs))

(defparameter *default-headers*
  '("layout = fdp;"
    "node [shape=\"circle\"];")
  "默认的 Graphiz 的设置.")

(defun arc-to-graph (arcs &key (headers *default-headers*)
                            (stream *standard-output*))
  "将无向图 ARCS 输出为 Graphviz 的代码并打印. 默认输出到标准输出."
  (format stream "graph {~%")
  (loop for header in headers do
        (format stream "  ~A~%" header))
  (loop for arc in arcs do
        (format stream "  ~A -- ~A;~%" (first arc) (second arc)))
  (format stream "}"))

(defun arc-to-matrix (size arcs)
  "将边组 ARCS 变换为邻接矩阵形式. 

其中 ARCS 的形式为 ((点1 点2) ...), 是无向图.
得到的 MATRIX 的形式为 ((a11 a12 ...) (a21 a22 ...) ...)."
  (let ((matrix (loop for i from 0 below size
                      collect (make-list size :initial-element 0))))
    (loop for arc in arcs do
          (let ((p1 (first  arc))
                (p2 (second arc)))
            (setf (at matrix p1 p2) 1
                  (at matrix p2 p1) 1)))
    matrix))

;;; 最大值记录帮助函数
(let ((max-value   0)
      (max-pattern NIL))
  (defun max-reset (&optional (max 0))
    "重置 MAX-VALUE 的值为 MAX, 默认为 0."
    (setf max-value   max
          max-pattern NIL))

  (defun re-max (&optional pattern)
    "比较 PATTERN 长度和 MAX-VALUE 的大小并更新 MAX-VALUE 的值. 
返回 MAX-VALUE 和 MAX-PATTERN."
    (let ((value (length pattern)))
      (when (and value (> value max-value))
        (setf max-value value
              max-pattern pattern)))
    (values max-value max-pattern)))

(defun max-connection-matrix (matrix)
  "在邻接矩阵的基础上查找最大的两两连接图."
  (labels ((test-connection (node others)
             "判断 NODE 与 OTHERS 之间是否相连."
             (loop for other in others
                   if (not (eq 1 (at matrix node other)))
                     do (return NIL)
                   finally (return T)))
           (search-max (to-search searched-nodes)
             "查找最大两两连接图的递归函数."
             (if (null to-search)
                 (re-max searched-nodes)
                 (let ((node  (first to-search)))
                   (search-max (rest to-search)
                               (if (test-connection node searched-nodes)
                                   (cons node searched-nodes)
                                   searched-nodes))))))

    (max-reset)                         ; 重置最大值
    (let* ((size  (length matrix))
           (nodes (loop for idx below size collect idx)))
      (loop for start in nodes do       ; 选择不同的起点
            (search-max nodes (list start))))
    (re-max)))

(defun test (p)
  "简单的测试函数, 测试对于整除 P 的组合的可能的结果数量."
  (let* ((patterns     (z-mod-m-ring-patterns p))
         (odd-patterns (pick-out-patterns p patterns))
         (matrix       (arc-to-matrix (expt p 3) odd-patterns)))
    (multiple-value-bind (size pattern-idx)
        (max-connection-matrix matrix)
      (values size
              pattern-idx
              patterns))))

(defun test-connection (nodes matrix)
  "测试 NODES 在 MATRIX 中是否是两两相连的. 
返回 T 如果是两两相连的, 否则返回 NIL.

示例代码:
    (let* ((p 3)
           (patterns     (z-mod-m-ring-patterns p))
           (odd-patterns (pick-out-patterns p patterns))
           (matrix       (arc-to-matrix (expt p 3) odd-patterns)))
      (multiple-value-bind (- pattern-idx)
          (max-connection-matrix matrix)
        (test-connection pattern-idx matrix)))
返回的结果应当为 T."
  (let ((upper (length nodes)))
    (loop for i below upper
          if (loop for j from (1+ i) below upper
                   if (eq 0 (at matrix (nth i nodes) (nth j nodes)))
                     do (return T)
                   finally (return NIL))
            do (return NIL)
          finally (return T))))

(defun test-max-connection (nodes matrix)
  "测试是否为最大的连接.

示例代码:
     (let* ((p 3)
           (patterns     (z-mod-m-ring-patterns p))
           (odd-patterns (pick-out-patterns p patterns))
           (matrix       (arc-to-matrix (expt p 3) odd-patterns)))
      (multiple-value-bind (- pattern-idx)
          (max-connection-matrix matrix)
        (test-max-connection pattern-idx matrix)))
结果应当为 T."
  (loop for idx below (length matrix)
        if (and (not (find idx nodes))
                (test-connection (cons idx nodes) matrix))
          do (return NIL)
        finally (return T)))

(defun make-matrix-of (p)
  "计算 P 下的组合图, 并将其以矩阵的形式输出."
  (let ((arcs '()))
    (loop for a from 0 below p do
          (loop for b from a below p
                if (not (modp (+ a b) p))
                  do (push (list a b) arcs)))
    (arc-to-matrix p arcs)))

(defun min-combination-pattern (matrix)
  "计算通过邻接矩阵 MATRIX 得到的最小长度和所有最小的组合."
  (multiple-value-bind (size patterns)
      (max-connection-matrix matrix)
    (let* ((nodes (loop for idx from 0 below (length matrix) collect idx))
           (new-pat (loop for node in nodes
                          if (loop for pattern in patterns
                                   if (eq 0 (at matrix node pattern))
                                     do (return T)
                                   finally (return NIL))
                            collect (cons node patterns))))
      (values (1+ size) new-pat))))

(defun combine-by-patterns (&rest patterns)
  "PATTERNS 为可能的模式组合的列表以及长度, 如:
(combine-by-patterns '(1 0) '(1 0)) 将得到 (1 1) (1 0) (0 1) (0 0) 
并且在组合中会剔除重复的模式."
  (labels ((combine-two-pattern (pat1 pat2)
             (let ((res '()))
               (loop for elem1 in pat1 do
                     (loop for elem2 in pat2 do
                           (setf res (union res (list (cons elem2 elem1))
                                            :test #'equal))))
               res)))
    (let ((patterns (reduce #'combine-two-pattern patterns
                            :initial-value '(NIL))))
      (values patterns (length patterns)))))

(defun combine-by-n-pattern (n pattern)
  "N 个 PATTERN 的组合的不重复的组合以及组合的长度."
  (apply #'combine-by-patterns
         (make-list n :initial-element pattern)))

(defun combine-patterns-count (n p)
  "对于 N 个元素在模 P 组合下进行计数."
  (multiple-value-bind (- patterns)
      (min-combination-pattern (make-matrix-of p))
    (let* ((combinations
             (mapcar (lambda (pattern)
                       (multiple-value-bind (pat size)
                           (combine-by-n-pattern n pattern)
                         (list size pat)))
                     patterns))
           (combination
             (sort combinations #'< :key #'first)))
      (values (length (second (first combination)))
              combination))))
