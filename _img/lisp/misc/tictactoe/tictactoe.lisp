(defpackage #:tictactoe
  (:use :cl :cl-webview)
  (:export #:run-tictactoe))

(in-package :tictactoe)

(defclass tictactoe ()
  (;; 棋子的信息 (初始为空)
   (chessboard :initform (make-array 9 :initial-element :empty))
   ;; 当前的控制者 (初始为 `:red' 红方)
   (player     :initform :red  :reader player)
   ;; 游戏状态
   (status     :initform :play :reader status))
  (:documentation "井字棋核心逻辑. "))

(defmethod drop-chess ((game tictactoe) chess-id)
  (with-slots (chessboard player) game
    (when (eq (aref chessboard chess-id) :empty)  ; `chess-id' 处为空
      (setf (aref chessboard chess-id) player)))) ; 在 `chess-id' 处落子

(defmethod swap-player ((game tictactoe))
  (with-slots (player) game
    ;; 若当前为 `:red' 则交换为 `:blue' 反之亦然
    (setf player (if (eq player :red) :blue :red))))

(defun %chessboard-status (chessboard)
  "根据 `chessboard' 得到可能的状态. "
  ;; `flet' 类似 `let', 前者绑定局部函数, 后者绑定局部变量
  (flet ((line? (i j k)
           ;; 判断坐标上的点的 grid 是否相同, 若相同, 返回相同的值
           (let ((value (reduce (lambda (a b) (if (eq a b) a nil))
                                (mapcar (lambda (idx) (aref chessboard idx))
                                        (list i j k)))))
             (and (not (eq value :empty)) value))))
    (let ((full? (= 9 ;; 对非空 grid 计数并求和, 判断是否和为 9 (全填满)
                    (reduce #'+ (map 'list
                                     (lambda (grid)
                                       (if (eq grid :empty) 0 1))
                                     chessboard))))
          (line? (or
                  ;; 横向相连
                  (line? 0 1 2) (line? 3 4 5) (line? 6 7 8)
                  ;; 纵向相连
                  (line? 0 3 6) (line? 1 4 7) (line? 2 5 8)
                  ;; 对角线相连
                  (line? 0 4 8) (line? 2 4 6))))
      ;; 根据 `chessboard' 返回状态
      (cond ((eq line? :red)  :red-win)
            ((eq line? :blue) :blue-win)
            (full?            :full)
            (t                :play)))))

(defmethod update-status ((game tictactoe))
  (with-slots (chessboard status) game
    (setf status (%chessboard-status chessboard))))

(defmethod clear-board ((game tictactoe))
  (with-slots (chessboard player status) game
    (loop for i below 9 do (setf (aref chessboard i) :empty)) ; 清空为 `:empty'
    (setf player (case status           ; 交换控制方, 输者先攻
                   (:red-win  :blue)
                   (:blue-win :red)
                   (otherwise player)))
    (setf status :play)))               ; 重置 `status'

(defmethod drop-chess :around ((game tictactoe) chess-id)
  (with-slots (status) game
    (when (and (eq status :play)        ; 状态为 :play 时才可落子
               (call-next-method))      ; 可以落子 (落子结果非 nil)
      (update-status game)              ; 更新棋局状态
      (swap-player   game))))           ; 交换控制方

(defmethod print-object :after ((game tictactoe) stream)
  (with-slots (chessboard player status) game
    (format stream "~& PLAYER: ~A, STATUS: ~A" player status)
    (loop for row below 3
          do (format stream "~&| ")
          do (loop for col below 3
                   do (format stream "~A | "
                              (case (aref chessboard (+ (* row 3) col))
                                (:empty " ")
                                (:red   "X")
                                (:blue  "O")))))))

(defclass webview-mixin ()
  (;; 棋盘的尺寸
   (size :initform 400
         :initarg  :size
         :reader   size)
   ;; 控制组件的高度
   (control-height :initform 100
                   :initarg  :control-height
                   :reader   control-height)
   ;; webview 窗体
   webview)
  (:documentation "绘制到 webview 窗体上的相关模块. "))

(defconstant +table-control-string+
  "<td style='width: ~dpx; height: ~dpx' id='~A' onclick='drop_chess(~d)'></td>"
  "用于输出 td. ")

(defmethod dump-chessboard ((win webview-mixin) stream)
  (let* ((gridsize  (floor (/ (size win) 3))))
    (format stream "<table border='1'>")
    (loop for row below 3
          do (format stream "<tr>")
          do (loop for col below 3
                   for chess-id = (+ (* row 3) col)
                   do (format stream +table-control-string+
                              gridsize gridsize chess-id chess-id))
          do (format stream "</tr>"))
    (format stream "</table>")))

(defmethod dump-controls :around ((win webview-mixin) stream)
  (format stream "<div>")
  (call-next-method)
  (format stream "</div>"))

(defmethod dump-controls ((win webview-mixin) stream)
  (format stream "<button onclick='clear_board()'>Clear Board</button>"))

(defun html (tictactoe)
  "返回一个绘制井字棋 `tictactoe' 的 HTML. "
  (with-output-to-string (stream)
    (format stream "<body>")
    (dump-chessboard tictactoe stream)
    (dump-controls   tictactoe stream)
    (format stream "</body>")))

(defmethod initialize-instance :after ((game webview-mixin) &key debug)
  (with-slots (size control-height webview) game
    ;; 初始化时绑定窗体大小以及 HTML
    (setf webview (make-webview :debug  debug
                                :width  size
                                :height (+ size control-height)
                                :title  "TicTacToe"
                                :hints  :fixed
                                :html   (html game)))
    ;; 绑定交互的事件的逻辑
    (webview-bind (webview chess-id) "drop_chess"
      (drop-chess game chess-id))
    (webview-bind (webview) "clear_board"
      (clear-board game))))

(defconstant +drop-control-string+
  "document.getElementById('~d').style.background = '~(~A~)';"
  "落子的 JS 控制代码. ")

;; 在落子后将对应格子的颜色更新
(defmethod drop-chess :after ((game webview-mixin) chess-id)
  (with-slots (webview chessboard) game
    (let ((grid (aref chessboard chess-id)))
      (webview-eval webview (format nil +drop-control-string+
                                    chess-id (case grid
                                               (:red "red")
                                               (:blue "blue")))))))

(defconstant +title-format-control+
         "TicTacToe (~A)"
         "显示窗口的 title 的格式. ")

(defmethod clear-board :before ((game webview-mixin))
  (with-slots (webview player) game
    (dotimes (i 9)
      (webview-eval webview (format nil +drop-control-string+ i "white"))
      (webview-set-title webview (format nil +title-format-control+ player)))))

(defmethod update-status :after ((game webview-mixin))
  (with-slots (status webview) game
    (unless (eq status :play)
      (webview-set-title webview (format nil +title-format-control+ status)))))

(defmethod swap-player :after ((game webview-mixin))
  (with-slots (status player webview) game
    (when (eq status :play)
      (webview-set-title webview (format nil +title-format-control+ player)))))

(defclass tictactoe-webview (tictactoe webview-mixin) ()
  (:documentation "井字棋游戏"))

(defclass ai-player-mixin ()
  ((ai-player :initform :blue :initarg :ai))
  (:documentation "AI mixin for tictactoe class. "))

(defgeneric ai-choose-drop (tictactoe)
  (:documentation "返回 AI 决定的落子位置. "))

(defmethod swap-player :after ((game ai-player-mixin))
  (with-slots (player ai-player status) game
    (when (and (eq player ai-player)    ; 轮到 AI
               (eq status :play))       ; 游戏还能继续玩
      (let ((drop (ai-choose-drop game)))
        (drop-chess game drop)))))

(defmethod clear-board :after ((game ai-player-mixin))
  (with-slots (player ai-player status) game
    (when (and (eq player ai-player)    ; 轮到 AI
               (eq status :play))       ; 游戏还能继续玩
      (let ((drop (ai-choose-drop game)))
        (drop-chess game drop)))))

(defclass montain-gorilla-mixin (ai-player-mixin) ()
  (:documentation "随机落子的 AI. "))

(defclass tictactoe-gorilla (tictactoe-webview montain-gorilla-mixin) ())

(defmethod ai-choose-drop ((game montain-gorilla-mixin))
  (with-slots (chessboard) game
    (let ((remain (loop for i below 9
                        if (eq (aref chessboard i) :empty)
                          collect i)))
      ;; 在剩余的空格子中随便挑出一个落子
      (nth (random (length remain)) remain))))

(defun %empty-grids (chessboard)
  "找到 `chessboard' 中所有空格子. "
  (loop for i below 9 if (eq (aref chessboard i) :empty) collect i))

(defun %get-score (status ai-player)
  "对当前棋局进行打分. "
  (cond ((or (and (eq ai-player :red)
                  (eq status :red-win))
             (and (eq ai-player :blue)
                  (eq status :blue-win)))
         1)                ; 若 AI 获胜, score = 1
        ((or (and (eq ai-player :red)
                  (eq status :blue-win))
             (and (eq ai-player :blue)
                  (eq status :red-win)))
         -1)               ; 若非 AI 获胜, score = -1
        (t                 ; 平局, score = 0
         0)))         

(defun %minimax (chessboard player ai-player &optional (depth 3))
  "返回一个最适合的落点点位, 以及其对应的打分.
Return (values score choose). "
  (let ((status (%chessboard-status chessboard)))
    (if (or (zerop depth)
            (not (eq status :play)))
        (%get-score status ai-player)
        (let ((best (if (eq player ai-player)
                        most-negative-fixnum   ; 最大化边界
                        most-positive-fixnum)) ; 最小化边界
              (choose -1)                      ; 选择的点位
              score)
          (dolist (next (%empty-grids chessboard))
            (let ((chessboard (alexandria:copy-array chessboard)))
              (setf (aref chessboard next) player) ; 落子
              (setf score (%minimax chessboard     ; 计算落子后 `chessboard' 对应的分数
                                    (if (eq player :red) :blue :red)
                                    ai-player (1- depth))))
            (cond ((and (eq player ai-player)
                        (> score best))   ; 最大化己方得分
                   (setf best   score
                         choose next))
                  ((and (not (eq player ai-player))
                        (< score best))   ; 最小化敌方得分
                   (setf best score
                         choose next))))
          (values best choose)))))

(defclass minimax-mixin (ai-player-mixin) ()
  (:documentation "Minimax AI"))

(defmethod ai-choose-drop ((game ai-player-mixin))
  (with-slots (chessboard player ai-player) game
    (multiple-value-bind (score choose)
        (%minimax chessboard player ai-player 4)
      (declare (ignore score))
      choose)))

(defclass tictactoe-minimax (tictactoe-webview minimax-mixin) ())

(defclass history-mixin ()
  ((history :initform ()))
  (:documentation "历史记录"))

;; 往历史记录中添加记录: (落点 . 玩家)
(defmethod drop-chess :before ((game history-mixin) chess-id)
  (with-slots (history) game
    (push chess-id history)))

(defmethod undrop-chess ((game history-mixin) chess-id)
  (with-slots (chessboard) game
    (setf (aref chessboard chess-id) :empty)))

(defmethod undrop-chess :before ((game webview-mixin) chess-id)
  (with-slots (webview) game
    (webview-eval webview (format nil +drop-control-string+ chess-id "white"))))

;; 悔棋
(defmethod undo ((game history-mixin))
  (with-slots (history) game
    (when history ;; 有历史记录可以回退
      ;; 清空历史记录中的格点
      (undrop-chess game (pop history))
      ;; 交换控制方
      (swap-player game))))

(defclass tictactoe-undo (tictactoe history-mixin webview-mixin) ())

(defmethod dump-controls ((game tictactoe-undo) stream)
  (declare (ignore game))
  (format stream "<button onclick='clear_board()'>Clear Board</button>")
  (format stream "<button onclick='undo()'>Undo</button>"))

(defmethod initialize-instance :after ((game tictactoe-undo) &key)
  (with-slots (webview) game
    (webview-bind (webview) "undo"
      (undo game))))

(defun square (x) (* x x))

(defun sigmoid (x)
  (let ((expx (exp x)))
    (/ expx (1+ expx))))

(defun d-sigmoid (x)
  (let ((expx (exp x)))
    (/ expx (square (1+ expx)))))

(defun relu (x)
  (if (> x 0) x 0))

(defun d-relu (x)
  (if (> x 0) 1 0))

(defclass layer ()
  ((weights  :accessor weights)
   (%out     :accessor %out)            ; 当前计算未过激活函数的输出
   (%in      :accessor %in)             ; 当前计算的输入
   (%rms     :accessor %rms)            ; 当前计算的误差
   (inputs   :initform 10          :initarg :inputs   :reader   inputs)
   (outputs  :initform 64          :initarg :outputs  :reader   outputs)
   (active   :initform #'sigmoid   :initarg :active   :reader   active)
   (d-active :initform #'d-sigmoid :initarg :d-active :reader   d-active)
   (learning-rate :initform 1e-3
                  :initarg  :learning-rate
                  :accessor learning-rate))
  (:documentation "一层神经元"))

;; 初始噪声的强度通过 `:noise' 来进行控制
(defmethod initialize-instance :after ((layer layer) &key (noise 0.1d0))
  (with-slots (weights inputs outputs) layer
    (setf weights
          (make-array (list (outputs layer) (inputs layer))
                      :initial-contents
                      (ryo:collect-i* ((i (inputs layer)) (j (outputs layer)))
                        (random noise))))))

(defgeneric feedforward (layer input)
  (:documentation "计算前向传播"))

(defgeneric feedbackward (layer err)
  (:documentation "反向传播误差并更新权重"))

(defmethod feedforward ((layer layer) input)
  (with-slots (%in %out outputs inputs weights active) layer
    (setf %in input)
    (setf %out
          (make-array outputs
                      :initial-contents
                      (ryo:collect-i* ((j outputs))
                        (ryo:sum-iter-i* ((i inputs))
                          (* (aref weights j i) (aref input i))))))
    (map 'vector active %out)))

(declaim (inline dot vec-sub num-mul))
(defun dot (vec1 vec2)
  (map 'vector #'* vec1 vec2))

(defun cross (vec1 vec2 &optional (scale 1d0))
  (let ((n (length vec1))
        (m (length vec2)))
    (make-array (list m n)
                :initial-contents
                (ryo:collect-i* ((i (length vec1)) (j (length vec2)))
                  (* scale (aref vec1 i) (aref vec2 j))))))

(defun num-mul (num vec)
  (map 'vector (lambda (vi) (* vi num)) vec))

(defun vec-sub (vec1 vec2)
  (map 'vector #'- vec1 vec2))

;; 返回传递给上一级的误差
(defmethod feedbackward ((layer layer) err)
  (with-slots (inputs outputs weights) layer
    (make-array (list inputs)
                :initial-contents
                (ryo:collect-i* ((i inputs))
                  (ryo:sum-iter-i* ((j outputs))
                    (* (ryo:at weights i j) (ryo:at err j)))))))

;; 更新当前权值
(defmethod feedbackward :after ((layer layer) err)
  (with-slots (inputs outputs d-active %in %out weights learning-rate)
      layer
    (let* ((dedv (dot err (map 'vector d-active %out))))
      ;; weights_ij = learning_rate * dedv_j * y_i
      (ryo:iter-i* ((i inputs) (j outputs))
        (incf (ryo:at weights i j)
              (* learning-rate (ryo:at dedv j) (ryo:at %in i)))))))

(defun %train (layer input output)
  (let ((err (vec-sub output (feedforward layer input))))
    (feedbackward layer err)
    (format t "~&ERROR: ~f" (reduce #'+ (map 'list #'square err)))
    (force-output)))

(defun train (layer input-output*
              &key (learning-rate (learning-rate layer))
                (repeat 1))
  (setf (learning-rate layer) learning-rate)
  (dotimes (i repeat)
    (dolist (samples input-output*)
      (%train layer (car samples) (cdr samples)))))

(defclass network ()
  ((layers :accessor layers)))

(defmethod (setf learning-rate) (lr (net network))
  (with-slots (layers) net
    (loop for i below (length layers)
          for layer = (aref layers i)
          do (setf (learning-rate layer) lr))))

(defmethod feedforward ((net network) input)
  (with-slots (layers) net
    (loop for i below (length layers)
          for layer = (aref layers i)
          for output = (feedforward layer input)
            then (feedforward layer output)
          finally (return output))))

(defmethod feedbackward ((net network) err)
  (with-slots (layers) net
    (loop for i from (1- (length layers)) downto 0
          for layer = (aref layers i)
          for back-err = (feedbackward layer err)
            then (feedbackward layer back-err)
          finally (return back-err))))

(defun make-tictactoe-mlp ()
  (let ((input-layer  (make-instance 'layer :inputs   10
                                            :outputs  64
                                            :active   #'relu
                                            :d-active #'d-relu))
        (hidden-layer (make-instance 'layer :inputs   64
                                            :outputs  64
                                            :active   #'relu
                                            :d-active #'d-relu))
        (output-layer (make-instance 'layer :inputs   64
                                            :outputs  9
                                            :active   #'sigmoid
                                            :d-active #'d-sigmoid))
        (net          (make-instance 'network)))
    (setf (layers net)
          (make-array 3 :initial-contents (list input-layer
                                                hidden-layer
                                                output-layer)))
    (setf (learning-rate net) 0.01)      ; 学习率高一点...
    net))

(defclass mlp-mixin (ai-player-mixin history-mixin)
  ((model :initform (make-tictactoe-mlp) :reader model)))

(defun %mlp-input (chessboard)
  "将 `chessboard' 变换为标准的 MLP 模型的输入"
  (flet ((val (g)
           (case g
             (:red   1)
             (:empty 0)
             (:blue -1))))
    (make-array 10 :initial-contents (cons 1d0 (map 'list #'val chessboard)))))

(defmethod ai-choose-drop ((game mlp-mixin))
  (with-slots (model chessboard history) game
    (let* ((output     (feedforward model (%mlp-input chessboard)))
           (p-patterns (reduce (lambda (p-patterns i)
                                 (cond ((> (aref output i) (car p-patterns))
                                        (list (aref output i) i))
                                       ((= (aref output i) (car p-patterns))
                                        (push i (cdr p-patterns)))
                                       (t p-patterns)))
                               (%empty-grids chessboard)
                               :initial-value '(-1 0))))
      (nth (random (length (cdr p-patterns))) (cdr p-patterns)))))

(defclass tictactoe-mlp (tictactoe-webview mlp-mixin) ())

;; 从历史记录中学习胜者落子
(defmethod learn-from-history ((game mlp-mixin) learn-player)
  (with-slots (model chessboard history) game
    (flet ((get-output ()
             (make-array 9 :initial-contents
                         (ryo:collect-i* ((i 9))
                           (if (eq (car history) i) 1 0)))))
      (when (eq (player game) learn-player) ; 当前玩家为需要学习的玩家
        (%train model (%mlp-input chessboard) (get-output)))
      (when (undo game)                 ; 如果还有可学习的历史
        (learn-from-history game learn-player)))))

(defmethod clear-board :before ((game mlp-mixin))
  (with-slots (status ai-player) game
    (case status
      (:red-win  (learn-from-history game :red))
      (:blue-win (learn-from-history game :blue))
      (:full     (learn-from-history game (if (eq ai-player :red)
                                              :blue :red))))))
