(defpackage #:llmisp.api
  (:use :cl)
  (:export
   #:*host*
   #:*api-key*
   #:*temperature*
   #:*model*
   #:define-api
   #:balance
   #:models
   #:chat-complete
   #:fill-in-middle))

(in-package :llmisp.api)

(defparameter *host* "api.deepseek.com"
  "DeepSeek API host. ")

(defparameter *api-key* "sk-you-should-set-it-your-self"
  "DeepSeek API key. ")

(defparameter *temperature* 1.0
  "Default temperature. ")

(defparameter *model* "deepseek-chat"
  "Default LLM model")
(defparameter *headers*
  '(("Accept"       . "application/json")
    ("Content-Type" . "application/json"))
  "Default API headers. ")

;; Utils
(defmacro do-plist ((key val plist &optional result) &body body)
  "Like `dolist' but on plist. "
  (let ((val-rest (gensym "VAL-REST")))
    `(loop for (,key . ,val-rest) on ,plist by #'cddr
           if (endp ,val-rest)
             do (error (format nil "Not property list: ~A. " ,plist))
           do (let ((,val (first ,val-rest))) ,@body)
           ,@(when result `(finally (return ,result))))))
(defmacro do-bind-list ((bindings list &optional result) &body body)
  "Like `do-bind-list' but with `destructuring-bind'. "
  `(loop for ,bindings in ,list
         do (progn ,@body)
         ,@(when result `(finally (return ,result)))))
(defun json (obj)
  "Turn Lisp `obj' to JSON string. "
  (with-output-to-string (json)
    (labels ((write-json (obj)
               (if (atom obj)
                   (cond ((null obj)      (write-string "false" json))
                         ((eq obj t)      (write-string "true"  json))
                         ((eq obj :null)  (write-string "null"  json))
                         ((eq obj :true)  (write-string "true"  json))
                         ((eq obj :false) (write-string "false" json))
                         ((stringp obj)   (shasht:write-json-string obj json))
                         (T               (shasht:write-json        obj json)))
                   (cond ((and (evenp (length obj)) (keywordp (first obj)))
                          ;; plist as object
                          (write-char #\{ json)
                          (shasht:write-json-string (str:snake-case (pop obj)) json)
                          (write-char #\: json)
                          (write-json (pop obj))
                          (do-plist (key val obj)
                            (write-char #\, json)
                            (shasht:write-json-string (str:snake-case key) json)
                            (write-char #\: json)
                            (write-json val))
                          (write-char #\} json))
                         (T
                          ;; normal list write as array
                          (write-char #\[ json)
                          (write-json (pop obj))
                          (dolist (elem obj)
                            (write-char #\, json)
                            (write-json elem))
                          (write-char #\] json))))))
      (write-json obj))))
(defun gethash* (hash-table &rest keys)
  "Recursively get keys from `hash-table'.
Return `default' if not found. "
  (if (endp keys) hash-table
      (let* ((key  (first keys))
             (hash (if (functionp key)
                       (funcall key hash-table)
                       (gethash key hash-table))))
        (apply #'gethash* hash (rest keys)))))

;; API
(define-api balance (res :get "user/balance")
  "Get user current balance.
Return values are total balance and raw JSON obj hash-table. "
  (when (gethash "is_available" res)
    (values (parse-float:parse-float
             (gethash* res "balance_infos" #'first "total_balance"))
            res)))
(define-api models (res :get "models")
  "Return a list of models name and raw JSON obj hash-table.

Lists the currently available models, and provides basic
information about each one such as the owner and availability.

Check Models & Pricing for our currently supported models."
  (values (mapcar #'(lambda (model) (gethash "id" model))
                  (gethash "data" res))
          res))
(define-api chat-complete (res :post "chat/completions"
                               :required (messages)
                               :optional ((model             *model*)
                                          (frequency-penalty 0)
                                          (max-tokens        4096)
                                          (presence-penalty  0)
                                          (response-format   :null)
                                          (stop              :null)
                                          (stream            nil)
                                          (stream-options    :null)
                                          (temperature       *temperature*)
                                          (top-p             1.0)
                                          (tools             :null)
                                          (tool-choice       "none")
                                          (logprobs          nil)
                                          (top-logprobs      :null)))
  "Creates a model response for the given chat conversation.
Return output message and raw JSON obj hash-table. "
  (:check
   (or (eq frequency-penalty :null) (<= -2 frequency-penalty 2))
   (or (eq max-tokens        :null) (< 1 max-tokens 8192))
   (or (eq presence-penalty  :null) (<= -2 presence-penalty  2))
   (or (eq temperature       :null) (<= 0  temperature 2))
   (or (eq temperature       :null) (<= top-p 1)))
  (values (gethash* res "choices" #'first "message" "content")
          res))
(define-api fill-in-middle (res :post "beta/completions"
                                :required (prompt)
                                :optional ((model             *model*)
                                           (echo              nil)
                                           (frequency-penalty 0)
                                           (logprobs          0)
                                           (max-tokens        4096)
                                           (presence-penalty  0)
                                           (stop              :null)
                                           (stream            nil)
                                           (suffix            :null)
                                           (temperature       *temperature*)
                                           (top-p             1)))
  "The FIM (Fill-In-the-Middle) Completion API.
Return values are message and raw JSON obj hash-table. "
  (:check
   (or (eq frequency-penalty :null) (<= -2 frequency-penalty 2))
   (or (eq logprobs          :null) (<= logprobs 20))
   (or (eq presence-penalty  :null) (<= -2 presence-penalty  2))
   (or (eq temperature       :null) (<= temperature 2))
   (or (eq top-p             :null) (<= top-p 1)))
  (values (gethash* res "choices" #'first "text")
          res))

(defpackage #:llmisp.try
  (:use :cl :llmisp.api)
  (:export
   #:ask-math))

(in-package :llmisp.try)

(defparameter *prompt-only-number*
  "Return only the finaly answer as number without any other text. ")
(defun ask-math (query &key (parse :read))
  "Ask with `query'.
Return results, raw string, raw JSON obj hash-table.

Arguments:
+ `parse':
  + `nil' for no parse, return raw string
  + `:int' for using `parse-integer'
  + `:float' for using `parse-float:parse-float'
  + `:read' for using `read-from-string'
  + `function' for using custom parsing function
"
  (multiple-value-bind (res raw)
      (chat-complete `((:content ,*prompt-only-number*
                        :role    "system")
                       (:content ,query
                        :role    "user"))
                     :temperature 0)
    (values
     (etypecase parse
       (null res)
       (function (funcall parse res))
       (keyword  (ecase parse
                   (:int   (parse-integer res :junk-allowed t))
                   (:float (parse-float:parse-float res :junk-allowed t))
                   (:read  (read-from-string res)))))
     res raw)))
(defun read-list-from-string (string &optional (max-retry 10))
  "Read list from `string', auto completing the `)' if eof. "
  (handler-case (read-from-string string)
    (end-of-file (err)
      (if (> max-retry 0)
          (read-list-from-string (str:concat string ")") (1- max-retry))
          (error err)))))

(defun ask-lisp-fim-expr (prefix suffix &key (max-tokens 128) (hints "") (echo t))
  "Using FIM to generate Lisp code. "
  (multiple-value-bind (res raw)
      (fill-in-middle (format nil
                              ";;; Common Lisp Code, output with no space indent
~A
;; ~A
"
                              prefix hints)
                      :suffix     suffix
                      :max-tokens max-tokens)
    (let ((res (str:concat prefix res suffix)))
      (when echo (write-string res echo))
      (values (read-list-from-string res)
              res raw))))

(defmacro lisp-fill-??? (expr &key (max-tokens 128) (hints "") (echo nil))
  (destructuring-bind (prefix suffix)
      (str:split "???" (let ((*print-pretty* nil))
                         (format nil "~A" expr)))
    `(ask-lisp-fim-expr ,prefix ,suffix
                        :max-tokens ,max-tokens
                        :hints      ,hints
                        :echo       ,echo)))
(defparameter *prompt-lisp-coder*
  "You are a common lisp code assitent. You should:
+ Return only common lisp s-expr without any other text.
+ Write code more lispy and functional
+ Not wrapp code in Markdown code block")

(defun ask-lisp-expr (query &key (goal '(:performance :readability :security)))
  "Ask for Lisp expression.
Return lisp expression, raw string, raw JSON hash-table. "
  (multiple-value-bind (res raw)
      (chat-complete `((:content ,*prompt-lisp-coder*
                        :role    "system")
                       (:content ,(format nil
                                          ";; Code Goal: ~{~A~^, ~}~%~A"
                                          goal query)
                        :role    "user"))
                     :temperature 0.2)
    (values (read-list-from-string
             (str:trim res :char-bag '(#\` #\Space #\Newline #\l #\i #\s #\p)))
            res raw)))
(defparameter *prompt-lisp-docstring*
  "You are a common lisp code assistent.
You should generate or refine the docstring with given lisp code.

Example 1:
Input
```lisp
(defun fibonacci (n)
  (declare (type unsigned-byte n))
  (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
```

Output
```
Return `n'th fibonacci number.

Arguments:
+ `n': nth fibonacci number
```

Example 2:
Input
```lisp
(defmacro do-bind-list ((var list &optional result) &body body)
  `(dolist (vars ,list ,result) (destructuring-bind ,var vars ,@body)))
```

Output
```
Iter over `list' and binds `var' list on each element of `list'.
Return `result' if given, or `nil'.

Syntax:

   (do-bind-list (([var]*) list [res]?) . body)

Example:

    (do-bind-list ((a b) list) (do-something-with a b))
```")

(defmacro refine-docstring (expr)
  "Refine docstring. "
  (if (and (listp expr)
           (or (eq (first expr) 'defun)
               (eq (first expr) 'defmacro)))
      (destructuring-bind (def name lambda-list docstring . body) expr
        `(,def ,name ,lambda-list
           ,(str:trim (chat-complete `((:content ,*prompt-lisp-docstring*
                                        :role "system")
                                       (:content ,(format nil "~A" expr)
                                        :role "user"))
                                     :temperature 0.2)
                      :char-bag '(#\` #\Space #\Newline))
           ,@(unless (stringp docstring) (list docstring))
           ,@body))
      expr))
