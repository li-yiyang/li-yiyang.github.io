(defpackage q-m-simplify
  (:use :cl)
  (:export :simplify-logic :reduce-logic)
  (:documentation "Use Q-M method to reduce the logic expression into AND/OR gate."))

(in-package q-m-simplify)

(defun ∧ (&rest in-s)
  "Logic AND for IN-S."
  (labels ((iter (in)
             (if (car in)
                 (if (eq (car in) 0) 0
                     (iter (rest in)))
                 1)))
    (iter in-s)))

(defun ∨ (&rest in-s)
  "Logic OR for IN-S."
  (labels ((iter (in)
             (if (car in)
                 (if (eq (car in) 1) 1
                     (iter (rest in)))
                 0)))
    (iter in-s)))

(defun ¬ (in)
  "Logic NOT for IN."
  (if (eq in 0) 1 0))

(defun ↑ (A B)
  "NAND A and B. "
  (¬ (∧ A B)))

(defun to-bits (num length &optional (base 2))
  "Turn NUM into LENGTH bits list in BASE (default to binary)."
  (let ((res '()))
    (dotimes (i length)
      (push (mod num base) res)
      (setq num (floor (/ num base))))
    res))

(defun to-num (bits &optional (base 2))
  "Turn BITS into number in BASE (default to binary)."
  (let ((acc 1)
        (num 0))
    (loop for val in (reverse bits)
          do (setq num (+ num (* val acc))
                          acc (* acc base)))
    num))

(defun logic-exp-eval (exp pattern)
  "Replace SYM in EXP by VAL. PATTERN like '((SYM VAL)). "
  (let ((exp-val exp))
    (loop for sym-val in pattern
          do (destructuring-bind (sym val) sym-val
               (setq exp-val (subst val sym exp-val))))
    (eval exp-val)))

(defun truth-table (exp vars)
  "Generate Truth Table of EXP using VARS. "
  (let ((length (length vars))
        (table  `((,vars VAL))))
    (dotimes (idx (expt 2 length))
      (let* ((pattern (to-bits idx length))
             (val-pattern (mapcar (lambda (sym val) (list sym val)) vars pattern))
             (val (logic-exp-eval exp val-pattern)))
        (push `(,pattern ,val) table)))
    (reverse table)))

(defun to-minterm (table)
  "Trun truth table TABLE into minterm expression."
  (let* ((vars (caar table))
         (body (rest table))
         (minterms (loop for row in body
                         when (eq (cadr row) 1)
                           collect (to-num (car row))))
         (length (length minterms)))
    (cond ((eq length 0) 0)
          ((eq length 1) `(minterm (quote ,(car minterms)) (quote ,vars)))
          (t `(minterm (quote ,minterms) (quote ,vars))))))

(defun minterm (patterns vars)
  "Make logic expressions by PATTERNS using VARS.

PATTERNS can be a list: `(minterm '(1 2) '(A B))';
or could be a single item: `(minterm 1 '(A B))'. "
  (let ((length (length vars)))
    (labels ((single (num)
               (cons '∧ (mapcar (lambda (sym val)
                                  (if (eq val 0) `(¬ ,sym) sym))
                                vars (to-bits num length)))))
      (if (atom patterns)
          (single patterns)
          (cons '∨ (mapcar #'single patterns))))))

(defun count-1 (bins)
  "Count `1' number in BINS."
  (let ((sum 0))
    (loop for val in bins
          when (eq val 1)
            do (setq sum (1+ sum)))
    sum))

(defun strip-zero-and-format (table)
  "Read truth table TABLE and produce a formatted output for further process.

For example: `(strip-zero-and-format (truth-table LOGIC-EXP VARS))'. "
  (let* ((vars (caar table))
         (body (rest table)))
    (labels ((val (row) (cadr row))
             (bin (row) (car row)))
      (cons `(GROUP MINTERM ,vars)
            (loop for row in body
                  when (eq (val row) 1)
                    collect
                    (let ((bins (bin row)))
                      (list (count-1 bins) (to-num bins) bins)))))))

(defun format-by-minterm-exp (minterm)
  "Input MINTERM expression: `(minterm 'MINTERMS 'VARS)', generate formatted table.

For example: `(format-by-minterm-exp '(minterm '(0 1) '(A B)))'. "
  (destructuring-bind (- (- minterms) (- vars)) minterm
    (cons `(GROUP MINTERM ,vars)
          (loop for term in (if (atom minterms) (list minterms) minterms)
                collect (let ((bins (to-bits term (length vars))))
                          (list (count-1 bins) term bins))))))

(defun assoc-update (key obj alist)
  "Insert OBJ into ALIST at KEY position."
  (if (assoc key alist)
      (push obj (cdr (assoc key alist))) ; push to existed key
      (push (cons key (list obj)) alist)) ; add new key
  alist)

(defun group-by (func lst)
  "Group items in LST by the value of FUNC. Return a AList."
  (let ((groups '()))
    (loop for item in lst               ; kinda like bucket-sort
          do (setq groups (assoc-update (funcall func item) item groups)))
    groups))

(defun group-formatted (formatted)
  "Group formatted truth table FORMATTED.

For example: `(group-formatted (format-by-minterm-exp MINTERM))'. "
  (let ((vars (nth 2 (car formatted)))
        (body (rest formatted)))
    (cons `(GROUP MINTERMS ,vars) (group-by #'first body))))

(defun grouped-and-format-output (grouped)
  "Input table with `(GROUP MINTERM (VALS))' pattern, output grouped one.

For example: `(grouped-and-format-output (group-formatted FORMATTED))'. "
  (let ((title (car grouped))
        (grouped (rest grouped))
        (res '()))
    (loop for group in grouped          ; for all groups
          do (loop for row in (cdr group) ; push items to output table
                   do (push row res)))
    (cons title res)))

(defun compare-bins (bins-a bins-b)
  "Compare BINS-A and BINS-B, return marked bins or NIL for fail.

For example: 
+ `(compare-bins '(0 0 1) '(0 0 0))' returns `(0 0 -)'
+ `(compare-bins '(0 1 1) '(0 0 0))' returns `NIL'."
  (let* ((count-diff 0)
         (tape (loop for a in bins-a
                     for b in bins-b
                     while (< count-diff 2)
                     if (eq a b)
                       collect a
                     else
                       collect '-
                       and do (setq count-diff (1+ count-diff)))))
    (if (eq count-diff 1)
        tape
        NIL)))

(defun compare-two-group (n g-n g-n+1)
  "Compare two group G-N and G-N+1.

The input two group are list of bins, for example: `((m1 (0 0 1)) (m2 (0 1 0)))'. 
The output are like `(((m1 m2) (1 0 -)) ((m3 m4) (0 - 0)))'. 

The input two group can also be like `(((m1 m2) (- 0 1)) ((m1 m2) (0 - 0)))',
which will be passed in Step 1.3 for further simplify. "
  (labels ((minterm-list (m-pattern)
             (let ((m (car m-pattern))) (if (atom m) (list m) m))))
    (let ((compare-res '()))
      (loop for m-a in g-n
            do (loop for m-b in g-n+1
                     do (let* ((m1 (minterm-list m-a)) (bin-1 (cadr m-a))
                               (m2 (minterm-list m-b)) (bin-2 (cadr m-b))
                               (cmp (compare-bins bin-1 bin-2)))
                          (when cmp
                            (push (list n (append m1 m2) cmp) compare-res)))))
      compare-res)))

(defun compare-grouped-nexts (grouped)
  "Compare the GROUPED input.

The input will be like `((GROUP MINTERM VARS) GROUPED-ALIST)'. "
  (let* ((vars (nth 2 (car grouped)))
         (body (rest grouped))
         (length (length vars)))
    (labels ((content (group)
               (mapcar (lambda (lst) (cdr lst)) (cdr group))))
      (let ((n-group NIL)
            (n+1-group NIL)
            (compare NIL))
        (cons
         `(GROUP MINTERMS ,vars)
         (loop for n from 0 to (1- length)
               if (and (setq n-group (assoc n body))
                       (setq n+1-group (assoc (1+ n) body))
                       (setq compare (compare-two-group n
                                                        (content n-group)
                                                        (content n+1-group))))
                 collect (append (list n) compare)))))))

(defun compared-group-format (compared-group)
  "Format the COMPARED-GROUP for output."
  (let ((title (first compared-group))
        (body (rest compared-group))
        (res '()))
    (loop for group in body
          do (loop for pattern in (cdr group)
                   do (push pattern res)))
    (cons title (reverse res))))

(defun find-prime-impilcant-of-formatted (grouped)
  "Find the Prime Impilcant of GROUPED. "
  (labels ((iter (formatted-input)
             (let ((res (compare-grouped-nexts formatted-input)))
               (if (rest res)           ; ALIST of formatted
                   (iter res)
                   formatted-input))))
    (iter grouped)))

(defun restore-minterms (prime-implicants)
  "Restore minterms from PRIME-IMPLICANTS. "
  (let ((body (rest prime-implicants))
        (minterm-lst '()))
    (labels ((read-minterms (pattern)
               (let ((m (nth 1 pattern)))
                 (if (atom m) (list m) m))))
      (loop for group in body
            do (loop for pattern in (cdr group)
                     do (loop for m in (read-minterms pattern)
                              do (when (not (find m minterm-lst))
                                   (push m minterm-lst)))))
      (sort minterm-lst #'<))))

(defun construct-prime-implicant-table (prime-implicants)
  "Format the PRIME-IMPLICANTS, for convenice"
  (let ((vars (nth 2 (first prime-implicants)))
        (body (rest prime-implicants))
        (minterm-lst (restore-minterms prime-implicants))
        (minterm-read '()))
    (labels ((make-exp (pattern)
               (let ((vals (nth 2 pattern)))
                 (when (not (find-if (lambda (elem) (equal elem vals))
                                     minterm-read))
                   (push vals minterm-read)
                   (cons '∧
                         (loop for val in (nth 2 pattern)
                               for sym in vars
                               if (not (eq val '-))
                                 collect (if (eq val 0) `(¬ ,sym) sym))))))
             (read-minterms (pattern)
               (let* ((minterms (nth 1 pattern))
                      (min (if (atom minterms) (list minterms) minterms)))
                 (mapcar (lambda (m) (if (find m min) 'X '_))
                         minterm-lst))))
      (let ((res '()))
        (loop for group in body
              do (loop for pattern in (cdr group)
                       do (let ((minterms (read-minterms pattern))
                                (logic-exp (make-exp pattern)))
                            (when logic-exp
                              (push (cons logic-exp minterms) res)))))
        (cons (cons 'LOGIC-EXP minterm-lst)
              res)))))

(defun transpose-table (table)
  "Transpose input TABLE."
  (apply #'mapcar #'list table))

(defun find-essential-prime-implicants (trans-table)
  "Find essential parts in TRANS-TABLE. "
  (let* ((exps (rest (first trans-table)))
         (rows (rest trans-table)))
    (labels ((count-X (row)
               (let ((count 0)
                     (expr NIL))
                 (loop for val in (rest row)
                       for exp in exps
                       while (< count 2)
                       if (eq val 'X)
                         do (setq count (1+ count)
                                  expr exp))
                 (if (eq count 1) expr NIL))))
      (let ((count NIL)
            (essential '()))
        (loop for row in rows
              do (setq count (count-X row))
              if count
                do (when (not (find-if (lambda (elem) (equal elem count))
                                       essential))
                     (push count essential)))
        essential))))

(defun remove-essential-prime-implicants (table)
  "Delete essential primes of TABLE, return removed table and essential ones."
  (let* ((trans-table (transpose-table table))
         (essential (find-essential-prime-implicants trans-table))
         (title (first trans-table))
         (exps (rest title))
         (rows (rest trans-table))
         (remain-rows '()))
    (loop for row in rows
          if (let ((remain-p T))
               (loop for val in (rest row)
                     for exp in exps
                     if (and (eq val 'X)
                             (find-if (lambda (elem) (equal elem exp))
                                      essential))
                       do (setq remain-p NIL))
               remain-p)
            do (push row remain-rows))
    (values (delete-if (lambda (row)
                         (find-if (lambda (elem) (equal elem (first row)))
                                  essential))
                       (transpose-table (cons title (reverse remain-rows))))
            essential)))

(defun remove-dominate (table)
  "Remove dominate elements in TABLE. "
  (let ((title (first table))
        (body  (rest table)))
    (labels ((detect-dominate (the-row)
               (let ((the-id (first the-row))
                     (the-content (rest the-row))
                     (dominate? T)
                     (found NIL))
                 (loop for row in body
                       while (not found)
                       if (not (equal (first row) the-id))
                         do (progn
                              (setq dominate? T)
                              (loop for elem-a in (rest row)
                                    for elem-b in the-content
                                    while dominate?
                                    do (progn
                                         (setq dominate?
                                               (not (and (eq elem-b '_)
                                                         (eq elem-a 'X))))
                                         (setq found dominate?)))))
                 dominate?)))
      (loop for row in (sort (rest table) #'exp-leq)
            if (detect-dominate row)
              do (setq body (delete-if (lambda (r) (equal r row)) body)))
      (cons title body))))

(defun exp-greater (a b)
  "Compare expressions A and B if A > B. "
  (cond ((and (atom a) (atom b))
         (if (and (symbolp a) (symbolp b)) NIL (> a b)))
        ((and (not (atom a)) (atom b)) T)
        ((and (atom a) (not (atom b))) NIL)
        (t (if (> (length a) (length b))
               T
               (or (exp-greater (nth 1 a) (nth 1 b))
                   (exp-greater (nth 2 a) (nth 2 b)))))))

(defun exp-leq (a b)
  "Compare expressions A and B is A ≤ B. "
  (not (exp-greater a b)))

(defun simplify-formatted (formatted)
  "Simplify the formatted input FORMATTED. 

Note: the FORMATTED could be:
+ `strip-zero-and-format';
+ `format-by-minterm-exp'."
  (multiple-value-bind (removed essential)
      (remove-essential-prime-implicants
       (construct-prime-implicant-table
        (find-prime-impilcant-of-formatted
         (group-formatted formatted))))
    (cons '∨
          (append essential
                  (rest (first
                         (transpose-table
                          (remove-dominate
                           (transpose-table
                            (remove-dominate
                             (transpose-table removed)))))))))))

(defun exp-latex-format (exp)
  "Export EXP to LaTeX format. "
  (labels ((2-to-latex (op)
             (lambda (a b)
               (concatenate 'string
                            (exp-latex-format a)
                            op
                            (exp-latex-format b)))))
    (if (atom exp)
        (string exp)
        (let ((op (first exp)))
          (cond ((eq op '∧) (reduce (2-to-latex " ") (rest exp)))
                ((eq op '∨) (reduce (2-to-latex " + ") (rest exp)))
                ((eq op '¬)
                 (concatenate 'string
                              "\\bar{" (exp-latex-format (nth 1 exp)) "}")))))))

(defun reduce-logic (exp)
  "A Simple Logic Exp Reduce function."
  (cond ((atom exp) exp)
        ((= 2 (length exp))
         (case (car exp)
           (∧ (reduce-logic (second exp)))
           (∨ (reduce-logic (second exp)))
           (otherwise exp)))
        (T (cons (car exp) (mapcar #'reduce-logic (cdr exp))))))

(defmacro simplify-logic ((&rest var) exp)
  "Simplify the Logic Expression."
  `(simplify-formatted (strip-zero-and-format (truth-table ',exp ',var))))
