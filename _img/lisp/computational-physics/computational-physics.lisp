(defpackage #:computational-physics
  (:use :cl :gurafu)
  (:nicknames :phy))

(in-package :phy)

(defun p-base-integer (p bits)
  "Trun a list of `p' base bits into integer.
Return an integer crossponding to `bits'.

  (a_n a_{n-1} ... a_1 a_0) = sum a_i p^i

For example:

  (p-base-integer '(0 0 1 1) 2) => 3
"
  (flet ((scanner (int bit)
           (assert (< -1 bit p))
           (+ (* p int) bit)))
    (reduce #'scanner bits :initial-value 0)))

(defun integer-p-base (p integer &optional bits)
  "Trun `interger' to a list of `p' base bits.
Return a list of `p' base bits.
"
  (if (zerop integer) bits
      (integer-p-base p (floor integer p) (cons (mod integer p) bits))))

(defun p-base-decimal (p bits)
  "Trun a list of `p' base decimal bits into a decimal number.
Return a float number crossponding to `bits' [0, 1).

  (b_1 b_2 ... b_n) = sum b_i p^{-i}

For example:

  (p-base-decimal '(1) 4) => 0.25
"
  (flet ((scanner (int bit) (+ (float (/ int p)) bit)))
    (float
     (/ (reduce #'scanner (reverse bits) :initial-value 0) p))))

(defun decimal-p-base (p decimal &optional bits)
  "Trun a decimal to `p' base bits.
Return a p base bits list.
"
  (if (zerop decimal) bits
      (let ((shifted (* p decimal)))
        (decimal-p-base p (mod shifted 1.0)
                        (cons (truncate (mod shifted p)) bits)))))

(defun p-base-real (p int-bits dec-bits)
  "Return a `p' base real number for `int-bits' and `dec-bits'.

  real = integer + decimal
"
  (+ (p-base-integer p int-bits)
     (p-base-decimal p dec-bits)))

(defun from-ieee-float (sign exponent fraction
                        &optional
                          (offset (1- (expt 2 (1- (length exponent)))))
                          (max-exponent (1- (expt 2 (length exponent)))))
  "Trun a IEEE float to float. "
  (let* ((sign     (if (zerop sign) 1 -1))         
         (exponent (p-base-integer 2 exponent))
         (fraction (p-base-decimal 2 fraction))
         (type (cond ((= exponent max-exponent)
                      (if (zerop fraction)
                          (if (> sign 0)
                              :positive-infinity
                              :negative-infinity)
                          :nan))
                     ((zerop exponent)
                      (if (zerop fraction) :zero :subnormal))
                     (t :normal))))
    (values (if (eq type :normal)
                (float (* sign (expt 2 (- exponent offset)) (1+ fraction)))
                (float (* sign (expt 2 (- 1 offset)) fraction)))
            type)))

(defun ieee-float (bits)
  "Return float from IEEE 754 float `bits'.

  HIGH                 LOW
  sign exponent fraction
   0   1       9       32
"
  (let ((sign (first bits))
        (exponent (subseq bits 1 9))
        (fraction (subseq bits 9 32)))
    (from-ieee-float sign exponent fraction 127 255)))

(defun ieee-half-float (bits)
  "Return IEEE 754 half float from `bit'

  HIGH                 LOW
  sign exponent fraction
  0    1        6      16
"
  (let ((sign (first bits))
        (exponent (subseq bits 1 6))
        (fraction (subseq bits 6 16)))
    (from-ieee-float sign exponent fraction 15 31)))

(defun richardson-extrapolation (fn &optional (k 1))
  "Apply `k' rank Richardson Extrapolation on function `fn'. "
  (declare (type (integer 0) k))
  (if (zerop k) fn
      (let ((2expk (ash 2 k)))
        (lambda (h)
          (/ (- (* 2expk (funcall fn (/ h 2.0d0))) (funcall fn h))
             (1- 2expk))))))

(defun make-hs-matrix (hs)
  "Make H matrix for hs.
Return a n*n array (matrix).

  H { f^{(i)}(x) } = { f(x + h_i) - f(x) }
"
  (let ((n (length hs)))
    (flet ((hw (h)
             (loop for i below n
                   for hi = h then (* hi h)
                   for i! = 1 then (* i! i)
                   collect (float (/ hi i!)))))
      (make-array (list n n) :initial-contents (mapcar #'hw hs)))))

(defun make-fs-matrix (n)
  "Make a F matrix for `n' f(x + h_i).
Return a (n+1)*n matrix.

  F { f(x), f(x + h_i) } = { f(x + h_i) - f(x) }
"
  (let ((f-mat (make-array (list n (1+ n)) :initial-element 0)))
    (loop for j below n do
      (setf (aref f-mat j (1+ j)) 1     ; f_{i+1,j} = 1
            (aref f-mat j 0)      -1))  ; f_{0,j}   = -1
    f-mat))

(defun make-finite-differentor-matrix (hs)
  "Make a matrix for finite differentor calculation.

  M = H^{-1} F; M { f(x), f(x + h_i) } = { f^{(i)}(x) }
"
  (let* ((n (length hs))
         (mat-h (make-hs-matrix hs))
         (mat-f (make-fs-matrix n)))
    (lla:mm (lla:invert mat-h) mat-f)))

(defun make-finite-differentor (hs &optional (dx 1e-3) (d 1))
  "Make a finite differentor with `hs'. "
  (let ((rank (1- d)))
    (lambda (fn &optional (dx dx))
      (let* ((hs (mapcar (lambda (h) (* h dx)) hs))
             (mat (make-finite-differentor-matrix hs))
             (hs* (cons 0 hs)))
        (lambda (x)
          (flet ((f (h) (funcall fn (+ x h))))
            (aref (lla:mm mat (map 'vector #'f hs*)) rank)))))))

(defmacro finite-diff ((&rest hs) &key (dx 1e-3) (d 1))
  "Wrapper for `make-finite-differentor'. "
  `(make-finite-differentor (list ,@hs) ,dx ,d))

(defparameter first-forward (finite-diff (1))
  "First forward different.

  f'(x) = (f(x + h) - f(x)) / h")

(defparameter first-backward (finite-diff (-1))
  "First backward different.

  f'(x) = (f(x) - f(x - h)) / h")

(defparameter first-central (finite-diff (-1 1))
  "First central different.

  f'(x) = (f(x + h) - f(x - h)) / (2 * h)")

(defun 2-fn-rms-error (f1 f2 xmin xmax &optional (dx 0.1))
  (flet ((square (x) (* x x)))
    (loop for x from xmin to xmax by dx
          collect (square (- (funcall f1 x) (funcall f2 x))) into sum
          finally (return (sqrt (reduce #'+ sum))))))

(defun integrate (fn xmin xmax method &optional (dx 1e-3))
  "Integrate function `fn' from `xmin' to `xmax' with `method'. "
  (loop with integrate = 0
        for x from xmin upto xmax by dx
        do (incf integrate (funcall method fn x dx))
        finally (return integrate)))

(defun midpoint-rule (fn x 2h)
  "Integarate function `fn' from `xmin' to `xmax'. "
  (* 2h (funcall fn (+ x (/ 2h 2)))))

(defun simpson-rule (fn x 2h)
  "Integrate function `fn' from `xmin' to `xmax'. "
  (* 2h 1/2 1/3
     (+ (funcall fn x)
        (* 4 (funcall fn (+ x (/ 2h 2))))
        (funcall fn (+ x 2h)))))
