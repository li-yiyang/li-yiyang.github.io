;;; SEM-Calculate Package
(defpackage :sem-calculate
  (:use :cl))

;;; The following code are in SEM-Calculate Package
(in-package :sem-calculate)

(defun discreate-integrate (points &key (method 'trapezoid))
  "Discreat integrate the POINTS, which would be like a list of points.

Methods are `rectangle', `trapezoid' 
or other possible function for `point1', `point2'.
Points are like `(x . y)'.

+ rectangle                + trapezoid

  |             +            |          ...+
  |     +-------|            |     +#######|
  |     |#######|            |    /|#######|
  |     |#######|            |   /#|#######|
  | +---|#######|            |  +##|#######|
  +---------------           +---------------

or if you provide a custom function, it should calculate the
area defined by two points."
  (labels ((rectangle (point1 point2)
             (let ((x1 (car point1)) (y1 (cdr point1))
                   (x2 (car point2)))
               (* y1 (- x2 x1))))
           (trapezoid (point1 point2)
             (let ((x1 (car point1)) (y1 (cdr point1))
                   (x2 (car point2)) (y2 (cdr point2)))
               (/ (* (+ y2 y1) (- x2 x1)) 2)))
           (iter (points method)
             (if (> (length points) 1)
                 (+ (funcall method (first points) (second points))
                    (iter (rest points) method))
                 0)))
    (iter points (cond ((eq method 'rectangle) #'rectangle)
                       ((eq method 'trapezoid) #'trapezoid)
                       (t method)))))

(defun discreate-integrate-dx (fs dx &key (sample 2))
  "Discreate integrate of POINTS using SAMPLE number."
  (let ((series
          (cond ((eq sample 1) '(1))
                ((eq sample 2) '(1/2    1/2))
                ((eq sample 3) '(1/3    4/3     1/3))
                ((eq sample 4) '(3/8    9/8     9/8     3/8))
                ((eq sample 5) '(14/45  64/45   8/15    64/45   14/45))
                ((eq sample 6) '(95/288 375/288 250/288 250/288 375/288 95/288))
                (t              (throw 'user "Unsupported sample.")))))
    (labels ((iter (points)
               (if (>= (length points) sample)
                   (+ (reduce
                       #'+
                       (mapcar (lambda (k f) (* k f dx)) series
                                      (subseq points 0 sample)))
                      (iter (nthcdr (1- sample) points)))
                   0)))
      (iter fs))))

(defun discreate-differential (point1 point2)
  "Discreate differential of two points POINT1 and POINT2."
  (let ((y1 (cdr point1)) (x1 (car point1))
        (y2 (cdr point2)) (x2 (car point2)))
    (/ (- y2 y1) (- x2 x1))))

(defun demagnification-rate (&rest lens)
  "Given a set of LENS, return the demagnification rate of the crossover.

The calculation equation is described below when object distance is large enough:

           f1 * f2 * ... * fn
    d0 = ---------------------- * dc = m * dc
           L1 * L2 * ... * Ln

where the `m' is the demagnification rate, `dc' is the crossover of the gun,
`d0' is the final electron probe diameter (with 0 rank aberration). "
  (let ((focal-length   (mapcar #'len-focal-length    lens))
        (large-distance (mapcar #'len-work-distance lens)))
    (reduce #'* (mapcar (lambda (f L) (/ f L))
                        focal-length
                        large-distance))))

(defun find-opt-heating-power-point (data threshold)
  "Try to find turning point in DATA where differential is less than THRESHOLD.

Return value are the first point found with T or last point when not found with NIL."
  (if (< (length data) 2)
      (values (car (first data)) NIL)
      (let* ((point1   (first data))
             (point2   (second data))
             (diff-abs (abs (discreate-differential point1 point2))))
        (if (<= diff-abs threshold)
            (values (car point1) T)
            (find-opt-heating-power-point (rest data) threshold)))))

(defun measure-gun-brightness-two-diaphragm (diameter1 diameter2 length current)
  "Measure gun brightness with DIAMETER1, DIAMETER2, LENGTH and CURRENT.

The gun brightness is measured by:

       jc        ΔI           π * d1^2         π * d2^2
  β = ---- = ---------, ΔS = ----------, ΔΩ = ----------
       ΔΩ     ΔS * ΔΩ            4              4 * l^2

where d1 is DIAMETER1, d2 is DIAMETER2, l is LENGTH, ΔI = CURRENT."
  (let ((ΔS (/ (* pi diameter1 diameter1) 4))
        (ΔΩ (/ (* pi diameter2 diameter2) (* 4 length length))))
    (/ current (* ΔS ΔΩ))))

;;; To measure a len:
;;;   + Focal Length
;;;   + Object Distance
;;;   + Working Distance
;;;   + diaphragm-radius
(defstruct len focal-length object-distance working-distance diaphragm-radius)

(defun calculate-focal-length-by (bz-points acc-U)
  "Provide BZ-POINTS on axial and accelerate voltage ACC-U (in SI Unit).

The focal length is calculated by:

    1         e         + ∞   
   --- = ----------- * ∫ Bz² dz
    f     8 * m * U   - ∞

to approximate, integration will only integrate at the provide BZ-POINTS,
which means BZ-POINTS shall be prefixed with a start and end or the start
and end point value is small enough to omit.

All the parameters provided should be in SI Unit. 
     e                                        C
The --- having the value: 1.758820024x10⁺¹¹ -----.
     m                                        kg

The functions returns f."
  (let* ((bz-square (mapcar (lambda (point) (cons (car point)
                                                  (* (cdr point) (cdr point))))
                            bz-points))
         (int-bz-square (discreate-integrate bz-square))
         (charge-to-mass-ratio 1.758820024E+11))
    (/ 1 (* int-bz-square (/ charge-to-mass-ratio
                             (* 8 acc-U))))))

(defun measure-len-by-demagnification-rate (m len)
  "Return a new len by LEN parameters maximizing demagnification rate M. 

The working distance L (or w) is calculated by L = f / m, 
where f is the focal length, m is the demagnification rate."
  (let* ((new (copy-structure len))
         (mR  (if (< 1 m) (/ 1 m) m))
         (f   (len-focal-length new))
         (L   (/ f mR)))
    (setf (len-object-distance new) L)
    new))

(defun measure-lens-by-demagnification-rate (m &rest lens)
  "Return a list of new LENS to archive the demagnification rate M.

Since the demagnification rate m is calculated by:

       f1 * f2 * ... * fn
  m = --------------------
       L1 * L2 * ... * Ln

if there's only one (object) len, its object length shall be f / L, 
or if there's more than one lens, they shall archieve the minimum aberraation."
  (let ((len (first lens)))
    (if (null (rest lens))
        (list (measure-len-by-demagnification-rate m len))
        (cons (measure-len-by-aberration len)
              (apply #'measure-lens-by-demagnification-rate (cons next-m (rest lens)))))))

(defun electron-probe-aperture (len)
  "The electron probe aperture ɑₚ is calculated by r/f."
  (let ((r (diaphram-radius-of len))
        (f (focal-length-of    len)))
    (/ r f)))
