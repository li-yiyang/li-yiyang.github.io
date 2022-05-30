;;; Integer

(define (iquot a b) (quotient  a b))          ; MPL iquot(a, b)
(define (irem a b) (remainder a b))           ; MPL irem(a, b)


(define (gcd a b)                             ; Euclid GCD
  (if (= b 0)
    (abs a)                                   ;; gcd(a, 0) = abs(a)
    (gcd b (irem a b))))                      ;; gcd(a, b) = gcd(b, r)


(define (ext-gcd a b)                         ; Extended Euclid GCD

  (define (iter-ext-gcd a b mm nn m n)
    (if (= b 0)
      (if (> a 0)
        (list a mm nn)
        (list (- a) (- mm) (- nn)))
      (let ((q (iquot a b)))
         (iter-ext-gcd
           b
           (remainder a b)
           m
           n
           (- mm (* q m))                     ;; m = mm - q * m
           (- nn (* q n))))))                 ;; n = nn - q * n
  (iter-ext-gcd a b 1 0 0 1))                 ;; initial condition


(define (prime? n)                            ; Test if prime
  (cond
    ((= n 0) #f)
   	((= n 1) #f)
    ((= n 2) #t)
    ((even? n) #f)
    (else
      (let loop ((d 3))                       ;; iter from 3 to sqrt(n)
    	 (cond ((> (square d) n) #t)
    	       ((zero? (remainder n d)) #f)
    	       (else (loop (+ d 2))))))))


(define (ifactor n)                           ; integer factor

  (define (times n p k)                       ;; p^k | n
  	(if (not (= 0 (remainder n p)))
    	(list n p k)                          ;; return (n p k)
    	(times (quotient n p) p (+ 1 k))))
  
  (define (iter-ifactor n p)
    (cond
      ((= 1 n) '())                           ;; ends iteration
      ((prime? p)
       (let ((t (times n p 0)))
         (if (= 0 (car (cddr t)))
           	 (iter-ifactor n (+ 1 p))         ;; irem(n, p) != 0, try next
           	 (append                          ;; add (p k) pair into res
           	   (list (cdr t))
           	   (iter-ifactor
           	     (car t)
           	     (+ 1 p))))))
      (else (iter-ifactor n (+ 1 p)))))
  
  (iter-ifactor n 2))                         ;; test from 2


(define (chinese-remainder eqs)               ; Chinese Remainder
                                              ;; input ((x m) ...) 
   (if (null? (cdr eqs))
     (let  ((x (caar eqs))
            (m (car (cdr (car eqs)))))
       (list (remainder x m) m))              ;; return result
     (let* ((x1 (caar eqs))
            (m1 (car (cdr (car eqs))))
            (x2 (car (car (cdr eqs))))
            (m2 (car (cdr (car (cdr eqs)))))
            (egcd (ext-gcd m1 m2))
            (c (car (cdr egcd)))
            (d (car (cddr egcd)))
            (x (+ (* c m1 x2) (* d m2 x1)))
            (m (* m1 m2)))
       (chinese-remainder
         (append (list (list x m))
                 (cddr eqs))))))

;; Integer Exercise

;; floor
; (define (floor n)                           ; n = a / b
;   (iquot (car n) (car (cdr n))))            ;; (floor '(a b))

;; ceiling
; (define (ceiling n)                         ; n = a / b
;   (let ((a (car n))                         ;; (ceiling '(a b))
;         (b (car (cdr n))))
;     (if (= 0 (irem a b))
;         (iquot a b)
;         (+ (iquot a b)))))

(define (integer-divisors n)                  ; integer divisors of n
  (define (iter t)                            ;; not the best algorithm
    (cond
      ((> (square t) n)                       ;; end
       '())
      ((= (irem n t) 0)                       ;; is the divisor
       (append
         (list t (- t)
               (iquot n t) (- (iquot n t)))
         (iter (+ 1 t))))
      (else (iter (+ 1 t)))))

  (iter 1))

(define (base-rep n b)                        ; base replace
  (if (= 0 n)                                 ;; convert n to base b
    '()
    (append (list (irem n b))
            (base-rep (iquot n b) b))))

(define (lcm a b) (/ (* a b) (gcd a b)))      ; lcm

(define (ext-lcm a b)                         ; ext-lcm
  (let* ((egcd (ext-gcd a b))
         (d (car egcd))
         (u (car (cdr egcd)))
         (v (car (cddr egcd)))
         (k (iquot a d))
         (t (iquot b d)))
    (list (/ (* a b) d)
          (* t k u)
          (* t k v))))

;; Rational Number
;; number type not down yet...

(define (frac a b)                            ; MPL FracOp
  (let ((d (gcd a b)))
    (if (> d 0)
      (list 'frac (/ a d) (/ b d))
      (list 'frac (/ (- a) d) (/ (- b) d)))))

(define-syntax sum
  (syntax-rules ()
    ((_) 0)
    ((_ e0)
     (if (integer? e0) (frac e0 1) e0))
    ((_ e1 e2 ...)
     (if (integer? e1)
       (list 'add (frac e1 1) (sum e2 ...))
       (list 'add e1 (sum e2 ...))))))

(define (tofrac int)                          ; convert int to frac
  (if (integer? int)
    (list int 1)
    int))



