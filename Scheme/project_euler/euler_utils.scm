(declare (unit euler-utils))

;;
;; List utils
;;
           
(define (foldl step acc xs)
    (define (go-foldl i-acc i-xs)
        (if (null? i-xs)
            i-acc
            (go-foldl (step i-acc (car i-xs)) (cdr i-xs))))
        
    (go-foldl acc xs))

(define (filter pred xs) 
    (cond ((null? xs)
           '())
          ((pred (car xs))
           (cons (car xs) (filter pred (cdr xs))))
          (else 
           (filter pred (cdr xs)))))
    
(define (range start stop)
    (if (> start stop)
        '()
        (cons start (range (+ start 1) stop))))

(define (any pred xs) 
    (cond ((null? xs) #f)
          ((pred (car xs)) #t)
          (else (any pred (cdr xs)))))

(define (take-while pred xs)
    (cond ((null? xs) '())
          ((pred (car xs)) 
           (cons (car xs) (take-while pred (cdr xs))))
          (else '())))

(define (drop-while pred xs)
    (cond ((null? xs) '())
          ((pred (car xs)) 
           (drop-while pred (cdr xs)))
          (else xs)))
          
(define (take n xs)
    (if (or (null? xs) (zero? n))
        '()
        (cons (car xs) (take (- n 1) (cdr xs)))))

; Sort a list using the quicksort algorithm.
(define (qsort xs)
    (cond ((null? xs) 
           '())
          (else
           (let* ((pivot (car xs))
                  (lesser  (filter (lambda (x) (<  x pivot)) (cdr xs)))
                  (greater (filter (lambda (x) (>= x pivot)) (cdr xs))))
            
            (append (qsort lesser) (cons pivot (qsort greater)))))))
; Returns the unique elements from a list.
(define (nub xs)
    (define (go-nub current ixs)
        (cond ((null? ixs) 
               '())
              ((= current (car ixs))
               (go-nub current (cdr ixs)))
              (else
               (cons (car ixs) (go-nub (car ixs) (cdr ixs))))))

    (if (null? xs)
        '()
        (let ((sorted (qsort xs)))
         (cons (car sorted) (go-nub (car sorted) (cdr sorted))))))
         
; Returns true if both list are equals
(define (list-eq? xs ys)
    (cond ((null? xs) (null? ys))
          ((null? ys) (null? xs))
          (else
           (and (eq? (car xs) (car ys)) (list-eq? (cdr xs) (cdr ys))))))
         
; Retuns all possible permutations of xs
(define (permutations xs)
    (define (permute-with y permuts acc)
        (if (null? permuts)
            acc
            (permute-with y (cdr permuts) (cons (cons y (car permuts))
                                                acc))))

    (define (go-permutations ys zs acc)
        (if (null? ys)
            acc
            (let* ((y (car ys))
                   (inner-permuts (permutations (append (cdr ys) zs)))
                   (new-acc (permute-with y inner-permuts acc)))
             (go-permutations (cdr ys) (cons y zs) new-acc))))
         
    (cond ((null? xs) '())
          ((null? (cdr xs)) (list xs))  
          (else (go-permutations xs '() '()))))

; Returns true if xs is a permutation of ys
(define (permutation? xs ys)
    (list-eq? (qsort xs) (qsort ys)))

;;
;; Numbers utils
;;

; Returns true if y is a divisor of x.
(define (divides? x y)
    (= 0 (modulo x y)))

(define (square x)
    (* x x))

(define (factorial x)
    (define (go-factorial i acc)
        (if (zero? i)
            acc
            (go-factorial (- i 1) (* acc i))))
   
    (go-factorial x 1))

;;
;; Factorization utils
;;

; Returns the list of all factors of x (without 1) and their reciprocal.
(define (factors x)
    (define root-x (floor (sqrt x)))
    
    (define (go-factors i acc)
        (cond ((> i root-x) acc)
              ((divides? x i) 
               (go-factors (+ 1 i) (cons i (cons (/ x i) acc))))
              (else 
               (go-factors (+ 1 i) acc))))
               
    (define init-acc
        (if (= (square root-x) x) 
            (list x)
            '()))
    
    (go-factors 2 init-acc)) 
    
; Returns all primes from 2 to up-to (inclusive).
(define (primes up-to)
    (define (go-primes i primes-list)
        (let* ((root-i (floor (sqrt i)))
               (divide-i? (lambda (x) (divides? i x)))
               (larger-than-root? (lambda (x) (> x root-i)))
               (reversed-list 
                (reverse (drop-while larger-than-root? primes-list))))
         (cond ((> i up-to) primes-list)
               ((any divide-i? reversed-list)  ; factorizable with primes
                (go-primes (+ i 1) primes-list))
               (else  ; i is prime
                (go-primes (+ i 1) (cons i primes-list))))))
    
    (reverse (go-primes 2 '())))

; Returns the list of the differents prime factors of x.
(define (prime-factors x primes-list)
    (define (divides-with div y) ; Divides y with div while div divides x
        (if (divides? y div)
            (divides-with div (/ y div))
            y))
            
    (define (go-prime-factors y xs)
        (cond ((= y 1) '())
              ((null? xs) (error "Primes list too short"))
              ((divides? y (car xs))
               (let* ((factor (car xs))
                      (y-new (divides-with factor y)))
                (cons factor (go-prime-factors y-new (cdr xs)))))
              (else (go-prime-factors y (cdr xs)))))
    
    (go-prime-factors x primes-list))
    
; Returns true if x if prime by attempting a factorization using a primes list.
(define (prime? x primes-list)
    (define (divides-x? y)
        (divides? x y))
    
    (define root-x (floor (sqrt x)))
    
    (define (less-equals-root? y)
        (<= y root-x))
    
    (not (any divides-x? (take-while less-equals-root? primes-list))))
    
;; 
;; Digits utils
;;

; Returns a list of digits from x (most-signifiant ordered).
(define (digits x)
    (define (go-digits remain acc)
        (if (zero? remain) 
            acc
            (go-digits (quotient remain 10) (cons (modulo remain 10) acc))))
    
    (if (zero? x)
        (list 0)
        (go-digits x '())))

; Returns a number from a list of digits.
(define (from-digits ds)
    (define (go-from-digits i-ds acc)
        (if (null? i-ds)
            acc
            (let ((new-acc (+ (* acc 10) (car i-ds))))
             (go-from-digits (cdr i-ds) new-acc))))
    
    (go-from-digits ds 0))
    
;;
;; Arbitrary-precision calulus
;;

; Represents an arbitrary-precision number using a list of digits 
; (least-signifiant ordered).
(define (arb-number x)
    (reverse (digits x)))

; Returns the integral value from an arbitrary-precision number
(define (from-arb-number arb)
    (from-digits (reverse arb)))

; Arbitrary-precision addition.
(define (arb-add arb-a arb-b precision)
    (define (go-arb-add i-arb-a i-arb-b remain)
        (cond ((and (null? i-arb-a) (null? i-arb-b))
               (if (zero? remain)
                   '()
                   (list remain)))
              ((null? i-arb-a)
               (let* ((sum (+ (car i-arb-b) remain))
                      (val (modulo sum 10))
                      (new-remain (quotient sum 10)))
                (cons val (go-arb-add '() (cdr i-arb-b) new-remain))
               ))
              ((null? i-arb-b)
               (let* ((sum (+ (car i-arb-a) remain))
                      (val (modulo sum 10))
                      (new-remain (quotient sum 10)))
                (cons val (go-arb-add (cdr i-arb-a) '() new-remain))
               ))
              (else
               (let* ((sum (+ (car i-arb-a) (car i-arb-b) remain))
                      (val (modulo sum 10))
                      (new-remain (quotient sum 10)))
                (cons val 
                      (go-arb-add (cdr i-arb-a) (cdr i-arb-b) new-remain))
               ))))

    (take precision (go-arb-add arb-a arb-b 0)))

; Arbitrary-precision multiplication.
(define (arb-mult arb-a arb-b precision)
    ; Multiplies two digits and return the arbitrary encoded result.
    (define (digit-mult a b base) 
        (let* ((product (* a b))
               (quot (quotient product 10)))
         (if (zero? quot)
             (append base (list (modulo product 10)))
             (append base (list (modulo product 10) quot)))))
          
    (define (incr-base base)
        (cons 0 base))
        
    (define (add-acc arb-i acc)
        (arb-add arb-i acc precision))
        
    (define (go-arb-mult i-arb-a i-arb-b acc base-a base-b)
        (cond ((null? i-arb-b) acc) ; No more multiplier
              ((null? i-arb-a)      ; No more muliplicand, next multiplier
               (let ((new-base (incr-base base-b)))
                (go-arb-mult arb-a (cdr i-arb-b) acc new-base new-base)))
              (else                 ; Next muliplicand
               (go-arb-mult (cdr i-arb-a) i-arb-b
                              (add-acc (digit-mult (car i-arb-a) 
                                                   (car i-arb-b) base-a) 
                                       acc)
                              (incr-base base-a) base-b))))
    
    (take precision (go-arb-mult arb-a arb-b (arb-number 0) '() '())))

; Arbitrary-precision exponentiation
(define (arb-expt arb-base exponent precision)
    (define (go-arb-expt i-exponent)
        (cond ((zero? i-exponent) (arb-number 1))
              ((even? i-exponent) 
               (let ((root (go-arb-expt (quotient i-exponent 2))))
                (arb-mult root root precision)))
              (else
               (let ((prec (go-arb-expt (- i-exponent 1))))
                (arb-mult prec arb-base precision)))))
    
    (take precision (go-arb-expt exponent)))

;; (print "qsort: " (qsort (list 7 8 7 5 7 3 5 1 0 5 1 6 9 8 4 2 3 1 0 9)))
;; (print "nub: " (nub (list 7 8 7 5 7 3 5 1 0 5 1 6 9 8 4 2 3 1 0 9)))
;; (print "take-while: " (take-while even? (list 2 4 6 8 9 10 12 14)))
;; (print "drop-while: " (drop-while even? (list 2 4 6 8 9 10 12 14)))
;; (print "primes: " (length (primes 1000)))
;; (print "prime-factors: " (prime-factors 644 (primes 1000)))
;; (print "factors: " (factors 644))
;; (print "filter: " (filter even? (list 5 8 6 1 2 0 3)))
;; (print "digits: " (digits 545848))
;; (print "arb-number: " (arb-number 545848))
;; (print "arb-add: " (arb-add (arb-number 5448) (arb-number 2154) 100))
;; (print "arb-mult: " (arb-mult (arb-number 18) (arb-number 2) 100))
;; (print "arb-expt: " (arb-expt (arb-number 18) 5 2))
;; (print (range 1 10))