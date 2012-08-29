; Returns true if y is a divisor of x.
(define (divides? x y)
    (= 0 (modulo x y)))
    
(define (filter pred xs) 
    (cond ((null? xs)
            '())
          ((pred (car xs))
            (cons (car xs) (filter pred (cdr xs))))
          (else 
            (filter pred (cdr xs)))))

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
          
; Sort a list using the quicksort algorithm.
(define (qsort xs)
    (cond ((null? xs) 
            '())
          (else
            (let* ((pivot (car xs))
                   (lesser  (filter (lambda (x) (<  x pivot)) (cdr xs)))
                   (greater (filter (lambda (x) (>= x pivot)) (cdr xs))))
            
             (append (qsort lesser) (cons pivot (qsort greater)))))))

; Returns the list of all factors of x (without 1) and their reciprocal.
(define (factors x)
    (define square-x (floor (sqrt x)))
    (define (factors-go i acc)
        (cond ((> i square-x)
                    acc)
              ((divides? x i) 
                    (factors-go (+ 1 i) (cons i (cons (/ x i) acc))))
              (else 
                    (factors-go (+ 1 i) acc))))
    (define init-acc
        (if (= (* square-x square-x) x) 
            (list x)
            '()))
    
    (factors-go 2 init-acc))
    
; Returns all primes from 2 to up-to.
(define (primes up-to)
    (define (primes-go i primes-list)
        (let* ((square-i (floor (sqrt i)))
               (divide-i? (lambda (x) (divides? i x)))
               (larger-than-square? (lambda (x) (> x square-i)))
               (reversed-list 
                    (reverse (drop-while larger-than-square? primes-list))))
         (cond ((> i up-to) primes-list)
               ((any divide-i? reversed-list)
                    (primes-go (+ i 1) primes-list)) ; factorizable with primes
               (else 
                    (primes-go (+ i 1) (cons i primes-list)))))) ; i is prime
    
    (reverse (primes-go 2 '())))

; Returns the list of the differents prime factors of x
(define (prime-factors x primes-list)
    (define (divides-with div y) ; Divides y with div while div divides x
        (if (divides? y div)
            (divides-with div (/ y div))
            y))
    (define (prime-factors-go y xs)
        (cond ((= y 1) '())
              ((null? xs) (error "Primes list too short"))
              ((divides? y (car xs))
                (let* ((factor (car xs))
                      (y-new (divides-with factor y)))
                 (cons factor (prime-factors-go y-new (cdr xs)))))
              (else (prime-factors-go y (cdr xs)))))
    
    (prime-factors-go x primes-list))

; Returns the unique elements from a list.
(define (nub xs)
    (define (nub-go current ixs)
        (cond ((null? ixs) 
                '())
              ((= current (car ixs))
                (nub-go current (cdr ixs)))
              (else
                (cons (car ixs) (nub-go (car ixs) (cdr ixs))))))

    (if (null? xs)
        '()
        (let ((sorted (qsort xs)))
         (cons (car sorted) (nub-go (car sorted) (cdr sorted))))))
         
(define (problem-47 n-factors)
    (define primes-list (primes 150000))
    (define (problem-47-go i n-prec)
        (if (= n-factors (length (prime-factors i primes-list)))
            (if (= n-prec (- n-factors 1))
                (- i (- n-factors 1))
                (problem-47-go (+ i 1) (+ n-prec 1)))
            (problem-47-go (+ i 1) 0)))
            
    (problem-47-go 2 0))
    
(print (problem-47 4))

;; (print "qsort: " (qsort (list 7 8 7 5 7 3 5 1 0 5 1 6 9 8 4 2 3 1 0 9)))
;; (print "nub: " (nub (list 7 8 7 5 7 3 5 1 0 5 1 6 9 8 4 2 3 1 0 9)))
;; (print "take-while: " (take-while even? (list 2 4 6 8 9 10 12 14)))
;; (print "drop-while: " (drop-while even? (list 2 4 6 8 9 10 12 14)))
;; (print "primes: " (length (primes 1000)))
;; (print "prime-factors: " (prime-factors 644 (primes 1000)))
;; (print "factors: " (factors 644))
;; (print "filter: " (filter even? (list 5 8 6 1 2 0 3)))