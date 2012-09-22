(declare (uses euler-utils))

(define (problem-50 up-to)
    (define primes-list (primes up-to))
    
    (define (find-length valid-primes i max-length cur-sum cur-length)
        (cond ((> cur-sum i) max-length) ; exceeds the value of the prime
              ((= cur-sum i) ; the sum reachs the value of the prime
               (max cur-length max-length)) 
              (else (find-length (cdr valid-primes) i max-length 
                                 (+ (car valid-primes) cur-sum) 
                                 (+ 1 cur-length)))))

    (define (find-max-length valid-primes i max-length) 
        (let ((min-length (+ max-length 1))) ; Minimum length needed to increase
                                             ; the score
         ; For-each start, begins a sum
         (if (>= (* (car valid-primes) min-length) i)
             max-length; First element from the sum is too large to reach i
             (let ((new-max-length (find-length valid-primes i max-length 0 0)))
              (find-max-length (cdr valid-primes) i new-max-length)))))
    
    (define (go-problem-50 i-primes-list max-length max-prime)
        (if (null? i-primes-list)
            (cons max-length max-prime)
            (let* ((i (car i-primes-list))
                   (new-i-primes-list (cdr i-primes-list))
                   (i-max-length (find-max-length primes-list i max-length)))
             
             (if (> i-max-length max-length)
                 (go-problem-50 new-i-primes-list i-max-length i)
                 (go-problem-50 new-i-primes-list max-length max-prime)))))

    (go-problem-50 primes-list 21 953))
    
(print (problem-50 1000000))
;; (print (length (primes 1000)))