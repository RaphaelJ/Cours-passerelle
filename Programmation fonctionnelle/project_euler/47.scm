(declare (uses euler-utils))

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