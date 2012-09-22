(declare (uses euler-utils))

(define (problem-48 max-exponent precision)
    (define (sum-exponent acc i)
        (let* ((arb-i (arb-number i))
               (val (arb-expt arb-i i precision)))
         (arb-add acc val precision)))

    (let* ((exponents (range 1 max-exponent))
           (arb-result (foldl sum-exponent (arb-number 0) exponents)))
     (from-arb-number arb-result)))
    
(print (problem-48 1000 10))