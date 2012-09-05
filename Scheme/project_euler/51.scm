(declare (uses euler-utils))

(define (problem-51 n-primes)
    (define primes-list (primes 200000))

    (define (transforms-go ds d-to-replace)
        
        (if (null? ds) 
            (cons fct-prec acc)
            (let* ((d (car ds))
                   (new-ds (cdr ds))
                   (with-d    (lambda (x) (cons d (fct-prec x))))
                   (without-d (lambda (x) (cons x (fct-prec x)))))
                   
             ; Imposes that at least a digit as been tranformed
             (if (and (not one) (null? (cdr ds))) 
                 (transforms-go new-ds with-d #t acc) 
                 (transforms-go new-ds with-d #t     
                    (transforms-go new-ds without-d one acc))))))

    ; Returns a list of all tranformsations of n, which are functions which 
    ; accept a digit (x) to replace.
    (define (transforms n) 
        (let* ((ds (digits n))
               (ds-unique (nub ds)))
         
        ))
        (nub (digits n))
        
        ; Reverse digits of each transforms
        (map (lambda (t) (lambda (x) (reverse (t x))))
             (transforms-go (digits n) (lambda (x) '()) #f '())))
    
    ; Returns true if the transformation yield enough primes
    (define (enough-primes? t)
        (define (valid-prime? x)
            (and (not (= (car x) 0)) (prime? (from-digits x) primes-list)))
    
        (cond ((= n-primes (length (filter valid-prime? (map t (range 0 9)))))
               (print (t "x")))
              (else #f)))

    (define (go-problem-51 ps)
        (let ((p (car ps)))
         (if (any enough-primes? (transforms p))
             p
             (go-problem-51 (cdr ps)))))
    
    (go-problem-51 primes-list))
    
(print (problem-51 8))