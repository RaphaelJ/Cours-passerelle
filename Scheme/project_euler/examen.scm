(declare (uses euler-utils))

(print (fibonacci 12))

(print (flat '(a (b c d) ((e f) g))))

(define f (lambda (n)
    (define go (lambda (i)
        (if (zero? i)
            (list 0)
            (let* ((prec (go (- i 1)))
                   (rev-prec (reverse prec)))
             (cons (modulo (go-sum 0 i prec rev-prec 0)
                           (+ (* 2 i) 3))
                   prec)))))
    
    (define go-sum (lambda (i j prec rev-prec acc)
        (if (= i j)
            acc
            (go-sum (+ i 1) j (cdr prec) (cdr rev-prec)
                    (+ acc (* (+ 2 (car rev-prec))
                              (+ 3 (car prec))))))))


    (car (go n))))
    
    
(define gib (lambda (n h)
    (h (gib-a n h '()))))
    
(define gib-a (lambda (n h l)
    (if (= n 0) 
        l 
        (gib-a (- n 1) h (cons (h l) l)))))

(define g (lambda (prec)
    (let* ((rev-prec (reverse prec))
           (len (length prec)))
     
     (modulo (go-sum 0 len prec rev-prec 0)
             (+ (* 2 len) 3)))))
    
(define go-sum (lambda (i j prec rev-prec acc)
    (if (= i j)
        acc
        (go-sum (+ i 1) j (cdr prec) (cdr rev-prec)
                (+ acc (* (+ 2 (car rev-prec))
                            (+ 3 (car prec))))))))
    
(print (partitions '(a)))