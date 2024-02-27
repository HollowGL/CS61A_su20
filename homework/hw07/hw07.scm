(define (cddr s) 
    (cdr (cdr s)))

(define (cadr s) 
    (car (cdr s)))

(define (caddr s) 
    (car (cddr s)))

(define (sign num)
  'YOUR-CODE-HERE
  (cond 
    ((> num 0) 1)
    ((< num 0) -1)
    (else      0)))

(define (square x) (* x x))

(define (pow x y) 
  (cond
    ((= y 1) x)
    ((even? y) (square (pow x (/ y 2))))
    (else (* (pow x (- y 1)) x))
  )  
)

(define (unique s) 
  (if (null? s)
    nil
    (append 
      (list(car s))
      (unique (filter (lambda (item) (not (equal? (car s) item))) (cdr s))))
  )
)

(define (replicate x n) 
  (define (helper x n res)
    (if (= n 0)
      res
      (helper x (- n 1) (cons x res))
    )
  )
  (helper x n '())
)

(define (accumulate combiner start n term)
  (define (acc_helper res i)
    (if (> i n)
      res
      (acc_helper (combiner res (term i)) (+ i 1))
    )
  )
  (acc_helper start 1)
)

(define (accumulate-tail combiner start n term)
  (accumulate combiner start n term)
)

(define-macro
 (list-of map-expr for var in lst if filter-expr)
 `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)
