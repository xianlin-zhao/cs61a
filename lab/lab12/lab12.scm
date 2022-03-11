(define (tail-replicate x n) 
    (define (loop so_far id)
    (if (= id 0)
        so_far
        (loop (append (list x) so_far) (- id 1))))
    (loop '() n))

(define-macro (def func args body)
    `(define (,func ,@args) ,body))

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let 
           ((y (repeatedly-cube (- n 1) x)))
        (* y y y))))
