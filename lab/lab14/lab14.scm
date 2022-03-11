(define (split-at lst n) 
    (define (loop so_far L id)
        (cond ((null? L) (cons so_far nil))
              ((= id 0) (cons so_far L))
              (else (loop (append so_far (list (car L))) (cdr L) (- id 1)))))
          (loop '() lst n))

(define (compose-all funcs) 
    (lambda (x) (
                  if (null? funcs)
                  x
                  ((compose-all (cdr funcs)) ((car funcs) x)))))
