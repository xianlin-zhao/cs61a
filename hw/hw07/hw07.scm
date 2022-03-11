(define (filter-lst fn lst)
    (define (loop so_far L)
        (cond ((null? L) so_far)
              ((fn (car L)) (loop (append so_far (list (car L))) (cdr L)))
              (else (loop so_far (cdr L)))))
          (loop '() lst))

; ;; Tests
(define (even? x) (= (modulo x 2) 0))

(filter-lst even? '(0 1 1 2 3 5 8))

; expect (0 2 8)
(define (interleave first second) 
    (define (loop so_far A B)
        (cond ((and (null? A) (null? B)) so_far)
              ((and (not (null? A)) (null? B)) (loop (append so_far (list (car A))) (cdr A) B))
              ((and (not (null? B)) (null? A)) (loop (append so_far (list (car B))) A (cdr B)))
              (else (loop (append so_far (list (car A) (car B))) (cdr A) (cdr B)))))
          (loop '() first second))

(interleave (list 1 5 3) (list 2 4 6))

; expect (1 2 5 4 3 6)
(interleave (list 1 3 5) nil)

; expect (1 3 5)
(interleave (list 1 3 5) (list 2 4))

; expect (1 2 3 4 5)
(define (accumulate combiner start n term) 
  (define (loop so_far id)
      (if (= id 0)
      so_far
      (loop (combiner so_far (term id)) (- id 1))))
  (loop start n))

(define (without-duplicates lst)
    (if (null? lst)
        lst
        (append (list (car lst)) (without-duplicates (filter (lambda (x) (not (= x (car lst)))) (cdr lst))))))
