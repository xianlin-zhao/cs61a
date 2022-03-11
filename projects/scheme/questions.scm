(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (loop so_far id lst)
      (if (null? lst)
          so_far
          (loop (append so_far (list (list id (car lst)))) (+ id 1) (cdr lst))))
      (loop '() 0 s)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (loop so_far lst1 lst2)
      (cond ((and (null? lst1) (null? lst2)) so_far)
            ((and (null? lst1) (not (null? lst2))) (loop (append so_far (list (car lst2))) lst1 (cdr lst2)))
            ((and (not (null? lst1)) (null? lst2)) (loop (append so_far (list (car lst1))) (cdr lst1) lst2))
            ((comp (car lst1) (car lst2)) (loop (append so_far (list (car lst1))) (cdr lst1) lst2))
            (else (loop (append so_far (list (car lst2))) lst1 (cdr lst2)))))
        (loop '() list1 list2)
  )
  ; END PROBLEM 16


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (cons form (cons (map let-to-lambda params) (map let-to-lambda body)))
           ; END PROBLEM 17
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (cons (cons 'lambda 
                     (cons (car (zip (let-to-lambda values))) (let-to-lambda body))) 
                (cadr (zip (let-to-lambda values))))
           ; END PROBLEM 17
           ))
        (else
         ; BEGIN PROBLEM 17
         expr
         (map let-to-lambda expr)
         )))

