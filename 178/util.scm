(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

(define (%bit? x)
  (cond ((boolean? x) #t)
        ((integer? x) (or (zero? x) (= x 1)))
        (else #f)))

(define (pair-or-null? x)
  (or (pair? x) (null? x)))

;; We only need a basic version of this SRFI 1 procedure.
(: every (procedure list -> boolean))
(define (every pred lis)
  (or (null? lis)
      (and (pred (car lis))
           (every pred (cdr lis)))))
