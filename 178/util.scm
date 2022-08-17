(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

(define (%bit? x)
  (cond ((boolean? x) #t)
        ((integer? x) (or (zero? x) (= x 1)))
        (else #f)))
