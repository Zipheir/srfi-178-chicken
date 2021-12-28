(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

(define (%bit? x) (or (boolean? x) (zero? x) (= x 1)))
