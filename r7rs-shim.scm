(define eof-object
  (let ((eof (read (open-input-string ""))))
    (lambda () eof)))

;; Snarfed from the r7rs egg.  Why didn't this get added to
;; (chicken base) in CHICKEN 5?
(define (floor/ x y)
  (let-values (((div rem) (quotient&remainder x y)))
    (if (positive? y)
        (if (negative? rem)
            (values (- div 1) (+ rem y))
            (values div rem))
        (if (positive? rem)
            (values (- div 1) (+ rem y))
            (values div rem)))))

(define (floor-remainder x y)
  (let-values (((_ rem) (floor/ x y)))
    rem))

;; Single-vector-only version.
(define (vector-map f vec)
  (let* ((len (vector-length vec))
         (res (make-vector len)))
    (let lp ((i 0))
      (cond ((= i len) res)
            (else (vector-set! res i (f (vector-ref vec i)))
                  (lp (+ i 1)))))))
