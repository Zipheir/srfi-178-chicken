(define eof-object
  (let ((eof (read (open-input-string ""))))
    (lambda () eof)))

(define floor-remainder remainder)

;; Single-vector-only version.
(define (vector-map f vec)
  (let* ((len (vector-length vec))
         (res (make-vector len)))
    (let lp ((i 0))
      (cond ((= i len) res)
            (else (vector-set! res i (f (vector-ref vec i)))
                  (lp (+ i 1)))))))
