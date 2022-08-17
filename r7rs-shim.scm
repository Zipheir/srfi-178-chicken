(: eof-object (-> eof))
(define (eof-object) #!eof)

;; Single-vector-only version.
(: vector-map ((* -> *) vector -> vector))
(define (vector-map f vec)
  (assert-type 'vector-map (procedure? f))
  (assert-type 'vector-map (vector? vec))
  (let* ((len (vector-length vec))
         (res (make-vector len)))
    (let lp ((i 0))
      (cond ((= i len) res)
            (else (vector-set! res i (f (vector-ref vec i)))
                  (lp (+ i 1)))))))
