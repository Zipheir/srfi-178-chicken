(: bitvector-field-any? (bitvector fixnum fixnum -> boolean))
(define (bitvector-field-any? bvec start end)
  (let lp ((i start))
    (and (< i end)
         (or (bitvector-ref/bool bvec i)
             (lp (+ i 1))))))

(: bitvector-field-every? (bitvector fixnum fixnum -> boolean))
(define (bitvector-field-every? bvec start end)
  (let lp ((i start))
    (or (>= i end)
        (and (bitvector-ref/bool bvec i)
             (lp (+ i 1))))))

(define (%bitvector-field-modify bvec bit start end)
  (bitvector-unfold
   (lambda (i)
     (if (and (>= i start) (< i end))
         bit
         (bitvector-ref/int bvec i)))
   (bitvector-length bvec)))

(: bitvector-field-clear (bitvector fixnum fixnum -> bitvector))
(define (bitvector-field-clear bvec start end)
  (%bitvector-field-modify bvec 0 start end))

(: %bitvector-fill!/int (bitvector fixnum fixnum fixnum -> undefined))
(define (%bitvector-fill!/int bvec int start end)
  (u8vector-fill! (U bvec) int start end))

(: bitvector-field-clear! (bitvector fixnum fixnum -> undefined))
(define (bitvector-field-clear! bvec start end)
  (%bitvector-fill!/int bvec 0 start end))

(: bitvector-field-set (bitvector fixnum fixnum -> bitvector))
(define (bitvector-field-set bvec start end)
  (%bitvector-field-modify bvec 1 start end))

(: bitvector-field-set! (bitvector fixnum fixnum -> undefined))
(define (bitvector-field-set! bvec start end)
  (%bitvector-fill!/int bvec 1 start end))

(: bitvector-field-replace
   (bitvector bitvector fixnum fixnum -> bitvector))
(define (bitvector-field-replace dest source start end)
  (bitvector-unfold
   (lambda (i)
     (if (and (>= i start) (< i end))
         (bitvector-ref/int source (- i start))
         (bitvector-ref/int dest i)))
   (bitvector-length dest)))

(: bitvector-field-replace!
   (bitvector bitvector fixnum fixnum -> undefined))
(define (bitvector-field-replace! dest source start end)
  (bitvector-copy! dest start source 0 (- end start)))

(: bitvector-field-replace-same
   (bitvector bitvector fixnum fixnum -> bitvector))
(define (bitvector-field-replace-same dest source start end)
  (bitvector-unfold
   (lambda (i)
     (bitvector-ref/int (if (and (>= i start) (< i end))
                            source
                            dest)
                        i))
   (bitvector-length dest)))

(: bitvector-field-replace-same!
   (bitvector bitvector fixnum fixnum -> undefined))
(define (bitvector-field-replace-same! dest source start end)
  (bitvector-copy! dest start source start end))

(: bitvector-field-rotate (bitvector fixnum fixnum fixnum -> bitvector))
(define (bitvector-field-rotate bvec count start end)
  (if (zero? count)
      bvec
      (let ((field-len (- end start)))
        (bitvector-unfold
         (lambda (i)
           (if (and (>= i start) (< i end))
               (bitvector-ref/int
                bvec
                (+ start (floor-remainder (+ (- i start) count) field-len)))
               (bitvector-ref/int bvec i)))
         (bitvector-length bvec)))))

(: bitvector-field-flip (bitvector fixnum fixnum -> bitvector))
(define (bitvector-field-flip bvec start end)
  (bitvector-unfold
   (lambda (i)
     (I (if (and (>= i start) (< i end))
            (not (bitvector-ref/bool bvec i))
            (bitvector-ref/bool bvec i))))
   (bitvector-length bvec)))

(: bitvector-field-flip! (bitvector fixnum fixnum -> undefined))
(define (bitvector-field-flip! bvec start end)
  (let lp ((i start))
    (unless (>= i end)
      (bitvector-set! bvec i (not (bitvector-ref/bool bvec i)))
      (lp (+ i 1)))))

