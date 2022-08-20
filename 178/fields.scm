(: bitvector-field-any? (bitvector integer integer -> boolean))
(define (bitvector-field-any? bvec start end)
  (assert-type 'bitvector-field-any? (bitvector? bvec))
  (assert-type 'bitvector-field-any? (exact-natural? start))
  (assert-type 'bitvector-field-any? (exact-natural? end))
  (%check-range 'bitvector-field-any? bvec start end)
  (let lp ((i start))
    (and (< i end)
         (or (bitvector-ref/bool bvec i)
             (lp (+ i 1))))))

(: bitvector-field-every? (bitvector integer integer -> boolean))
(define (bitvector-field-every? bvec start end)
  (assert-type 'bitvector-field-every? (bitvector? bvec))
  (assert-type 'bitvector-field-every? (exact-natural? start))
  (assert-type 'bitvector-field-every? (exact-natural? end))
  (%check-range 'bitvector-field-every? bvec start end)
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

(: bitvector-field-clear (bitvector integer integer -> bitvector))
(define (bitvector-field-clear bvec start end)
  (assert-type 'bitvector-field-clear (bitvector? bvec))
  (assert-type 'bitvector-field-clear (exact-natural? start))
  (assert-type 'bitvector-field-clear (exact-natural? end))
  (%check-range 'bitvector-field-clear bvec start end)
  (%bitvector-field-modify bvec 0 start end))

(: %bitvector-fill!/int (bitvector integer integer integer -> undefined))
(define (%bitvector-fill!/int bvec int start end)
  (u8vector-fill! (U bvec) int start end))

(: bitvector-field-clear! (bitvector integer integer -> undefined))
(define (bitvector-field-clear! bvec start end)
  (assert-type 'bitvector-field-clear! (bitvector? bvec))
  (assert-type 'bitvector-field-clear! (exact-natural? start))
  (assert-type 'bitvector-field-clear! (exact-natural? end))
  (%check-range 'bitvector-field-clear! bvec start end)
  (%bitvector-fill!/int bvec 0 start end))

(: bitvector-field-set (bitvector integer integer -> bitvector))
(define (bitvector-field-set bvec start end)
  (assert-type 'bitvector-field-set (bitvector? bvec))
  (assert-type 'bitvector-field-set (exact-natural? start))
  (assert-type 'bitvector-field-set (exact-natural? end))
  (%check-range 'bitvector-field-set bvec start end)
  (%bitvector-field-modify bvec 1 start end))

(: bitvector-field-set! (bitvector integer integer -> undefined))
(define (bitvector-field-set! bvec start end)
  (assert-type 'bitvector-field-set! (bitvector? bvec))
  (assert-type 'bitvector-field-set! (exact-natural? start))
  (assert-type 'bitvector-field-set! (exact-natural? end))
  (%check-range 'bitvector-field-set! bvec start end)
  (%bitvector-fill!/int bvec 1 start end))

(: bitvector-field-replace
   (bitvector bitvector integer integer -> bitvector))
(define (bitvector-field-replace dest source start end)
  (assert-type 'bitvector-field-replace (bitvector? dest))
  (assert-type 'bitvector-field-replace (bitvector? source))
  (assert-type 'bitvector-field-replace (exact-natural? start))
  (assert-type 'bitvector-field-replace (exact-natural? end))
  (%check-range 'bitvector-field-replace bvec start end)
  (bitvector-unfold
   (lambda (i)
     (if (and (>= i start) (< i end))
         (bitvector-ref/int source (- i start))
         (bitvector-ref/int dest i)))
   (bitvector-length dest)))

(: bitvector-field-replace!
   (bitvector bitvector integer integer -> undefined))
(define (bitvector-field-replace! dest source start end)
  (assert-type 'bitvector-field-replace! (bitvector? dest))
  (assert-type 'bitvector-field-replace! (bitvector? source))
  (assert-type 'bitvector-field-replace! (exact-natural? start))
  (assert-type 'bitvector-field-replace! (exact-natural? end))
  (%check-range 'bitvector-field-replace! bvec start end)
  (bitvector-copy! dest start source 0 (- end start)))

(: bitvector-field-replace-same
   (bitvector bitvector integer integer -> bitvector))
(define (bitvector-field-replace-same dest source start end)
  (assert-type 'bitvector-field-replace-same (bitvector? dest))
  (assert-type 'bitvector-field-replace-same (bitvector? source))
  (assert-type 'bitvector-field-replace-same (exact-natural? start))
  (assert-type 'bitvector-field-replace-same (exact-natural? end))
  (%check-range 'bitvector-field-replace-same bvec start end)
  (bitvector-unfold
   (lambda (i)
     (bitvector-ref/int (if (and (>= i start) (< i end))
                            source
                            dest)
                        i))
   (bitvector-length dest)))

(: bitvector-field-replace-same!
   (bitvector bitvector integer integer -> undefined))
(define (bitvector-field-replace-same! dest source start end)
  (assert-type 'bitvector-field-replace-same! (bitvector? dest))
  (assert-type 'bitvector-field-replace-same! (bitvector? source))
  (assert-type 'bitvector-field-replace-same! (exact-natural? start))
  (assert-type 'bitvector-field-replace-same! (exact-natural? end))
  (%check-range 'bitvector-field-replace-same! bvec start end)
  (bitvector-copy! dest start source start end))

(: bitvector-field-rotate (bitvector integer integer integer -> bitvector))
(define (bitvector-field-rotate bvec count start end)
  (assert-type 'bitvector-field-rotate (bitvector? bvec))
  (assert-type 'bitvector-field-rotate (exact-integer? count))
  (assert-type 'bitvector-field-rotate (exact-natural? start))
  (assert-type 'bitvector-field-rotate (exact-natural? end))
  (%check-range 'bitvector-field-rotate bvec start end)
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

(: bitvector-field-flip (bitvector integer integer -> bitvector))
(define (bitvector-field-flip bvec start end)
  (assert-type 'bitvector-field-flip (bitvector? bvec))
  (assert-type 'bitvector-field-flip (exact-natural? start))
  (assert-type 'bitvector-field-flip (exact-natural? end))
  (%check-range 'bitvector-field-flip bvec start end)
  (bitvector-unfold
   (lambda (i)
     (I (if (and (>= i start) (< i end))
            (not (bitvector-ref/bool bvec i))
            (bitvector-ref/bool bvec i))))
   (bitvector-length bvec)))

(: bitvector-field-flip! (bitvector integer integer -> undefined))
(define (bitvector-field-flip! bvec start end)
  (assert-type 'bitvector-field-flip! (bitvector? bvec))
  (assert-type 'bitvector-field-flip! (exact-natural? start))
  (assert-type 'bitvector-field-flip! (exact-natural? end))
  (%check-range 'bitvector-field-flip! bvec start end)
  (let lp ((i start))
    (unless (>= i end)
      (bitvector-set! bvec i (not (bitvector-ref/bool bvec i)))
      (lp (+ i 1)))))

