(: bitvector-logical-shift (bitvector integer bit -> bitvector))
(define (bitvector-logical-shift bvec count bit)
  (assert (bitvector? bvec))
  (assert (exact-integer? count))
  (assert (%bit? bit))
  (cond ((positive? count)
         (%bitvector-left-shift bvec count (I bit)))
        ((negative? count)
         (%bitvector-right-shift bvec (- count) (I bit)))
        (else bvec)))

(: %bitvector-left-shift (bitvector integer integer -> bitvector))
(define (%bitvector-left-shift bvec count bit)
  (let ((len (bitvector-length bvec)))
    (bitvector-unfold
     (lambda (i)
       (let ((i* (+ i count)))
         (if (< i* len) (bitvector-ref/int bvec i*) bit)))
     len)))

(: %bitvector-right-shift (bitvector integer integer -> bitvector))
(define (%bitvector-right-shift bvec count bit)
  (bitvector-unfold
   (lambda (i)
     (if (< i count)
         bit
         (bitvector-ref/int bvec (- i count))))
   (bitvector-length bvec)))

(: bitvector-count (bit bitvector --> integer))
(define (bitvector-count bit bvec)
  (assert (%bit? bit))
  (assert (bitvector? bvec))
  (let ((int (I bit)))
    (bitvector-fold/int (lambda (n b) (if (= b int) (+ n 1) n))
                        0
                        bvec)))

(: bitvector-count-run (bit bitvector integer --> integer))
(define (bitvector-count-run bit bvec index)
  (assert (%bit? bit))
  (assert (bitvector? bvec))
  (assert (exact-natural? index))
  (let ((int (I bit))
        (len (bitvector-length bvec)))
    (let lp ((i index) (c 0))
      (if (or (>= i len) (not (= int (bitvector-ref/int bvec i))))
          c
          (lp (+ i 1) (+ c 1))))))

(: bitvector-if (bitvector bitvector bitvector -> bitvector))
(define (bitvector-if if-bvec then-bvec else-bvec)
  (bitvector-map/bool (lambda (bit then-bit else-bit)
                        (if bit then-bit else-bit))
                      if-bvec
                      then-bvec
                      else-bvec))

(: bitvector-first-bit (bit bitvector -> integer))
(define (bitvector-first-bit bit bvec)
  (assert (%bit? bit))
  (assert (bitvector? bvec))
  (let ((int (I bit)) (len (bitvector-length bvec)))
    (let lp ((i 0))
      (cond ((>= i len) -1)
            ((= int (bitvector-ref/int bvec i)) i)
            (else (lp (+ i 1)))))))

