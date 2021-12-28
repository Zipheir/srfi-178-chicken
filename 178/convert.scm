;;;; Bit conversions

(: bit->integer (bit --> fixnum))
(define (bit->integer bit)
  (assert (%bit? bit))
  (I bit))

(: bit->boolean (bit --> boolean))
(define (bit->boolean bit)
  (assert (%bit? bit))
  (B bit))

(: bitvector->string (bitvector --> string))
(define (bitvector->string bvec)
  (assert (bitvector? bvec))
  (let loop ((i (- (bitvector-length bvec) 1))
             (r '()))
    (if (< i 0)
      (list->string (cons #\# (cons #\* r)))
      (loop (- i 1)
            (cons (if (bitvector-ref/bool bvec i) #\1 #\0) r)))))

(: string->bitvector (string -> (or bitvector false)))
(define (string->bitvector str)
  (assert (string? str))
  (call/cc
   (lambda (return)
     (and
       (> (string-length str) 1)
       (char=? (string-ref str 0) #\#)
       (char=? (string-ref str 1) #\*)
       (bitvector-unfold
        (lambda (ri si)
          (case (string-ref str si)
            ((#\0) (values 0 (+ si 1)))
            ((#\1) (values 1 (+ si 1)))
            (else (return #f))))
        (- (string-length str) 2)
        2)))))

;;;; Bitvector/integer conversions

(: bitvector->integer (bitvector --> fixnum))
(define (bitvector->integer bvec)
  (assert (bitvector? bvec))
  (bitvector-fold-right/int (lambda (r b) (+ (* r 2) b)) 0 bvec))

(: integer->bitvector (fixnum #!optional fixnum -> bitvector))
(define integer->bitvector
  (case-lambda
    ((int) (integer->bitvector int (integer-length int)))
    ((int len)
     (assert (exact-natural? int))
     (assert (exact-natural? len))
     (bitvector-unfold
      (lambda (_ int)
        (values (bit-set? 0 int) (arithmetic-shift int -1)))
      len
      int))))

;;; Additional vector conversions

(: reverse-vector->bitvector
   ((vector-of bit) #!optional fixnum fixnum -> bitvector))
(define reverse-vector->bitvector
  (case-lambda
    ((vec) (reverse-vector->bitvector vec 0 (vector-length vec)))
    ((vec start) (reverse-vector->bitvector vec start (vector-length vec)))
    ((vec start end)
     (assert (vector? vec))
     (assert (exact-natural? start))
     (assert (exact-natural? end))
     (bitvector-unfold
      (lambda (i)
        (vector-ref vec (- end 1 i)))
      (- end start)))))

(: reverse-bitvector->vector/int
   (bitvector #!optional fixnum fixnum -> (vector-of fixnum)))
(define reverse-bitvector->vector/int
  (case-lambda
    ((bvec)
     (reverse-bitvector->vector/int bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (reverse-bitvector->vector/int bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert (bitvector? bvec))
     (assert (exact-natural? start))
     (assert (exact-natural? end))
     (let ((u8vec (U bvec)))
       (vector-unfold (lambda (i)
                        (u8vector-ref u8vec (- end 1 i)))
                      (- end start))))))

(: reverse-bitvector->vector/bool
   (bitvector #!optional fixnum fixnum -> (vector-of boolean)))
(define reverse-bitvector->vector/bool
  (case-lambda
    ((bvec)
     (reverse-bitvector->vector/bool bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (reverse-bitvector->vector/bool bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert (bitvector? bvec))
     (assert (exact-natural? start))
     (assert (exact-natural? end))
     (let ((u8vec (U bvec)))
       (vector-unfold (lambda (i)
                        (B (u8vector-ref u8vec (- end 1 i))))
                      (- end start))))))
