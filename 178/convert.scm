;;;; Bit conversions

(: bit->integer (bit --> integer))
(define (bit->integer bit)
  (assert-type 'bit->integer (%bit? bit))
  (I bit))

(: bit->boolean (bit --> boolean))
(define (bit->boolean bit)
  (assert-type 'bit->boolean (%bit? bit))
  (B bit))

(: bitvector->string (bitvector --> string))
(define (bitvector->string bvec)
  (assert-type 'bitvector->string (bitvector? bvec))
  (let loop ((i (- (%bitvector-length-no-checks bvec) 1))
             (r '()))
    (if (< i 0)
      (list->string (cons #\# (cons #\* r)))
      (loop (- i 1)
            (cons (if (%bitvector-ref/bool-no-checks bvec i)
                      #\1
                      #\0)
                  r)))))

(: string->bitvector (string -> (or bitvector false)))
(define (string->bitvector str)
  (assert-type 'string->bitvector (string? str))
  (call/cc
   (lambda (return)
     (and
       (> (string-length str) 1)
       (char=? (string-ref str 0) #\#)
       (char=? (string-ref str 1) #\*)
       (%bitvector-unfold-no-checks
        (lambda (ri si)
          (case (string-ref str si)
            ((#\0) (values 0 (+ si 1)))
            ((#\1) (values 1 (+ si 1)))
            (else (return #f))))
        (- (string-length str) 2)
        2)))))

;;;; Bitvector/integer conversions

(: bitvector->integer (bitvector --> integer))
(define (bitvector->integer bvec)
  (assert-type 'bitvector->integer (bitvector? bvec))
  (bitvector-fold-right/int (lambda (r b) (+ (* r 2) b)) 0 bvec))

(: integer->bitvector (integer #!optional integer -> bitvector))
(define integer->bitvector
  (case-lambda
    ((int) (integer->bitvector int (integer-length int)))
    ((int len)
     (assert-type 'integer->bitvector (exact-natural? int))
     (assert-type 'integer->bitvector (exact-natural? len))
     (%bitvector-unfold-no-checks
      (lambda (_ int)
        (values (bit-set? 0 int) (arithmetic-shift int -1)))
      len
      int))))

;;; Additional vector conversions

(: reverse-vector->bitvector
   ((vector-of bit) #!optional integer integer -> bitvector))
(define reverse-vector->bitvector
  (case-lambda
    ((vec) (reverse-vector->bitvector vec 0 (vector-length vec)))
    ((vec start) (reverse-vector->bitvector vec start (vector-length vec)))
    ((vec start end)
     (assert-type 'reverse-vector->bitvector (vector? vec))
     (assert-type 'reverse-vector->bitvector (exact-integer? start))
     (assert-type 'reverse-vector->bitvector (exact-integer? end))
     (unless (<= 0 start end (vector-length vec))
       (bounds-exception 'reverse-vector->bitvector
                         "invalid start, end indices"
                         start
                         end
                         vec))
     (%bitvector-unfold-no-checks
      (lambda (i)
        (vector-ref vec (- end 1 i)))
      (- end start)))))

(: reverse-bitvector->vector/int
   (bitvector #!optional integer integer -> (vector-of integer)))
(define reverse-bitvector->vector/int
  (case-lambda
    ((bvec)
     (reverse-bitvector->vector/int bvec 0 (%bitvector-length-no-checks bvec)))
    ((bvec start)
     (reverse-bitvector->vector/int bvec
                                    start
                                    (%bitvector-length-no-checks bvec)))
    ((bvec start end)
     (assert-type 'reverse-bitvector->vector/int (bitvector? bvec))
     (assert-type 'reverse-bitvector->vector/int (exact-integer? start))
     (assert-type 'reverse-bitvector->vector/int (exact-integer? end))
     (%check-range 'reverse-bitvector->vector/int bvec start end)
     (let ((u8vec (U bvec)))
       (vector-unfold (lambda (i)
                        (u8vector-ref u8vec (- end 1 i)))
                      (- end start))))))

(: reverse-bitvector->vector/bool
   (bitvector #!optional integer integer -> (vector-of boolean)))
(define reverse-bitvector->vector/bool
  (case-lambda
    ((bvec)
     (reverse-bitvector->vector/bool bvec
                                     0
                                     (%bitvector-length-no-checks bvec)))
    ((bvec start)
     (reverse-bitvector->vector/bool bvec
                                     start
                                     (%bitvector-length-no-checks bvec)))
    ((bvec start end)
     (assert-type 'reverse-bitvector->vector/bool (bitvector? bvec))
     (assert-type 'reverse-bitvector->vector/bool (exact-integer? start))
     (assert-type 'reverse-bitvector->vector/bool (exact-integer? end))
     (%check-range 'reverse-bitvector->vector/bool bvec start end)
     (let ((u8vec (U bvec)))
       (vector-unfold (lambda (i)
                        (B (u8vector-ref u8vec (- end 1 i))))
                      (- end start))))))
