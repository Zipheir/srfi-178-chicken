;;;; SRFI 178 procedures that are just wrappers

(: make-bitvector (integer #!optional bit -> bitvector))
(define make-bitvector
  (case-lambda
    ((size)
     (make-bitvector size 0))
    ((size bit)
     (assert-type 'make-bitvector (exact-integer? size))
     (assert-type 'make-bitvector (%bit? bit))
     (W (make-u8vector size (I bit))))))

(: bitvector-copy (bitvector #!optional integer integer -> bitvector))
(define bitvector-copy
  (case-lambda
    ((bvec)
     (assert-type 'bitvector-copy (bitvector? bvec))
     (W (u8vector-copy (U bvec))))
    ((bvec start)
     (bitvector-copy bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'bitvector-copy (bitvector? bvec))
     (assert-type 'bitvector-copy (exact-integer? start))
     (assert-type 'bitvector-copy (exact-integer? end))
     (%check-range 'bitvector-copy bvec start end)
     (W (u8vector-copy (U bvec) start end)))))

(: bitvector-reverse-copy
   (bitvector #!optional integer integer -> bitvector))
(define bitvector-reverse-copy
  (case-lambda
    ((bvec)
     (assert-type 'bitvector-reverse-copy (bitvector? bvec))
     (W (u8vector-reverse-copy (U bvec))))
    ((bvec start)
     (bitvector-reverse-copy bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'bitvector-reverse-copy (bitvector? bvec))
     (assert-type 'bitvector-reverse-copy (exact-integer? start))
     (assert-type 'bitvector-reverse-copy (exact-integer? end))
     (%check-range 'bitvector-reverse-copy bvec start end)
     (W (u8vector-reverse-copy (U bvec) start end)))))

(: bitvector-append (#!rest bitvector -> bitvector))
(define (bitvector-append . bvecs)
  (bitvector-concatenate bvecs))

(: bitvector-concatenate ((list-of bitvector) -> bitvector))
(define (bitvector-concatenate bvecs)
  (assert-type 'bitvector-concatenate (pair-or-null? bvecs))
  (W (u8vector-concatenate (map U bvecs))))

(: bitvector-append-subbitvectors (#!rest * -> bitvector))
(define (bitvector-append-subbitvectors . args)
  (assert-type 'bitvector-append-subbitvectors
               (every (lambda (x)
                        (or (bitvector? x) (exact-integer? x)))
                      args))
  (W (apply u8vector-append-subvectors
            (map (lambda (x) (if (bitvector? x) (U x) x)) args))))

(: bitvector-empty? (bitvector -> boolean))
(define (bitvector-empty? bvec)
  (assert-type 'bitvector-empty? (bitvector? bvec))
  (eqv? 0 (u8vector-length (U bvec))))

(: bitvector=? (#!rest bitvector -> boolean))
(define (bitvector=? . bvecs)
  (assert-type 'bitvector=? (every bitvector? bvecs))
  (apply u8vector= (map U bvecs)))

(: bitvector-ref/int (bitvector integer -> integer))
(define (bitvector-ref/int bvec i)
  (assert-type 'bitvector-ref/int (bitvector? bvec))
  (assert-type 'bitvector-ref/int (exact-integer? i))
  (%check-index 'bitvector-ref/int bvec i)
  (u8vector-ref (U bvec) i))

(: bitvector-ref/bool (bitvector integer -> boolean))
(define (bitvector-ref/bool bvec i)
  (assert-type 'bitvector-ref/bool (bitvector? bvec))
  (assert-type 'bitvector-ref/bool (exact-integer? i))
  (%check-index 'bitvector-ref/bool bvec i)
  (B (u8vector-ref (U bvec) i)))

(: bitvector-length (bitvector -> integer))
(define (bitvector-length bvec)
  (assert-type 'bitvector-length (bitvector? bvec))
  (u8vector-length (U bvec)))

(: bitvector-take (bitvector integer -> bitvector))
(define (bitvector-take bvec n)
  (assert-type 'bitvector-take (bitvector? bvec))
  (assert-type 'bitvector-take (exact-integer? n))
  (unless (<= 0 n (bitvector-length bvec))
    (bounds-exception 'bitvector-take
                      "bitvector too short"
                      n
                      bvec))
  (W (u8vector-take (U bvec) n)))

(: bitvector-take-right (bitvector integer -> bitvector))
(define (bitvector-take-right bvec n)
  (assert-type 'bitvector-take-right (bitvector? bvec))
  (assert-type 'bitvector-take-right (exact-integer? n))
  (unless (<= 0 n (bitvector-length bvec))
    (bounds-exception 'bitvector-take-right
                      "bitvector too short"
                      n
                      bvec))
  (W (u8vector-take-right (U bvec) n)))

(: bitvector-drop (bitvector integer -> bitvector))
(define (bitvector-drop bvec n)
  (assert-type 'bitvector-drop (bitvector? bvec))
  (assert-type 'bitvector-drop (exact-integer? n))
  (unless (<= 0 n (bitvector-length bvec))
    (bounds-exception 'bitvector-drop
                      "bitvector too short"
                      n
                      bvec))
  (W (u8vector-drop (U bvec) n)))

(: bitvector-drop-right (bitvector integer -> bitvector))
(define (bitvector-drop-right bvec n)
  (assert-type 'bitvector-drop-right (bitvector? bvec))
  (assert-type 'bitvector-drop-right (exact-integer? n))
  (unless (<= 0 n (bitvector-length bvec))
    (bounds-exception 'bitvector-drop-right
                      "bitvector too short"
                      n
                      bvec))
  (W (u8vector-drop-right (U bvec) n)))

(: bitvector-segment (bitvector integer -> (list-of bitvector)))
(define (bitvector-segment bvec n)
  (assert-type 'bitvector-segment (bitvector? bvec))
  (assert-type 'bitvector-segment (exact-natural? n))
  (when (zero? n)
    (error 'bitvector-segment "invalid segment length" n))
  (map W (u8vector-segment (U bvec) n)))

(: bitvector-fold/int (procedure * #!rest bitvector -> *))
(define bitvector-fold/int
  (case-lambda
    ((kons knil bvec)
     (assert-type 'bitvector-fold/int (procedure? kons))
     (assert-type 'bitvector-fold/int (bitvector? bvec))
     (u8vector-fold kons knil (U bvec)))  ; fast path
    ((kons knil . bvecs)
     (assert-type 'bitvector-fold/int (procedure? kons))
     (assert-type 'bitvector-fold/int (every bitvector? bvecs))
     (apply u8vector-fold kons knil (map U bvecs)))))

(: bitvector-fold/bool (procedure * #!rest bitvector -> *))
(define bitvector-fold/bool
  (case-lambda
    ((kons knil bvec)
     (assert-type 'bitvector-fold/bool (procedure? kons))
     (assert-type 'bitvector-fold/bool (bitvector? bvec))
     (u8vector-fold (lambda (x b) (kons x (B b)))  ; fast path
                    knil
                    (U bvec)))
    ((kons knil . bvecs)
     (assert-type 'bitvector-fold/bool (procedure? kons))
     (assert-type 'bitvector-fold/int (every bitvector? bvecs))
     (apply u8vector-fold
            (lambda (x . bits)
              (apply kons x (map bit->boolean bits)))
            knil
            (map U bvecs)))))

(: bitvector-fold-right/int (procedure * #!rest bitvector -> *))
(define bitvector-fold-right/int
  (case-lambda
    ((kons knil bvec)
     (assert-type 'bitvector-fold-right/int (procedure? kons))
     (assert-type 'bitvector-fold-right/int (bitvector? bvec))
     (u8vector-fold-right kons knil (U bvec)))    ; fast path
    ((kons knil . bvecs)
     (assert-type 'bitvector-fold-right/int (procedure? kons))
     (assert-type 'bitvector-fold-right/int (every bitvector? bvecs))
     (apply u8vector-fold-right kons knil (map U bvecs)))))

(: bitvector-fold-right/bool (procedure * #!rest bitvector -> *))
(define bitvector-fold-right/bool
  (case-lambda
    ((kons knil bvec)
     (assert-type 'bitvector-fold-right/bool (procedure? kons))
     (assert-type 'bitvector-fold-right/bool (bitvector? bvec))
     (u8vector-fold-right (lambda (x bit) (kons x (B bit)))  ; fast path
                          knil
                          (U bvec)))
    ((kons knil . bvecs)
     (assert-type 'bitvector-fold-right/bool (procedure? kons))
     (assert-type 'bitvector-fold-right/bool (every bitvector? bvecs))
     (apply u8vector-fold-right
            (lambda (x . bits)
              (apply kons x (map bit->boolean bits)))
            knil
            (map U bvecs)))))

(: bitvector-map/int (procedure #!rest bitvector -> bitvector))
(define bitvector-map/int
  (case-lambda
    ((f bvec)
     (assert-type 'bitvector-map/int (procedure? f))
     (assert-type 'bitvector-map/int (bitvector? bvec))
     (W (u8vector-map f (U bvec))))        ; one-bitvector fast path
    ((f bvec1 bvec2)
     (assert-type 'bitvector-map/int (procedure? f))
     (assert-type 'bitvector-map/int (bitvector? bvec1))
     (assert-type 'bitvector-map/int (bitvector? bvec2))
     (%bitvector-map2/int f bvec1 bvec2))  ; two-bitvector fast path
    ((f . bvecs)
     (assert-type 'bitvector-map/int (procedure? f))
     (assert-type 'bitvector-map/int (every bitvector? bvecs))
     (W (apply u8vector-map f (map U bvecs))))))  ; normal path

;; Tuned two-bitvector version, mainly for binary logical ops.
(define (%bitvector-map2/int f bvec1 bvec2)
  (let ((u8vec1 (U bvec1))
        (u8vec2 (U bvec2)))
    (bitvector-unfold
     (lambda (i)
       (f (u8vector-ref u8vec1 i) (u8vector-ref u8vec2 i)))
     (bitvector-length bvec1))))

(: bitvector-map/bool (procedure #!rest bitvector -> bitvector))
(define bitvector-map/bool
  (case-lambda
    ((f bvec)          ; one-bitvector fast path
     (assert-type 'bitvector-map/bool (procedure? f))
     (assert-type 'bitvector-map/bool (bitvector? bvec))
     (W (u8vector-map (lambda (n) (I (f (B n)))) (U bvec))))
    ((f bvec1 bvec2)   ; two-bitvector fast path
     (assert-type 'bitvector-map/bool (procedure? f))
     (assert-type 'bitvector-map/bool (bitvector? bvec1))
     (assert-type 'bitvector-map/bool (bitvector? bvec2))
     (%bitvector-map2/int (lambda (n m) (I (f (B n) (B m)))) bvec1 bvec2))
    ((f . bvecs)       ; normal path (ugh)
     (assert-type 'bitvector-map/bool (procedure? f))
     (assert-type 'bitvector-map/bool (every bitvector? bvecs))
     (W (apply u8vector-map
               (lambda ns (I (apply f (map bit->boolean ns))))
               (map U bvecs))))))

(: bitvector-map!/int (procedure #!rest bitvector -> undefined))
(define bitvector-map!/int
  (case-lambda
    ((f bvec)
     (assert-type 'bitvector-map!/int (procedure? f))
     (assert-type 'bitvector-map!/int (bitvector? bvec))
     (u8vector-map! f (U bvec)))            ; one-bitvector fast path
    ((f bvec1 bvec2)
     (assert-type 'bitvector-map!/int (procedure? f))
     (assert-type 'bitvector-map!/int (bitvector? bvec1))
     (assert-type 'bitvector-map!/int (bitvector? bvec2))
     (%bitvector-map2!/int f bvec1 bvec2))  ; two-bitvector fast path
    ((f . bvecs)
     (assert-type 'bitvector-map!/int (procedure? f))
     (assert-type 'bitvector-map!/int (every bitvector? bvecs))
     (apply u8vector-map! f (map U bvecs)))))  ; normal path

;; Tuned two-bitvector version, mainly for binary logical ops.
(define (%bitvector-map2!/int f bvec1 bvec2)
  (let ((len (bitvector-length bvec1))
        (u8vec1 (U bvec1))
        (u8vec2 (U bvec2)))
    (let lp ((i 0))
      (unless (>= i len)
        (u8vector-set! u8vec1 i (f (u8vector-ref u8vec1 i)
                                   (u8vector-ref u8vec2 i)))
        (lp (+ i 1))))
    bvec1))

(: bitvector-map!/bool (procedure #!rest bitvector -> undefined))
(define bitvector-map!/bool
  (case-lambda
    ((f bvec)          ; one-bitvector fast path
     (assert-type 'bitvector-map!/bool (procedure? f))
     (assert-type 'bitvector-map!/bool (bitvector? bvec))
     (u8vector-map! (lambda (n) (I (f (B n)))) (U bvec)))
    ((f bvec1 bvec2)   ; two-bitvector fast path
     (assert-type 'bitvector-map!/bool (procedure? f))
     (assert-type 'bitvector-map!/bool (bitvector? bvec1))
     (assert-type 'bitvector-map!/bool (bitvector? bvec2))
     (%bitvector-map2!/int (lambda (n m) (I (f (B n) (B m)))) bvec1 bvec2))
    ((f . bvecs)       ; normal path (ugh)
     (assert-type 'bitvector-map!/bool (procedure? f))
     (assert-type 'bitvector-map!/bool (every bitvector? bvecs))
     (apply u8vector-map!
            (lambda ns (I (apply f (map bit->boolean ns))))
            (map U bvecs)))))

(: bitvector-for-each/int (procedure #!rest bitvector -> undefined))
(define bitvector-for-each/int
  (case-lambda
    ((f bvec)
     (assert-type 'bitvector-for-each/int (procedure? f))
     (assert-type 'bitvector-for-each/int (bitvector? bvec))
     (u8vector-for-each f (U bvec)))    ; fast path
    ((f . bvecs)
     (assert-type 'bitvector-for-each/int (procedure? f))
     (assert-type 'bitvector-for-each/int (every bitvector? bvecs))
     (apply u8vector-for-each f (map U bvecs)))))

(: bitvector-for-each/bool (procedure #!rest bitvector -> undefined))
(define bitvector-for-each/bool
  (case-lambda
    ((f bvec)
     (assert-type 'bitvector-for-each/bool (procedure? f))
     (assert-type 'bitvector-for-each/bool (bitvector? bvec))
     (u8vector-for-each (lambda (n) (f (B n))) (U bvec)))    ; fast path
    ((f . bvecs)
     (assert-type 'bitvector-for-each/bool (procedure? f))
     (assert-type 'bitvector-for-each/bool (every bitvector? bvecs))
     (apply u8vector-for-each
            (lambda ns (apply f (map bit->boolean ns)))
            (map U bvecs)))))

(: bitvector-set! (bitvector integer bit -> undefined))
(define (bitvector-set! bvec i bit)
  (assert-type 'bitvector-set! (bitvector? bvec))
  (assert-type 'bitvector-set! (exact-integer? i))
  (assert-type 'bitvector-set! (%bit? bit))
  (%check-index 'bitvector-set! bvec i)
  (u8vector-set! (U bvec) i (I bit)))

(: bitvector-swap! (bitvector integer integer -> undefined))
(define (bitvector-swap! bvec i j)
  (assert-type 'bitvector-swap! (bitvector? bvec))
  (assert-type 'bitvector-swap! (exact-integer? i))
  (assert-type 'bitvector-swap! (exact-integer? j))
  (%check-index 'bitvector-swap! bvec i)
  (%check-index 'bitvector-swap! bvec j)
  (u8vector-swap! (U bvec) i j))

(: bitvector-reverse! (bitvector #!optional integer integer -> undefined))
(define bitvector-reverse!
  (case-lambda
    ((bvec)
     (assert-type 'bitvector-reverse! (bitvector? bvec))
     (u8vector-reverse! (U bvec)))
    ((bvec start)
     (assert-type 'bitvector-reverse! (bitvector? bvec))
     (assert-type 'bitvector-reverse! (exact-integer? start))
     (%check-index 'bitvector-reverse! bvec start)
     (u8vector-reverse! (U bvec) start))
    ((bvec start end)
     (assert-type 'bitvector-reverse! (bitvector? bvec))
     (assert-type 'bitvector-reverse! (exact-integer? start))
     (assert-type 'bitvector-reverse! (exact-integer? end))
     (%check-range 'bitvector-reverse! bvec start end)
     (u8vector-reverse! (U bvec) start end))))

(: bitvector-copy!
   (bitvector integer bitvector #!optional integer integer -> undefined))
(define bitvector-copy!
  (case-lambda
    ((to at from)
     (bitvector-copy! to at from 0 (bitvector-length from)))
    ((to at from start)
     (bitvector-copy! to at from start (bitvector-length from)))
    ((to at from start end)
     (assert-type 'bitvector-copy! (bitvector? to))
     (assert-type 'bitvector-copy! (exact-integer? at))
     (assert-type 'bitvector-copy! (bitvector? from))
     (assert-type 'bitvector-copy! (exact-integer? start))
     (assert-type 'bitvector-copy! (exact-integer? end))
     (%check-index 'bitvector-copy! to at)
     (%check-range 'bitvector-copy! from start end)
     (u8vector-copy! (U to) at (U from) start end))))

(: bitvector-reverse-copy!
   (bitvector integer bitvector #!optional integer integer -> undefined))
(define bitvector-reverse-copy!
  (case-lambda
    ((to at from)
     (bitvector-reverse-copy! to at from 0 (bitvector-length from)))
    ((to at from start)
     (bitvector-reverse-copy! to at from start (bitvector-length from)))
    ((to at from start end)
     (assert-type 'bitvector-reverse-copy! (bitvector? to))
     (assert-type 'bitvector-reverse-copy! (exact-integer? at))
     (assert-type 'bitvector-reverse-copy! (bitvector? from))
     (assert-type 'bitvector-reverse-copy! (exact-integer? start))
     (assert-type 'bitvector-reverse-copy! (exact-integer? end))
     (%check-index 'bitvector-reverse-copy! to at)
     (%check-range 'bitvector-reverse-copy! from start end)
     (u8vector-reverse-copy! (U to) at (U from) start end))))

(: bitvector->list/int (bitvector integer integer -> (list-of integer)))
(define bitvector->list/int
  (case-lambda
    ((bvec)
     (assert-type 'bitvector->list/int (bitvector? bvec))
     (u8vector->list (U bvec)))
    ((bvec start)
     (bitvector->list bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'bitvector->list/int (bitvector? bvec))
     (assert-type 'bitvector->list/int (exact-integer? start))
     (assert-type 'bitvector->list/int (exact-integer? end))
     (%check-range 'bitvector->list/int bvec start end)
     (u8vector->list (U bvec) start end))))

(: bitvector->list/bool (bitvector integer integer -> (list-of boolean)))
(define bitvector->list/bool
  (case-lambda
    ((bvec)
     (assert-type 'bitvector->list/bool (bitvector? bvec))
     (map bit->boolean (u8vector->list (U bvec))))
    ((bvec start)
     (bitvector->list/bool bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'bitvector->list/bool (bitvector? bvec))
     (assert-type 'bitvector->list/bool (exact-integer? start))
     (assert-type 'bitvector->list/bool (exact-integer? end))
     (%check-range 'bitvector->list/bool bvec start end)
     (map bit->boolean (u8vector->list (U bvec) start end)))))

(: reverse-bitvector->list/int
   (bitvector #!optional integer integer -> (list-of integer)))
(define reverse-bitvector->list/int
  (case-lambda
    ((bvec)
     (assert-type 'reverse-bitvector->list/int (bitvector? bvec))
     (reverse-u8vector->list (U bvec)))
    ((bvec start)
     (reverse-bitvector->list bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'reverse-bitvector->list/int (bitvector? bvec))
     (assert-type 'reverse-bitvector->list/int (exact-integer? start))
     (assert-type 'reverse-bitvector->list/int (exact-integer? end))
     (%check-range 'reverse-bitvector->list/int bvec start end)
     (reverse-u8vector->list (U bvec) start end))))

(: reverse-bitvector->list/bool
   (bitvector #!optional integer integer -> (list-of boolean)))
(define reverse-bitvector->list/bool
  (case-lambda
    ((bvec)
     (assert-type 'reverse-bitvector->list/bool (bitvector? bvec))
     (map bit->boolean (reverse-u8vector->list (U bvec))))
    ((bvec start)
     (reverse-bitvector->list/bool bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'reverse-bitvector->list/bool (bitvector? bvec))
     (assert-type 'reverse-bitvector->list/bool (exact-integer? start))
     (assert-type 'reverse-bitvector->list/bool (exact-integer? end))
     (%check-range 'reverse-bitvector->list/bool bvec start end)
     (map bit->boolean (reverse-u8vector->list (U bvec) start end)))))

(: bitvector->vector/int
   (bitvector #!optional integer integer -> (vector-of integer)))
(define bitvector->vector/int
  (case-lambda
    ((bvec)
     (assert-type 'bitvector->vector/int (bitvector? bvec))
     (u8vector->vector (U bvec)))
    ((bvec start)
     (bitvector->vector/int bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'bitvector->vector/int (bitvector? bvec))
     (assert-type 'bitvector->vector/int (exact-integer? start))
     (assert-type 'bitvector->vector/int (exact-integer? end))
     (%check-range 'bitvector->vector/int bvec start end)
     (u8vector->vector (U bvec) start end))))

(: bitvector->vector/bool
   (bitvector #!optional integer integer -> (vector-of boolean)))
(define bitvector->vector/bool
  (case-lambda
    ((bvec)
     (assert-type 'bitvector->vector/bool (bitvector? bvec))
     (vector-map bit->boolean (u8vector->vector (U bvec))))
    ((bvec start)
     (bitvector->vector/bool bvec start (bitvector-length bvec)))
    ((bvec start end)
     (assert-type 'bitvector->vector/bool (bitvector? bvec))
     (assert-type 'bitvector->vector/bool (exact-integer? start))
     (assert-type 'bitvector->vector/bool (exact-integer? end))
     (%check-range 'bitvector->vector/bool bvec start end)
     (vector-map bit->boolean (u8vector->vector (U bvec) start end)))))

(: list->bitvector ((list-of bit) -> bitvector))
(define (list->bitvector list)
  (assert-type 'list->bitvector (pair-or-null? list))
  (W (list->u8vector (map bit->integer list))))

(: reverse-list->bitvector ((list-of bit) -> bitvector))
(define (reverse-list->bitvector list)
  (assert-type 'reverse-list->bitvector (pair-or-null? list))
  (W (reverse-list->u8vector (map bit->integer list))))

(: bitvector (#!rest bit -> bitvector))
(define (bitvector . bits) (list->bitvector bits))

(: vector->bitvector
   ((vector-of bit) #!optional integer integer -> bitvector))
(define vector->bitvector
  (case-lambda
    ((vec)
     (assert-type 'vector->bitvector (vector? vec))
     (W (vector->u8vector (vector-map bit->integer vec))))
    ((vec start)
     (vector->bitvector vec start (vector-length vec)))
    ((vec start end)
     (assert-type 'vector->bitvector (vector? vec))
     (assert-type 'vector->bitvector (exact-integer? start))
     (assert-type 'vector->bitvector (exact-integer? end))
     (unless (<= 0 start end (vector-length vec))
       (bounds-exception 'vector->bitvector
                         "invalid range"
                         start
                         end
                         vec))
     (W (vector->u8vector (vector-map bit->integer vec) start end)))))
