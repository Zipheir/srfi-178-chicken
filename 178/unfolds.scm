;;;; unfold

;; Zero-seed fast path.
(: %bitvector-tabulate ((fixnum -> fixnum) fixnum -> bitvector))
(define (%bitvector-tabulate f len)
  (let ((res (make-u8vector len)))
    (let lp ((i 0))
    (cond ((= i len) (W res))
          (else (u8vector-set! res i (I (f i)))
                (lp (+ i 1)))))))

;; One-seed fast path.
(: %bitvector-unfold-1 (procedure fixnum * -> bitvector))
(define (%bitvector-unfold-1 f len seed)
  (let ((res (make-u8vector len)))
    (let lp ((i 0) (seed seed))
      (if (= i len)
          (W res)
          (let-values (((b seed*) (f i seed)))
            (u8vector-set! res i (I b))
            (lp (+ i 1) seed*))))))

(: bitvector-unfold (procedure fixnum #!rest * -> bitvector))
(define bitvector-unfold
  (case-lambda
   ((f len)
    (assert (procedure? f))
    (assert (exact-natural? len))
    (%bitvector-tabulate f len))
   ((f len seed)
    (assert (procedure? f))
    (assert (exact-natural? len))
    (%bitvector-unfold-1 f len seed))
   ((f len . seeds)
    (assert (procedure? f))
    (assert (exact-natural? len))
    (let ((res (make-u8vector len)))
      (let lp ((i 0) (seeds seeds))
        (if (= i len)
            (W res)
            (let-values (((b . seeds*) (apply f i seeds)))
              (u8vector-set! res i (I b))
              (lp (+ i 1) seeds*))))))))

;;;; unfold-right

;; Zero-seed fast path.
(: %bitvector-tabulate-right ((fixnum -> fixnum) fixnum -> bitvector))
(define (%bitvector-tabulate-right f len)
  (let ((res (make-u8vector len)))
    (let lp ((i (- len 1)))
      (cond ((< i 0) (W res))
            (else (u8vector-set! res i (I (f i)))
                  (lp (- i 1)))))))

;; One-seed fast path.
(: %bitvector-unfold-1-right (procedure fixnum * -> bitvector))
(define (%bitvector-unfold-1-right f len seed)
  (let ((result (make-u8vector len)))
    (let lp ((i (- len 1)) (seed seed))
      (if (< i 0)
          (W result)
          (let-values (((b seed*) (f i seed)))
            (u8vector-set! result i (I b))
            (lp (- i 1) seed*))))))

(: bitvector-unfold-right (procedure fixnum #!rest * -> bitvector))
(define bitvector-unfold-right
  (case-lambda
   ((f len)
    (assert (procedure? f))
    (assert (exact-natural? len))
    (%bitvector-tabulate-right f len))
   ((f len seed)
    (assert (procedure? f))
    (assert (exact-natural? len))
    (%bitvector-unfold-1-right f len seed))
   ((f len . seeds)
    (assert (procedure? f))
    (assert (exact-natural? len))
    (let ((res (make-u8vector len)))
      (let lp ((i (- len 1)) (seeds seeds))
        (if (< i 0)
            (W res)
            (let-values (((b . seeds*) (apply f i seeds)))
              (u8vector-set! res i (I b))
              (lp (- i 1) seeds*))))))))
