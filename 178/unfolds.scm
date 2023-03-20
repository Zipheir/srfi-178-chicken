;;;; unfold

;;; These procedures work by building temporary lists, then converting
;;; them to vectors. This uses more space than pre-allocating a bitvector
;;; and filling it, but it's referentially transparent: there's no way
;;; to capture a partially-filled bitvector through continuation tricks.

;; Unfold a list. f is passed the current index and list of seeds
;; on each step, and must return a bit and new seeds on each step.
(define (%unfold/index f len seeds)
  (letrec
   ((build
     (lambda (i seeds)
       (if (= i len)
           '()
           (let-values (((b . seeds*) (apply f i seeds)))
             (cons b (build (+ i 1) seeds*)))))))

    (build 0 seeds)))

(: bitvector-unfold (procedure integer #!rest * -> bitvector))
(define bitvector-unfold
  (case-lambda
    ((f len)
     (assert-type 'bitvector-unfold (procedure? f))
     (assert-type 'bitvector-unfold (exact-natural? len))
     (list->bitvector (list-tabulate len f)))
    ((f len seed . rest)
     (assert-type 'bitvector-unfold (procedure? f))
     (assert-type 'bitvector-unfold (exact-natural? len))
     (list->bitvector (%unfold/index f len (cons seed rest))))))

;; Since several procedures are implemented in terms of
;; bitvector-unfold, here's a zero-or-one-seed version
;; with no checks.
(: %bitvector-unfold-no-checks
   (procedure fixnum #!optional * -> bitvector))
(define %bitvector-unfold-no-checks
  (case-lambda
    ((f len) (list->bitvector (list-tabulate len f)))
    ((f len seed)
     (letrec
      ((build
        (lambda (i seed)
          (if (= i len)
              '()
              (let-values (((b seed*) (f i seed)))
                (cons b (build (+ i 1) seed*)))))))

       (list->bitvector (build 0 seed))))))

;;;; unfold-right

;; Unfold a list from the right. f is passed the current index and
;; list of seeds on each step, and must return a bit and new seeds
;; on each step.
(define (%unfold-right/index f len seeds)
  (letrec
   ((build
     (lambda (i seeds res)
       (if (< i 0)
           res
           (let-values (((b . seeds*) (apply f i seeds)))
             (build (- i 1) seeds* (cons b res)))))))

    (build (- len 1) seeds '())))

(: bitvector-unfold-right (procedure integer #!rest * -> bitvector))
(define (bitvector-unfold-right f len . seeds)
  (assert-type 'bitvector-unfold-right (procedure? f))
  (assert-type 'bitvector-unfold-right (exact-natural? len))
  (list->bitvector (%unfold-right/index f len seeds)))
