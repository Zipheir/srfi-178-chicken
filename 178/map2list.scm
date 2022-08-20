(: bitvector-map->list/int
   (procedure #!rest bitvector -> (list-of integer)))
(define bitvector-map->list/int
  (case-lambda
    ((f bvec)                    ; fast path
     (assert-type 'bitvector-map->list/int (procedure? f))
     (assert-type 'bitvector-map->list/int (bitvector? bvec))
     (bitvector-fold-right/int (lambda (xs b) (cons (f b) xs))
                               '()
                               bvec))
    ((f . bvecs)
     (assert-type 'bitvector-map->list/int (procedure? f))
     (assert-type 'bitvector-map->list/int (every bitvector? bvecs))
     (when (null? bvecs)
       (arity-exception 'bitvector-map->list/int bvecs))
     (apply bitvector-fold-right/int
            (lambda (xs . bs) (cons (apply f bs) xs))
            '()
            bvecs))))

(: bitvector-map->list/bool
   (procedure #!rest bitvector -> (list-of boolean)))
(define bitvector-map->list/bool
  (case-lambda
    ((f bvec)                    ; fast path
     (assert-type 'bitvector-map->list/bool (procedure? f))
     (assert-type 'bitvector-map->list/bool (bitvector? bvec))
     (bitvector-fold-right/bool (lambda (xs b) (cons (f b) xs))
                                '()
                                bvec))
    ((f . bvecs)
     (assert-type 'bitvector-map->list/bool (procedure? f))
     (assert-type 'bitvector-map->list/bool (every bitvector? bvecs))
     (when (null? bvecs)
       (arity-exception 'bitvector-map->list/bool bvecs))
     (apply bitvector-fold-right/bool
            (lambda (xs . bs) (cons (apply f bs) xs))
            '()
            bvecs))))

