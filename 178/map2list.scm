(: bitvector-map->list/int
   (procedure #!rest bitvector -> (list-of fixnum)))
(define bitvector-map->list/int
  (case-lambda
    ((f bvec)                    ; fast path
     (bitvector-fold-right/int (lambda (xs b) (cons (f b) xs))
                               '()
                               bvec))
    ((f . bvecs)
     (apply bitvector-fold-right/int
            (lambda (xs . bs) (cons (apply f bs) xs))
            '()
            bvecs))))

(: bitvector-map->list/bool
   (procedure #!rest bitvector -> (list-of boolean)))
(define bitvector-map->list/bool
  (case-lambda
    ((f bvec)                    ; fast path
     (bitvector-fold-right/bool (lambda (xs b) (cons (f b) xs))
                                '()
                                bvec))
    ((f . bvecs)
     (apply bitvector-fold-right/bool
            (lambda (xs . bs) (cons (apply f bs) xs))
            '()
            bvecs))))

