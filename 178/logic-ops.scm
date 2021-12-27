(: u1-not (fixnum -> fixnum))
(define (u1-not a)
  (- 1 a))

(: bitvector-not (bitvector -> bitvector))
(define (bitvector-not avec)
  (bitvector-map/int u1-not avec))

(: bitvector-not! (bitvector -> undefined))
(define (bitvector-not! avec)
  (bitvector-map!/int u1-not avec))

(: u1-and (#!rest fixnum -> fixnum))
(define (u1-and . args)
  (I (apply * args)))

(: bitvector-and (#!rest bitvector -> bitvector))
(define (bitvector-and  . vecs)
  (apply bitvector-map/int u1-and vecs))

(: bitvector-and! (#!rest bitvector -> undefined))
(define (bitvector-and!  . vecs)
  (apply bitvector-map!/int u1-and vecs))

(: u1-ior (#!rest fixnum -> fixnum))
(define (u1-ior . args)
  (I (apply + args)))

(: bitvector-ior (#!rest bitvector -> bitvector))
(define (bitvector-ior . vecs)
  (apply bitvector-map/int u1-ior vecs))

(: bitvector-ior! (#!rest bitvector -> undefined))
(define (bitvector-ior! . vecs)
  (apply bitvector-map!/int u1-ior vecs))

(: u1-xor (#!rest fixnum -> fixnum))
(define (u1-xor . args)
  (I (odd? (apply + args))))

(: bitvector-xor (#!rest bitvector -> bitvector))
(define (bitvector-xor . vecs)
  (apply bitvector-map/int u1-xor vecs))

(: bitvector-xor! (#!rest bitvector -> undefined))
(define (bitvector-xor! . vecs)
  (apply bitvector-map!/int u1-xor vecs))

(: u1-eqv (#!rest fixnum -> fixnum))
(define (u1-eqv . args)
  (let ((xor-value (apply u1-xor args)))
    (if (odd? (length args))
      xor-value
      (u1-not xor-value))))

(: bitvector-eqv (#!rest bitvector -> bitvector))
(define (bitvector-eqv . vecs)
  (apply bitvector-map/int u1-eqv vecs))

(: bitvector-eqv (#!rest bitvector -> undefined))
(define (bitvector-eqv! . vecs)
  (apply bitvector-map!/int u1-eqv vecs))

(: u1-nand (fixnum fixnum -> fixnum))
(define (u1-nand a b)
  (u1-not (u1-and a b)))

(: bitvector-nand (bitvector bitvector -> bitvector))
(define (bitvector-nand a b)
  (bitvector-map/int u1-nand a b))

(: bitvector-nand! (bitvector bitvector -> undefined))
(define (bitvector-nand! a b)
  (bitvector-map!/int u1-nand a b))

(: u1-nor (fixnum fixnum -> fixnum))
(define (u1-nor a b)
  (u1-not (u1-ior a b)))

(: bitvector-nor (bitvector bitvector -> bitvector))
(define (bitvector-nor a b)
  (bitvector-map/int u1-nor a b))

(: bitvector-nor! (bitvector bitvector -> undefined))
(define (bitvector-nor! a b)
  (bitvector-map!/int u1-nor a b))

(: u1-andc1 (fixnum fixnum -> fixnum))
(define (u1-andc1 a b)
  (u1-and (u1-not a) b))

(: bitvector-andc1 (bitvector bitvector -> bitvector))
(define (bitvector-andc1 a b)
  (bitvector-map/int u1-andc1 a b))

(: bitvector-andc1! (bitvector bitvector -> undefined))
(define (bitvector-andc1! a b)
  (bitvector-map!/int u1-andc1 a b))

(: u1-andc2 (fixnum fixnum -> fixnum))
(define (u1-andc2 a b)
  (u1-and a (u1-not b)))

(: bitvector-andc2 (bitvector bitvector -> bitvector))
(define (bitvector-andc2 a b)
  (bitvector-map/int u1-andc2 a b))

(: bitvector-andc2! (bitvector bitvector -> undefined))
(define (bitvector-andc2! a b)
  (bitvector-map!/int u1-andc2 a b))

(: u1-orc1 (fixnum fixnum -> fixnum))
(define (u1-orc1 a b)
  (u1-ior (u1-not a) b))

(: bitvector-orc1 (bitvector bitvector -> bitvector))
(define (bitvector-orc1 a b)
  (bitvector-map/int u1-orc1 a b))

(: bitvector-orc1! (bitvector bitvector -> undefined))
(define (bitvector-orc1! a b)
  (bitvector-map!/int u1-orc1 a b))

(: u1-orc2 (fixnum fixnum -> fixnum))
(define (u1-orc2 a b)
  (u1-ior a (u1-not b)))

(: bitvector-orc2 (bitvector bitvector -> bitvector))
(define (bitvector-orc2 a b)
  (bitvector-map/int u1-orc2 a b))

(: bitvector-orc2! (bitvector bitvector -> undefined))
(define (bitvector-orc2! a b)
  (bitvector-map!/int u1-orc2 a b))

