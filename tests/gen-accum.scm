(test-group "Generators and accumulators"
  (define test-bvec (bitvector 1 0 1 1 0 1 0 1))

  ;;; Generators

  (check (eof-object? ((make-bitvector/int-generator (bitvector)))) => #t)
  (check (eof-object? ((make-bitvector/bool-generator (bitvector)))) => #t)
  (check (bitvector=
          (bitvector-unfold (lambda (_ g) (values (g) g))
                            (bitvector-length test-bvec)
                            (make-bitvector/int-generator test-bvec))
          test-bvec)
   => #t)
  (check (bitvector=
          (bitvector-unfold (lambda (_ g) (values (g) g))
                            (bitvector-length test-bvec)
                            (make-bitvector/bool-generator test-bvec))
          test-bvec)
   => #t)

  ;;; Accumulator

  (check (bitvector-empty? ((make-bitvector-accumulator) #!eof))
   => #t)
  ;; Accumulate integers.
  (check (bitvector= test-bvec
                     (let ((acc (make-bitvector-accumulator)))
                       (bitvector-for-each/int acc test-bvec)
                       (acc #!eof)))
   => #t)
  ;; Accumulate booleans.
  (check (bitvector= test-bvec
                     (let ((acc (make-bitvector-accumulator)))
                       (bitvector-for-each/bool acc test-bvec)
                       (acc #!eof)))
   => #t)

  ;;; Generator/accumulator identities

  ;; Accumulating generated values yields the original structure.
  (check (bitvector=
          (let ((gen (make-bitvector/int-generator test-bvec))
                (acc (make-bitvector-accumulator)))
            (generator-for-each acc gen)
            (acc #!eof))
          test-bvec)
   => #t)

  ;; Generating accumulated values yields the original values.
  ;; Integer generator.
  (let ((lis (bitvector->list/int test-bvec)))
    (check (equal?
            (let ((acc (make-bitvector-accumulator)))
              (for-each acc lis)
              (generator->list
               (make-bitvector/int-generator (acc #!eof))))
            lis)
     => #t))
  ;; Boolean generator.
  (let ((lis (bitvector->list/bool test-bvec)))
    (check (equal?
            (let ((acc (make-bitvector-accumulator)))
              (for-each acc lis)
              (generator->list
               (make-bitvector/bool-generator (acc #!eof))))
            lis)
     => #t))
)
