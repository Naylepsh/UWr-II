#lang racket

( define-signature monoid^
   (( contracted
      [ elem? (-> any/c boolean?)]
      [ neutral elem?]
      [ oper (-> elem? elem? elem?) ])))

(define-unit monoid-list@ ;; monoid-list
  (import)
  (export monoid^)

  (define elem? list?) ;; dla listy (define elem? list)
  (define neutral null)      ;; (define neutral null)
  (define oper append))        ;; (define oper append)

(define-values/invoke-unit/infer monoid-list@)
;(oper 1 2)

(require quickcheck)

(quickcheck
 (property
  ([x (arbitrary-list arbitrary-integer)] ;; dla list: [xs arbitrary-list]
   [y (arbitrary-list arbitrary-integer)]
   [z (arbitrary-list arbitrary-integer)])
  (and (and (equal? (oper neutral x) x) (equal? (oper x neutral) x))
       (equal? (oper (oper x y) z)
               (oper x (oper y z))))))