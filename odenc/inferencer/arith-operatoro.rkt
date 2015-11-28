#lang racket

(provide arith-operatoro)

(require Racket-miniKanren/miniKanren/mk)

(define (appendo l s out)
  (conde
   ((== '() l) (== s out))
   ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res)))))

(define (membero x y)
  (fresh [a b]
         (appendo a `(,x . ,b) y)))

;; TODO: need some better way to do polymorphic operators
(define (arith-operatoro o t)
  (fresh (ot)
         (conde
          [(== '+ o)
           (membero ot '(int float string))
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(membero o '(- * /))
           (membero ot '(int float))
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(== '% o )
           (== ot 'int)
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(membero o '(== !=))
           (membero ot '(int float string))
           (== `(,ot -> (,ot -> bool)) t)]
          [(membero o '(> < >= <=))
           (membero ot '(int float))
           (== `(,ot -> (,ot -> bool)) t)])))
