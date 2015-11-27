#lang racket

(provide (all-defined-out))

(require unstable/hash)
(require "make-monomorph-name.rkt")
(require "instantiate-p-def.rkt")

(struct
  monomorphed-def
  (name instance-name def)
  #:transparent)

(define (hash-replace v1 v2) v2)

(define/contract (monomorph pdefs expr [monomorphed (hash)] [local '()])
  (->* (hash? any/c)
       ((hash/c (list/c symbol? any/c) monomorphed-def?) (listof symbol?))
       (list/c any/c (hash/c (list/c symbol? any/c) monomorphed-def?)))
  (match expr
    [`(,(? symbol? s) : ,t)
     (if (and (not (member s local))
              (hash-has-key? pdefs s))
         
         ;; this was a reference to a p-def
         (let* ([key (list s t)]
                [mname (make-monomorph-name s t)])
           (if (hash-has-key? monomorphed key)
               ;; already monomorphed
               `((,mname : ,t) monomorphed)
               
               ;; monomorph the p-def recursively
               (let* ([p-def (hash-ref pdefs s)]
                      [instance (instantiate-p-def p-def t mname)])
                 ;; TODO: if the p-def is calling itself recursively this should never exit?
                 (match (monomorph pdefs instance monomorphed '())
                   [`(,def ,m)
                    `((,mname : ,t) ,(hash-set m key (monomorphed-def s mname def)))]))))

         ;; not a p-def reference
         `(,expr ,monomorphed))]
    [`(define ,name ,e)
     (match (monomorph pdefs e monomorphed local)
       [`(,m-expr ,m) `((define ,name ,m-expr) ,m)])]
    [(? number?) `(,expr ,monomorphed)]
    [(? string?) `(,expr ,monomorphed)]
    [`(,e : ,t)
     (let ([sub (monomorph pdefs e monomorphed local)])
       `((,(car sub) : ,t) ,(cadr sub)))]
    [`(let ([,name ,value]) ,body)
     (let* ([value-sub (monomorph pdefs value monomorphed local)]
            [body-sub (monomorph pdefs body monomorphed (cons name local))]
            [all-monomorphed (hash-union (cadr value-sub) (cadr body-sub) #:combine hash-replace)])
       `((let ([,name ,(car value-sub)])
           ,(car body-sub))
         ,all-monomorphed))]
    [`(fn (,a) ,b)
     (let ([b-sub (monomorph pdefs b monomorphed (cons a local))])
       `((fn (,a) ,(car b-sub))
         ,(cadr b-sub)))]
    [`(fn () ,b)
     (let ([b-sub (monomorph pdefs b monomorphed local)])
       `((fn () ,(car b-sub))
         ,(cadr b-sub)))]
    [`(,f ,a)
     (let* ([f-sub (monomorph pdefs f monomorphed local)]
            [a-sub (monomorph pdefs a monomorphed local)]
            [all-monomorphed (hash-union (cadr f-sub) (cadr a-sub) #:combine hash-replace)])
       `((,(car f-sub) ,(car a-sub)) ,all-monomorphed))]
    [`(if ,c ,a ,b)
     (let* ([c-sub (monomorph pdefs c monomorphed local)]
            [a-sub (monomorph pdefs a monomorphed local)]
            [b-sub (monomorph pdefs b monomorphed local)]
            [all-monomorphed (hash-union (cadr c-sub) (cadr a-sub) (cadr b-sub) #:combine hash-replace)])
       `((if ,(car c-sub) ,(car a-sub) ,(car b-sub))
         ,all-monomorphed))]
    [e (error (format "Cannot monomorph expr: ~v" e))]))

(module+ test
  (require rackunit)
  (require "../inferencer.rkt")

  (let* ([env '((identity : ((var a) -> (var a)))
                (identity-again : ((var a) -> (var a))))]
         [pdefs (hash 'identity
                      (infer-def '(define identity (fn (x) x)))
                      'identity-again
                      (infer-def '(define identity-again (fn (x) (identity x))) env))])
    
    (test-case "one instance of a fn"
      (check-match
       (monomorph pdefs
                  (infer '(fn ([y : int]) (identity y)) env))
       `((,e : (int -> int))
         ,(hash-table
           ('(identity (int -> int))
            (monomorphed-def 'identity
                             'identity_inst_int_to_int
                             `((define identity_inst_int_to_int ,expr) : (int -> int))))))))

    (test-case "two different instances of one fn"
      (check-match
       (monomorph pdefs
                  (infer '(fn ([y : int]) ((identity identity) y)) env))
       `((,e : (int -> int))
         ,(hash-table
           ('(identity (int -> int))
            (monomorphed-def 'identity
                             'identity_inst_int_to_int
                             `((define identity_inst_int_to_int ,name) : (int -> int))))
           ('(identity ((int -> int) -> (int -> int)))
            (monomorphed-def 'identity
                             'identity_inst_int__to__int_to_int__to__int
                             `((define identity_inst_int__to__int_to_int__to__int ,expr)
                               :
                               ((int -> int) -> (int -> int)))))))))


    (test-case "two calls with one instance of a fn"
      (check-match
       (monomorph pdefs
                  (infer '(fn ([y : int]) (identity (identity y))) env))
       `((,e : (int -> int))
         ,(hash-table
           ('(identity (int -> int))
            (monomorphed-def 'identity
                             'identity_inst_int_to_int
                             `((define identity_inst_int_to_int ,expr) : (int -> int))))))))

    (test-case "multiple levels of p-def calls"
      (check-match
       (monomorph pdefs
                  (infer '(fn ([y : int]) (identity-again y)) env))
       `((,e : (int -> int))
         ,(hash-table
           ('(identity (int -> int))
            (monomorphed-def 'identity
                             'identity_inst_int_to_int
                             _))           
           ('(identity-again (int -> int))
            (monomorphed-def 'identity-again
                             'identity-again_inst_int_to_int
                             _))))))))
