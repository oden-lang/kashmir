#lang racket

(define (explode expr)
  (match expr
    [`(lambda () ,body)
     `(lambda () ,(explode body))]
    [`(lambda (,arg) ,body)
     `(lambda (,arg) ,(explode body))]
    [`(lambda ,args ,body)
     (explode `(lambda ,(list (car args)) ,(explode `(lambda ,(cdr args) ,body))))]
    [`(let ((,s ,v)) ,b)
     `(let ((,s ,(explode v))) ,(explode b))]
    [`(let ,ps ,b)
     `(let ,(list (car ps)) ,(explode `(let ,(cdr ps) ,b)))]
    [`(if ,c ,a ,b)
     `(if ,(explode c) ,(explode a) ,(explode b))]
    [`(,e : ,t)
     `(,(explode e) : ,t)]
    [`(,f) `(,(explode f))]
    [`(,f ,a) `(,(explode f) ,(explode a))]
    ;; (f x y z) -> (((f x) y) z)
    [`(,f . ,args)
     (explode `((,f ,(car args)) . ,(cdr args)))]
    [e e]))

(define (explode-definition d)
  (match d
    [`(define ,(? symbol? name) ,expr)
     `(define ,name ,(explode expr))]
    [`(define (,(? symbol? name) . ,args) ,body)
     `(define ,name ,(explode `(lambda ,args ,body)))]))

(provide
 explode
 explode-definition)