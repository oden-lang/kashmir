#lang racket

(provide
 explode
 explode-definition)

(define (explode expr)
  (match expr
    [`(fn () ,body)
     `(fn () ,(explode body))]
    [`(fn (,arg) ,body)
     `(fn (,arg) ,(explode body))]
    [`(fn ,args ,body)
     (explode `(fn ,(list (car args)) ,(explode `(fn ,(cdr args) ,body))))]
    [`(let ((,s ,v)) ,b)
     `(let ((,s ,(explode v))) ,(explode b))]
    [`(let ((,s ,v) ,ps ...) ,b)
     `(let ((,s ,(explode v))) ,(explode `(let ,ps ,b)))]
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
     `(define ,name ,(explode `(fn ,args ,body)))]))

(module+ test
  (require rackunit)

  (test-case "literal"
    (check-equal?
     (explode 'foo)
     'foo))

   (test-case "type-annotated literal"
    (check-equal?
     (explode '(123 : int))
     '(123 : int)))

   (test-case "type-annotated complex expression"
    (check-equal?
     (explode '((+ 1 2) : int))
     '(((+ 1) 2) : int)))

   (test-case "simple let expression"
    (check-equal?
     (explode '(let ((n 1)) n))
     '(let ((n 1)) n)))

   (test-case "simple double let expression"
     (check-equal?
      (explode '(let ((n 1) (m 2)) (+ n m)))
      '(let ((n 1)) (let ((m 2)) ((+ n) m)))))

   (test-case "single complex let expression"
    (check-equal?
     (explode '(let ((n (+ 1 2))) n))
     '(let ((n ((+ 1) 2))) n)))

   (test-case "double complex let expression"
     (check-equal?
      (explode '(let ((n (+ 1 2)) (m 5)) (+ n m)))
      '(let ((n ((+ 1) 2))) (let ((m 5)) ((+ n) m)))))

   (test-case "function application"
    (check-equal?
     (explode '(f a b c d e))
     '(((((f a) b) c) d) e)))

   (test-case "function application without arguments"
    (check-equal?
     (explode '(f))
     '(f)))

   (test-case "fn with single argument"
    (check-equal?
     (explode '(fn (x) x))
     '(fn (x) x)))

   (test-case "fn with multiple arguments"
    (check-equal?
     (explode '(fn (x y z) x))
     '(fn (x) (fn (y) (fn (z) x)))))

   (test-case "fn with single type-annotated argument"
    (check-equal?
     (explode '(fn ([x : int]) x))
     '(fn ([x : int]) x)))

   (test-case "fn with multiple type-annotated arguments"
    (check-equal?
     (explode '(fn (x [y : int] [z : bool]) x))
     '(fn (x) (fn ([y : int]) (fn ([z : bool]) x)))))

     (test-case "fn with multiple arguments applied at once"
    (check-equal?
     (explode '((fn (x y z) x) 1 2 3))
     '((((fn (x) (fn (y) (fn (z) x))) 1) 2) 3)))

   (test-case "if is not exploded like a function application"
    (check-equal?
     (explode '(if c a b))
     '(if c a b)))

   (test-case "simple define is not transformed"
    (check-equal?
     (explode-definition '(define foo bar))
     '(define foo bar)))

   (test-case "define function shorthand explodes to define fn"
    (check-equal?
     (explode-definition '(define (foo bar baz) (bar baz)))
     '(define foo
	(fn (bar)
	  (fn (baz)
	    (bar baz)))))))
