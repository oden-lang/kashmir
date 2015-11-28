#lang racket

(provide
 codegen-expr
 codegen-imports
 codegen-definitions
 codegen-pkg)

(require "compiler/compiled-pkg.rkt")

(define (translate-identifier id)
  (let* ([parts (string-split (symbol->string id) "-" #:trim? #t)]
	 [titles (cons (car parts) (map string-titlecase (cdr parts)))])
    (string-join titles "")))

(define (codegen-type type)
  (match type
    ['unit ""]
    [`(,r -> ,d)
     (format "func (~a) (~a)"
	     (codegen-type r)
	     (codegen-type d))]
    [`(-> ,d)
     (format "func () (~a)"
	     (codegen-type d))]
    [(? list? l) (string-join (map ~a l) "_")]
    [(? symbol? t) t]))

(define (infix-operator? op)
  (member op '(+ - * / % == != < > <= >=)))

(define (codegen-return typed-expr)
  (match typed-expr
    [`((,f) : unit)
     (format "~a\nreturn\n" (codegen-expr typed-expr))]
    [`((,f ,a) : unit)
     (format "~a\nreturn\n" (codegen-expr typed-expr))]
    [`((let . ,_) : unit)
     (format "~a\nreturn\n" (codegen-expr typed-expr))]
    [`(,e : unit) "return\n"]
    [te (format "return ~a\n"
		(codegen-expr te))]))

(define (codegen-expr typed-expr)
  (match typed-expr
    [(? number? x) (~a x)]
    [`(,e : ,t) (codegen-expr e)]
    ['false "false"]
    ['true "true"]
    ['unit ""]
    [(? symbol? s) (translate-identifier s)]
    [(? string? s) (~v s)]
    [(list
      (list
       (list
        (list (? infix-operator? op) ': _)
        (list a ': _))
       ':
       (list _ '-> _))
      (list b ': _))
     (format "(~a ~a ~a)"
             (codegen-expr a)
             op
             (codegen-expr b))]
    [`(fn () (,e : ,et))
     (format "(func () ~a {\n~a})"
	     (codegen-type et)
	     (codegen-return `(,e : ,et)))]
    [`(fn ((,x : ,xt)) (,e : ,et))
     (format "(func (~a ~a) ~a {\n~a})"
             (codegen-expr x)
             (codegen-type xt)
             (codegen-type et)
             (codegen-return `(,e : ,et)))]
    [`(let (([,x : ,xt] (,e : ,et))) (,b : ,bt))
     (format "(func () ~a {\nvar ~a ~a = ~a\n~a}())"
             (codegen-type bt)
             (codegen-expr x)
             (codegen-type xt)
             (codegen-expr e)
	     (codegen-return `(,b : ,bt)))]
    [`(if ,c (,a : ,t) (,b : ,t))
     (format "(func() ~a {\nif ~a {\n~a} else {\n~a}\n})()\n"
             (codegen-type t)
             (codegen-expr c)
             (codegen-return `(,a : ,t))
             (codegen-return `(,b : ,t)))]
    [`((,f : ,_))
     (format "~a()"
	     (codegen-expr f))]
    [`((,f : ,ft) (,a : ,at))
     (format "~a(~a)"
             (codegen-expr f)
             (codegen-expr a))]
    [e (error
        (format "Go codegen failed for: ~v" e))]))

(define/contract (codegen-imports pkg)
  (-> compiled-pkg? string?)
  (string-join
   (for/list ([import (compiled-pkg-imports pkg)])
     (format "import \"~a\"\n" (car (cdr import))))))

(define/contract (codegen-definitions defs)
  (-> (listof any/c) string?)
  (string-join
   (for/list ([definition defs])
     (match definition       
       [`((define ,name ((fn ,arg (,e : ,et)) : ,_)) : ,_)
	(let ([codegend-param (match arg
				[`() "()"]
				[`((,x : ,xt)) (format "(~a ~a)"
						       (codegen-expr x)
						       (codegen-type xt))]
				[_ (error (format "Invalid argument list: ~a" arg))])])
	  (format "func ~a ~a ~a {\n~a}\n"
		  (translate-identifier name)		 
		  codegend-param
		  (codegen-type et)
		  (codegen-return `(,e : ,et))))]      
       [`((define ,name (,e : ,t)) : ,_)
	(format "var ~a ~a = ~a\n"
		(translate-identifier name)
		(codegen-type t)
		(codegen-expr e))]
       [_ (error (format "Invalid definition: ~v" definition))]))))

(define (codegen-pkg-name pkg)
  (let* ([name (compiled-pkg-name pkg)]
	 [split (string-split (symbol->string name) "/")])
    (last split)))

(define (codegen-pkg pkg)
  (format "package ~a\n\n// imports\n~a\n// monomorphed\n~a\n// definitions\n~a"
	  (codegen-pkg-name pkg)
	  (codegen-imports pkg)
          (codegen-definitions (compiled-pkg-monomorphed-defs pkg))
	  (codegen-definitions (compiled-pkg-defs pkg))))

(module+ test
  (require rackunit)

  (require "inferencer.rkt")
  (require "compiler/explode.rkt")

  (define (codegen-single-expression expr)
    (codegen-expr (infer (explode expr))))

   (test-case "empty pkg"
    (check-equal?
     (codegen-pkg (compiled-pkg 'foo '() '() '() '()))
     "package foo\n\n// imports\n\n// monomorphed\n\n// definitions\n"))

   (test-case "int literal"
    (check-equal?
     (codegen-single-expression 123)
     "123"))

   (test-case "bool literal"
    (check-equal?
     (codegen-single-expression 'true)
     "true"))

   (test-case "fn -> func"
    (check-equal?
     (codegen-single-expression
      '(fn ([q : int]) q))
     "(func (q int) int {\nreturn q\n})"))

   (test-case "fn with no arguments -> func()"
    (check-equal?
     (codegen-single-expression
      '(fn () 1))
     "(func () int {\nreturn 1\n})"))

   (test-case "apply no-argument function"
    (check-equal?
     (codegen-single-expression
      '((fn () 1)))
     "(func () int {\nreturn 1\n})()"))

   (test-case "let no-argument function"
    (check-equal?
     (codegen-single-expression
      '(let ([x (fn () 1)]) 2))
     "(func () int {\nvar x func () (int) = (func () int {\nreturn 1\n})\nreturn 2\n}())"))

  (test-case "let no-argument function"
    (check-equal?
     (codegen-single-expression '(let ([name-with-dashes 1]) name-with-dashes))
     "(func () int {\nvar nameWithDashes int = 1\nreturn nameWithDashes\n}())"))

   (test-case "partial application"
    (check-equal?
     (codegen-single-expression '(((fn (x y) x) 1) 2))
     "(func (x int) func (int) (int) {\nreturn (func (y int) int {\nreturn x\n})\n})(1)(2)"))

   (test-case "let"
    (check-equal?
     (codegen-single-expression '(let ([x 1]) (+ x 2)))
     "(func () int {\nvar x int = 1\nreturn (x + 2)\n}())"))

   (test-case "let type annotated"
    (check-equal?
     (codegen-single-expression '(let ([[x : int] 1]) (+ x 2)))
     "(func () int {\nvar x int = 1\nreturn (x + 2)\n}())"))

  (test-case "let unit"
    (check-equal?
     (codegen-single-expression '(let ([x "foo"]) (fmt.Println x)))
     "(func ()  {\nvar x string = \"foo\"\nfmt.Println(x)\nreturn\n}())"))
  
  (test-case "let unit in fn"
    (check-equal?
     (codegen-single-expression '(fn ([x : string]) (let ([y x]) (fmt.Println y))))
     "(func (x string)  {\n(func ()  {\nvar y string = x\nfmt.Println(y)\nreturn\n}())\nreturn\n})"))
  
   (test-case "higher-order functions"
    (check-equal?
     (codegen-single-expression '(((fn (x y) (x y)) (fn (x) x)) 1))
     "(func (x func (int) (int)) func (int) (int) {\nreturn (func (y int) int {\nreturn x(y)\n})\n})((func (x int) int {\nreturn x\n}))(1)"))

   (test-case "call by name"
    (check-equal?
     (codegen-single-expression '(let ([make-num (fn () 3)]) (* (make-num) (make-num))))
     "(func () int {\nvar makeNum func () (int) = (func () int {\nreturn 3\n})\nreturn (makeNum() * makeNum())\n}())")))
