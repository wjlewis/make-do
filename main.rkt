#lang racket

(require (for-syntax syntax/parse racket/syntax))

;; TODO
;; 1. Describe monad param patterns as a syntax class
;; 2. Collect common macro functionality between zero/non-zero cases
;; 3. Add error checking for `zero` functionality when no zero has
;;    been supplied

(begin-for-syntax
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
    #:description "sequence of distinct binding pairs"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
             "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...))))

(define-syntax (make-do stx)
  (syntax-parse stx
    [(_ m-name (~alt
                (~once (~seq #:return m-return:expr)
                       #:name "the monad's `return` function")
                (~once (~seq #:>>= m->>=:expr)
                       #:name "the monad's `(>>=)` function")
                (~optional (~seq #:zero m-zero:expr)
                           #:name "the monad's `zero` value")) ...)
     (with-syntax ([do-m (format-id #'m-name "do-~a" #'m-name)])
       (if (not (attribute m-zero))
           #'(define-syntax (do-m stx)
               (syntax-parse stx
                 #:datum-literals (return <- let)
                 [(do-m (return e:expr))
                  #'(m-return e)]
                 [(do-m (<- x:id rhs:expr) . es)
                  #'(m->>= rhs
                           (lambda (x)
                             (do-m . es)))]
                 [(do-m (let bs:distinct-bindings) . es)
                  #'(let bs (do-m . es))]
                 [(do-m x . es)
                  #'(begin x
                           (do-m . es))]))
           #'(define-syntax (do-m stx)
               (syntax-parse stx
                 #:datum-literals (return <- let)
                 [(do-m (return e:expr))
                  #'(m-return e)]
                 [(do-m (<- x:id rhs:expr) . es)
                  #'(m->>= rhs
                           (lambda (x)
                             (do-m . es)))]
                 [(do-m (let bs:distinct-bindings) . es)
                  #'(let bs (do-m . es))]
                 [(do-m #:when cond:expr . es)
                  #'(m->>= (if cond
                               (m-return '())
                               m-zero)
                           (lambda (_)
                             (do-m . es)))]
                 [(do-m x . es)
                  #'(begin x
                           (do-m . es))]))))]))

;; Demo 1
(define (list-return x)
  (list x))

(define (list->>= l f)
  (apply append (map f l)))

(define list-zero '())

(make-do list
         #:return list-return
         #:>>= list->>=
         #:zero list-zero)

(define triples
  (do-list (<- x (range 1 20))
           (<- y (range 1 20))
           #:when (< x y)
           (<- z (range 1 100))
           #:when (= (* z z) (+ (* x x) (* y y)))
           (return (list x y z))))
