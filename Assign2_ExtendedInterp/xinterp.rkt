#lang plai
;; login : <YOUR-LOGIN-HERE>

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

;; lookup-op : symbol -> procedure
; Finds the (algebraic) procedure that matches the given symbol
(define (lookup-op op)
  (case op
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [else (error 'parse "Unknown operation symbol")]
    )
  )


;; create-bindings : (listof symbol) -> (listof Binding)
; Takes the parser's list of symbols in with expressions and creates a list of bindings
(define (create-bindings bindings)
  (if (empty? (first bindings)) null
      (map (lambda (pair) 
             (binding (first pair) (parse (second pair)))) 
           bindings))
  )

;; duplicate-binding-names? : (list) -> boolean
; Returns true if the input list has any duplicate entries
(define (has-duplicates? items)
  (not (= (length (remove-duplicates items)) (length items))))

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
; The biggest problem right now (that I have with it, that is) is that it doesn't give
; intelligent error messages. For example, (parse '{if0 0 1}) will throw an "unknown
; operation" error, not "if0 requires condition, then and else statements".
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list 'with (list bindings ...) body)
     (with 
      (cond
        [(not (andmap symbol? (map first bindings))) (error 'parse "Names in with statements must be symbols")]
        [(has-duplicates? (map first bindings)) (error 'parse "Duplicate identifiers in with expression")]
        [else (create-bindings bindings)])
      (parse body))]
    [(list 'if0 c t e) (if0 (parse c) (parse t) (parse e))]
    [(list 'fun (list args ...) body)
     (fun 
      (cond
        [(not (andmap symbol? args)) (error 'parse "Function argument list must be symbols")]
        [(has-duplicates? args) (error 'parse "Duplicate arguments in function definition")]
        [else args])
      (parse body))]
    [(list 'app f (list args ...)) (app (parse f) (parse args))]
    [(list op a b) (binop (lookup-op op) (parse a) (parse b))]
    [else (error 'parse "Unknown syntax expression")]
    ))

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
;(define (interp expr)
;  (...))

