#lang plai
;; login: <YOUR-LOGIN-HERE>

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; lookup-op : symbol -> procedure
; Finds the (algebraic) procedure that matches the given symbol
(define (lookup-op op)
  (case op
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [else (error 'lookup-op "Unknown operation symbol")]
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

;; duplicate-binding-names? : (listof Binding) -> boolean
; Takes a list of bindings and returns true if every binding has a unique name
; Returns false otherwise.
(define (duplicate-binding-names? bindings)
  (not (= (length (remove-duplicates bindings #:key binding-name)) (length bindings))))

;; parse : expression -> WAE
; This procedure parses an expression into a WAE
(define (parse sexp)
  (match sexp
    ; number
    [(? number?) (num sexp)]
    ; id
    [(? symbol?) (id sexp)]
    ; multi-armed with
    [(list 'with (list bindings ...) body) (with (create-bindings bindings) (parse body))]
    ; binary operation  (? symbol? op)
    [(list op a b) (binop (lookup-op op) (parse a) (parse b))]
    ))

;; subst : WAE symbol WAE -> WAE
;; Substitutes second argument (sub-id) with third argument (val) in first argument (expr),
;; as per rules of substitution (see p. 19, sec 3.1)
(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [binop (op l r) (binop op 
                           (subst l sub-id val)
                           (subst r sub-id val))]
    [id (v)(if(symbol=? v sub-id) val expr)]
    [with (lob body)
          (with (map (lambda (with-binding)
                       (binding (binding-name with-binding) (subst (binding-named-expr with-binding) sub-id val)))
                     lob)
                ; if any of bindings have the same id, don't substitute in the body
                (if(ormap(lambda(binding) (symbol=? (binding-name binding) sub-id)) lob)
                   body
                   (subst body sub-id val)))
          ]))

;; calc : WAE -> number
;; This procedure executes the WAE code and produces a numeric result
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [binop (op lhs rhs) (op (calc lhs) (calc rhs))]
    [with(lob body)
         (if (duplicate-binding-names? lob) (error 'calc "Mulitple bindings for one identifier")
             (calc (foldl (lambda (binding expr)
                            (subst expr (binding-name binding)(binding-named-expr binding)))
                          body
                          lob))
             )]
    [id  (v)(error 'calc "free identitifer")]
    ))
