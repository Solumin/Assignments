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
    )
  )

;; valid-op? : symbol -> boolean
; Checks if the given operation is known
(define (valid-op? op)
  (ormap (lambda (known-op)
           (symbol=? known-op op))
         (list '+ '- '* '/)))

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

;; valid-id? : symbol -> boolean
(define (valid-id? id)
  (not (ormap (lambda (reserved-id)
                (symbol=? reserved-id id))
              (list '+ '- '* '/ 'with 'if0 'fun)))
  )

;; lookup : symbol Env -> CFWAE
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "no binding for identifier: ~a" name )]
    [anEnv (bound-name bound-value rest-env)
           (if(symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
; The biggest problem right now (that I have with it, that is) is that it doesn't give
; intelligent error messages. For example, (parse '{if0 0 1}) will throw an "unknown
; operation" error, not "if0 requires condition, then and else statements".
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (if (valid-id? sexp) (id sexp)
                     (error 'parse "IDs may not be +, -, *, /, with, if0 or fun. (Received ~a)" sexp))]
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
    [(list (? symbol? op) a b) (binop (lookup-op op) (parse a) (parse b))]
    [(list f args ..1) (app (parse f) (map parse args))]
    [else (error 'parse "Unknown syntax expression: ~a" sexp)]
    ))

;; preproces : CFWAE -> CFWAE
(define (preprocess expr)
  (type-case CFWAE expr
    [num (n) expr]
    [binop (op l r) (binop op (preprocess l) (preprocess r))]
    [with (lob body) (app 
                      (fun (map binding-name lob) (preprocess body))
                      (map binding-named-expr lob))]
    [id (name) expr]
    [if0 (c t e) (if0 (preprocess c) (preprocess t) (preprocess e))]
    [fun (args body) (fun args (preprocess body))]
    [app (f args) (app (preprocess f) args)]
    ))

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp expr)
  (interp-sub (preprocess expr) (mtEnv)))

;; interp-no-with : CFWAE Env -> CFWAE-Value
(define (interp-sub expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (op l r) (numV (op (numV-n (interp-sub l env)) (numV-n (interp-sub r env))))]
    [with (lob body) (error 'interp "With statements should have been preprocessed.")]
    [id (name) (error 'interp "Tried to access id ~a" name)]; (lookup name env)]
    [if0 (c t e) 
         (local ([define cond-val (interp-sub c env)])
           (cond
             [(closureV? cond-val)
              (error 'interp "if0 conditions must evaluate to a number (given ~a)" cond-val)]
             [(if (= 0 (numV-n (interp-sub c env)))
                  (interp-sub t env)
                  (interp-sub e env))]))]
    [fun (ids body) (closureV ids body env)]
    [app (fun args)
         (local ([define fun-val (interp-sub fun env)])
           (interp-sub (closureV-body fun-val)
                           (foldl
                            (lambda (fv arg new-env)
                              (anEnv fv (interp-sub arg env) new-env))
                            (mtEnv)
                            (closureV-params fun-val) args)))]
    ))