#lang racket
(provide expr-compare)
(provide test-expr-compare)
(provide test-expr-x)
(provide test-expr-y)

; from TA hint code, to check if element is lambda or lambda symbol
(define (lambda? x)
  (member x '(lambda λ)))

; defines exclusive xor, https://stackoverflow.com/questions/1930901/exclusive-or-in-scheme
(define (xor a b)
  (and (not (and a b)) (or a b)))

; combines xSymb!ySymb
(define (combineSymbols xSymb ySymb)
    (string-append (symbol->string xSymb) "!" (symbol->string ySymb)))

; checks if both xElem + yElem are booleans
(define (bothBool xElem yElem)
    (and (boolean? xElem) (boolean? yElem)))

; checks if xElem is if or quote
(define (isConstant xElem)
    (or (equal? xElem 'quote) (equal? xElem 'if)))

; helper fx for % output
(define (percentOp x)
    (if x '% '(not %)))

; based off TA hint code, check if equal then check list cases
(define (expr-compare x y)
    (cond 
        [(equal? x y) ; base case, x + y are the same
            x]
        [(bothBool x y) ; x + y are bools, do % op
            (percentOp x)]
        ; if x or y isn't list, means not fx
        [(or (not (list? x)) (not (list? y)))
            (list 'if '% x y)]
        ; otherwise, check if x + y are lists of same length
        [(and (list? x) (list? y)
            (= (length x) (length y)))
            (compareSameLenLists x y)]
        ; handle if x + y are lists w/ diff length
        [(and (list? x) (list? y)
            (not (= (length x) (length y))))
            (list 'if '% x y)]
    ))

; check lists w/ same length
(define (compareSameLenLists x y)
    (cond
        ; check if x and/or y start w/ quote
        [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
            (list 'if '% x y)]  
        ; check if x OR y start w/ if
        [(xor (equal? (car x) 'if) (equal? (car y) 'if))
            (list 'if '% x y)]
        ; check if either list starts w/ lambda or lambda symbol
        [(or (lambda? (car x)) (lambda? (car y)))
            (compareSameLenLambdas x y)]
        ; any other case, call rec helper fx
        [else (compareRegularList x y)]
    ))

; handles unnested lists
(define (compareRegularList x y)
    (cond
        ; base case, empty lists = done parsing
        [(and (zero? (length x)) (zero? (length y)))
            '()]
        ; identical elems, append to output + recurse
        [(equal? (car x) (car y))
            (cons (car x) (compareRegularList (cdr x) (cdr y)))]
        ; non-identical bool elems
        [(bothBool (car x) (car y))
            (cons (percentOp (car x)) (compareRegularList (cdr x) (cdr y)))]
        ; check if elems are lists
        [(and (list? (car x)) (list? (car y)))
            (cond
                ; check if lists have same length
                [(= (length (car x)) (length (car y)))
                    (cons (compareSameLenLists (car x) (car y)) (compareRegularList (cdr x) (cdr y)))]
                ; otherwise, diff lengths
                [else (cons (list 'if '% (car x) (car y)) (compareRegularList (cdr x) (cdr y)))]
            )
        ]
        [else (cons (list 'if '% (car x) (car y)) (compareRegularList (cdr x) (cdr y)))]
    ))     

; starts lambda expr procession, init x + y maps
(define (handleLambdaStart lambdaType x y)
    (list lambdaType (handleLambdaArgs (cadr x) (cadr y))
        (parseLambdaExpr lambdaType  
            ; start list/map for x + y
            (caddr x) (cons (createXMap #t (cadr x) (cadr y)) '()) 
            (caddr y) (cons (createYMap #t (cadr x) (cadr y)) '())
        ))
    )

; checks lambda start cases when handling lists w/ same length
(define (compareSameLenLambdas x y)
    (cond
        ; x OR y starts w/ lambda symbol
        [(xor (lambda? (car x)) (lambda? (car y)))
            (list 'if '% x y)]
        ; both start w/ lambda symbols, check if same # of args (cadr is car (cdr x) = 2nd elem)
        [(not (= (length (cadr x)) (length (cadr y))))
            (list 'if '% x y)]
        ; if diff # of symbols, check if both lambda OR 1 lambda + 1 lam symbol or 2 lam symbols
        [(and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
            (handleLambdaStart 'lambda x  y)]
        [else (handleLambdaStart 'λ  x  y)]
    )
)

; goes through args passed to lambda fxs + checks equality
(define (handleLambdaArgs x y)
    (cond
        ; base case, all args have been parsed
        [(and (zero? (length x)) (zero? (length y)))
            '()]
        ; if diff args, need to combine x!y + recurse to get rest of args
        [(not (equal? (car x) (car y)))
            (cons (string->symbol (combineSymbols (car x) (car y)))
                (handleLambdaArgs (cdr x) (cdr y)))]
        ; if same args, just add + recurse
        [(equal? (car x) (car y))
            (cons (car x) (handleLambdaArgs (cdr x) (cdr y)))]
    ))

(define (parseLambda  lambdaType x y xMap yMap)
    (list lambdaType (handleLambdaArgs (cadr x) (cadr y))
        (parseLambdaExpr lambdaType 
            (caddr x) (cons (createXMap #f (cadr x) (cadr y)) xMap) 
            (caddr y) (cons (createYMap #f (cadr x) (cadr y)) yMap)
            ))
    )

(define (parseLambdaExpr lambdaType x xMap y yMap)
    (cond
        ; base case, x + y are the same elem
        [(equal? (findVar x xMap) (findVar y yMap))
            (findVar x xMap)]
        ; simple case, x + y are both bools
        [(bothBool x y) (percentOp x)]
        ; both x + y are lists
        [(and (list? x) (list? y))
            (if (= (length x) (length y))
                (parseListExpr x xMap y yMap) ; same length, call fx to parse
                (list 'if '% (renameList x xMap) (renameList y yMap))) ; diff length, rename vars + add
        ]
        ; either x or y is a list
        [else 
            (list 'if '%
                (if (list? x) (renameList x xMap) (findVar x xMap))
                (if (list? y) (renameList y xMap) (findVar y yMap)))]
    ))

; helper fx for parsing lambda expr for lambda cases
(define (lambdaHelper x xMap xRenamed y yMap yRenamed)
    (cond
        ; x OR y starts w/ lambda
        [(xor (lambda? (car x)) (lambda? (car y)))
            (list 'if '% xRenamed yRenamed)]
        ; otherwise, check if lists have diff length
        [(not (= (length (cadr x)) (length (cadr y))))
            (list 'if '% xRenamed yRenamed)]
        ; lists have same length, check if both start w/ lambda
        [(and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
            (parseLambda 'lambda x y xMap yMap)]
        ; otherwise, both start w/ lambda symbol or diff lambdas
        [else 
            (parseLambda 'λ x y xMap yMap)]
    ))

(define (lambdaHelperConstants x xMap xRenamed y yMap yRenamed)
    (cond
        ; x + y are both quotes
        [(and (equal? (car x) 'quote) (equal? (car y) 'quote)) ('quote)]
        ; x OR y are quotes
        [(xor (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% xRenamed yRenamed)]
        ; x OR y are  if
        [(xor (equal? (car x) 'if) (equal? (car y) 'if)) (list 'if '% xRenamed yRenamed)]
        ; x + y are if
        [(and (equal? (car x) 'if) (equal? (car y) 'if)) 
            (cons 'if (parseLambdaList (cdr x) xMap (cdr y) yMap))]
    )
)

(define (parseListExpr x xMap y yMap)
    (let ((xRenamed (renameList x xMap)) (yRenamed (renameList y yMap)))
        (cond
            ; check if x or y starts w/ lambda or lambda symbol
            [(or (lambda? (car x)) (lambda? (car y)))
                (lambdaHelper x xMap xRenamed y yMap yRenamed)]
            ; check if x or y starts w/ constant (if or quote)
            [(or (isConstant (car x)) (isConstant (car y)))
                (lambdaHelperConstants x xMap xRenamed y yMap yRenamed)]
            ; otherwise, call helper fx to check list
            [else (parseLambdaList x xMap y yMap)]
        )
    )
)

; checks switch cases, separated for debugging
(define (listHelper2 x xMap xRenamed y yMap yRenamed)
    (let ((firstElem '()))
        (cond
            ; check if x + y are lists
            [(and (list? (findVar (car x) xMap)) (list? (findVar (car y) yMap)))
                (if (= (length (car x)) (length (car y)))
                    (set! firstElem (parseListExpr (car x) xMap (car y) yMap))
                    (set! firstElem (list 'if '% xRenamed yRenamed)))
                (cons firstElem (parseLambdaList (cdr x) xMap (cdr y) yMap))
            ]
            [else 
                (cond
                    ; check if starting elem = same for x + y
                    [(and (equal? (car x) 'quote) (equal? (car y) 'quote)) 
                        (set! firstElem (percentOp (car x)))]
                    [(bothBool (car x) (car y)) 
                        (set! firstElem (percentOp (car x)))]
                    [(equal? (findVar (car x) xMap) (findVar (car y) yMap)) 
                        (set! firstElem (findVar (car x) xMap))]
                    [else 
                        (set! firstElem (list 'if '% (findVar (car x) xMap) (findVar (car y) yMap)))]
                )
                (cons firstElem (parseLambdaList (cdr x) xMap (cdr y) yMap))
            ]
        )
    ))

; recursively goes through elems in lambda lists
(define (parseLambdaList x xMap y yMap)
    (let ((xRenamed (renameList x xMap)) (yRenamed (renameList y yMap)))
        (cond
            ; base case, empty lists = done parsing
            [(and (zero? (length x)) (zero? (length y))) '()]
            ; either x OR y is list
            [(xor (list? (findVar (car x) xMap)) (list? (findVar (car y) yMap)))
                (list 'if '%
                    (if (list? x) xRenamed (findVar (car x) xMap))
                    (if (list? y) xRenamed (findVar (car y) yMap))
            )]
            ; check switch cases
            [else (listHelper2 x xMap xRenamed y yMap yRenamed)]
    )))

; based off stackoverflow answers
; https://stackoverflow.com/questions/62952288/scheme-procedure-to-replace-elements-in-a-list
; https://stackoverflow.com/questions/4542386/searching-and-replacing-n-element-on-list-scheme
; startSymbol (lambda or NA), x, + map ==> renames vars found in map
(define (renameList x xMap)
    (let ((currentElem (if (not (zero? (length x))) (car x) '())))
    (cond
        ; base case, done when empty
        [(zero? (length x)) '()]
        ; check if constant (if or quote)
        [(isConstant currentElem)
            (if (equal? currentElem 'quote)
                x ; quote, do nothing
                (cons currentElem (renameList (cdr x) xMap))); if, recurse
        ]
        ; check if bool
        [(boolean? currentElem)
            (cons currentElem (renameList (cdr x) xMap))]
        ; check if starts w/ lambda
        [(lambda? currentElem)
            (cons currentElem
            (list (cadr x) (renameList (cddr x) (cons (createXMap #f (cadr x) (cadr x)) xMap))))]
        ; check for list
        [(list? currentElem)
            (cons (renameList currentElem xMap) (renameList (cdr x) xMap))]
        ; otherwise, checks map for var name
        [else
        (cons
           (findVar currentElem xMap) 
           (renameList (cdr x) xMap))]
    )))

; gets name from map if it exists
(define (findVar x xMap)
    (let ((foundVar (findVarHelper x xMap)))
    (if foundVar foundVar x ))
)

; helper fx for findVar, checks map for most recent var
; https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-ref%29%29
(define (findVarHelper x xMap)
    (cond
        [(zero? (length xMap)) #f]
        [(equal? (hash-ref (car xMap) x #f) #f)
            (findVarHelper x (cdr xMap))]
        [else 
             (hash-ref (car xMap) x #f)]
))

; rec creates map for x elems
; stack overflow inspiration
; https://stackoverflow.com/questions/56589496/lisp-accessing-recursive-hashes-with-syntactic-sugar
;https://stackoverflow.com/questions/1099509/how-can-i-reuse-a-gethash-lookup-in-common-lisp
;https://stackoverflow.com/questions/52349884/create-a-nested-dictionary-using-recursion-python
(define (createXMap isStart x y)
    ; currentElem = 1st elem in x if nonempty, () if x is empty
    (let ((currentElem (if (not (zero? (length x))) (car x) '())))
    (cond 
        ; bool start, start hash table
        ;[(equal? isStart #t) (hash)]
        ; empty, start hash table
        [(and (zero? (length x)) (zero? (length y))) (hash)]
        ; same current elem, map elem to itself
        [(equal? currentElem (car y))
            (hash-set (createXMap #f (cdr x) (cdr y)) currentElem currentElem)]
        ; otherwise, map elem to combined elem
        [else
            (hash-set (createXMap #f (cdr x) (cdr y)) currentElem
            (string->symbol (combineSymbols currentElem (car y))))]
    ))
)

; rec creates map for y elems
(define (createYMap isStart x y)
    ; currentElem = 1st elem in x if nonempty, () if x is empty
    (let ((currentElem (if (not (zero? (length y))) (car y) '())))
    (cond 
        ; bool start, start hash table
        ;[(equal? isStart #t) (hash)]
        ; empty, start hash table
        [(and (zero? (length x)) (zero? (length y))) (hash)]
        ; same current elem
        [(equal? (car x) currentElem)
            (hash-set (createXMap #f (cdr x) (cdr y)) currentElem currentElem)]
        ; otherwise, combine elems
        [else
            (hash-set (createXMap #f (cdr x) (cdr y)) currentElem
            (string->symbol (combineSymbols (car x) currentElem)))]
    ))
)

; #2, from TA hint code
; compare and see if the (expr-compare x y) result is the same with x when % = #t
; and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)),(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)),(expr-compare x y))))))

;#3
(define test-expr-x
    (list
        (+ 1 3)
        '(lambda (a b) (* a b))
        '(if x y z)
        '(cons "hello" "world")
        #f
        '(cons (1 3) (1))
        '(quote λ (a b c))
        '(list x y z)
        (= (+ 1 2) (+ 2 1))
        "test"
        "test2"
        '(1 3 1)
        #t
    ))

(define test-expr-y
    (list
        (* 1 3)
        '(lambda (a b) (+ a b))
        '(if x y z)
        '(cons "hello" (cons "hello" "world"))
        #f
        '(cons (1) (3 1))
        '(quote lambda (a b d))
        '(list x x z)
        (equal? (+ 1 2) (* 2 1))
        "test1"
        "test2"
        '(1 3 1)
        #f
    ))

(expr-compare test-expr-x test-expr-y)

; testcases
(equal? (expr-compare 12 12) '12)
(equal? (expr-compare 12 20) '(if % 12 20))
(equal? (expr-compare #t #t) #t)
(equal? (expr-compare #f #f) #f)
(equal? (expr-compare #t #f) '%)
(equal? (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
(equal? (expr-compare '(cons a b) '(cons a b)) '(cons a b))
; Xinyu's stronger test cases
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(equal? (expr-compare '(lambda (a) a) '(lambda (b) b)) '(lambda (a!b) a!b))
(equal? (expr-compare '(lambda (a) b) '(cons (c) b)) '(if % (lambda (a) b) (cons (c) b)))
(equal? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3)) 
    '((λ (if!fi) (+ if!fi 1)) 3))
(equal? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ)) '(λ (lambda!λ) lambda!λ))
(equal? (expr-compare ''lambda '(quote λ)) '(if % 'lambda 'λ))
(equal? (expr-compare '(lambda (a b) a) '(λ (b) b))  
    '(if % (lambda (a b) a) (λ (b) b)))
(equal? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
    '(λ (let) (let (((if % x y) 1)) (if % x y))))
(equal? (expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
              '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))
(equal? (expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
                '(((λ (g!x)
                   ((λ (x!n) (g!x (λ () (x!n x!n))))
                    (λ (x!r) (g!x (λ () (x!r x!r))))))
                 (λ (r!g)
                   (λ (n!x) (if (= n!x 0)
                                1
                                (* n!x ((r!g) (- n!x 1)))))))
                (if % 10 9)))

(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
(expr-compare '(cons a b) '(list a b))
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
(expr-compare '(if x y z) '(g x y z))