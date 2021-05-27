#lang racket
(provide expr-compare)

; from TA hint code, to check if element is lambda or lambda symbol
(define (lambda? x)
  (member x '(lambda λ)))

; defines exclusive xor, https://stackoverflow.com/questions/1930901/exclusive-or-in-scheme
(define (xor a b)
  (and 
   (not (and a b))
   (or a b)))

; combines xSymb!ySymb
(define (combineSymbols xSymb ySymb)
    (string-append (symbol->string xSymb) "!" (symbol->string ySymb))
)

; from TA hint code, check if equal then check list cases
(define (expr-compare x y)
    (cond 
        [(equal? x y) ; base case, x + y are the same
            x]
        [(and (boolean? x) (boolean? y)) ; x + y are bools, do % op
            (if x '% '(not %))]
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
        [(and (boolean? (car x)) (boolean? (car y)))
            (cons (if (car x) '% '(not %)) (compareRegularList (cdr x) (cdr y)))]
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
            (handleLambdaStart 'lambda (cdr x) (cdr y))]
        [else (handleLambdaStart 'λ (cdr x) (cdr y))]
    )
)

; starts lambda expr procession, init x + y maps
(define (handleLambdaStart lambdaType x y)
    (list lambdaType (handleLambdaArgs (car x) (car y))
        (process-lambda-fun (cadr x) (cadr y) 
            ; start list/map for x + y
            (cons (build-dictx (car x) (car y)) '())
            (cons (build-dicty (car x) (car y)) '())))
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

; first function call for two lambda expression with same # of parameters
(define (process-lambda x y lambda dictx-list dicty-list)
  (list lambda (handleLambdaArgs (car x) (car y)) ; append correct lambda and output correctly combined arguments
        (process-lambda-fun (car (cdr x)) ; expr part of lambda expressions
                            (car (cdr y))
                            (cons (build-dictx (car x) (car y)) dictx-list) ; build list of dictionaries
                            (cons (build-dicty (car x) (car y)) dicty-list))))   
 
; processes 'expr' part of lambda expression
(define (process-lambda-fun x y dictx-list dicty-list)
  ; we first get the most recent name for each element if there exists a new name
  (let ([x-curr (if (equal? (get-latest-name x dictx-list) "Not Found1") x (get-latest-name x dictx-list))]
        [y-curr (if (equal? (get-latest-name y dicty-list) "Not Found1") y (get-latest-name y dicty-list))])
  (cond
    ; if both are lists of same lengths, needs to check this first 
    [(and (list? x) (list? y) (equal? (length x) (length y))) (process-lambda-fun-list-start x y dictx-list dicty-list)]
    ; if curr elements r equal
    [(equal? x-curr y-curr) x-curr]
    ; if are boolean
    [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
    ; if one of them is not list
    [(or (not (list? x)) (not (list? y)))
     ; need to call replace-all to update all variable names before returning output if element is a list
     (list 'if '% (if (list? x) (replace-all x dictx-list #t) x-curr) (if (list? y) (replace-all y dicty-list #t) y-curr))]
    ; if both are list, but are lists of different length
    [(and (list? x) (list? y) (not (equal? (length x) (length y)))) (list 'if '% (replace-all x dictx-list #t) (replace-all y dicty-list #t))]
    )))

; starting point for 'expr' part of lambda expression that are *lists* of the same length
(define (process-lambda-fun-list-start x y dictx dicty)
  (cond
    ; want to first check if both of them are 'if's, append if first and deal with rest of list
    [(and (equal? (car x) 'if) (equal? (car x) (car y))) (cons 'if (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
    ; check if only one of them are 'if'
    [(or (equal? (car x) 'if) (equal? (car y) 'if)) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
    ; check if either of them are quotes...
    [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
     (if (equal? x y) x (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t)))]
    ; check for beginning with 'lambda'
    [(and (equal? (car x) 'lambda) (equal? (car x) (car y)))
     (cond
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
       [else (process-lambda (cdr x) (cdr y) 'lambda dictx dicty)])]
    ; check for beginning with lambda symbol
    [(and (equal? (car x) 'λ) (equal? (car x) (car y)))
     (cond
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
       [else (process-lambda (cdr x) (cdr y) 'λ dictx dicty)])]
    ; check for beginning with different lambda symbols
    [(and (lambda? (car x)) (lambda? (car y)))
     (cond
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
       [else (process-lambda (cdr x) (cdr y) 'λ dictx dicty)])]
    ; only one begin with lambda
    [(or (lambda? (car x)) (lambda? (car y))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
    ; all other cases...
    [else (expr-comp-list-lambda x y dictx dicty)]
    ))

; processes elements in lists within lambda functions one by one
(define (expr-comp-list-lambda x y dictx dicty)
  (if (and (empty? x) (empty? y)) '()
      (let ([x-curr (if (equal? (get-latest-name (car x) dictx) "Not Found1") (car x) (get-latest-name (car x) dictx))]
            [y-curr (if (equal? (get-latest-name (car y) dicty) "Not Found1") (car y) (get-latest-name (car y) dicty))])
        (cond
          ; want to first check if they're both lists
          [(and (list? x-curr) (list? y-curr))
           (cond
             ; if lists are of same length
             [(equal? (length (car x)) (length (car y))) (cons (process-lambda-fun-list-start (car x) (car y) dictx dicty)
                                                               (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
             ; if lists are different length
             [else (cons (list 'if '% (replace-all (car x) dictx #t) (replace-all (car y) dicty #t)) (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
             )]
          ; if curr element are equal
          [(equal? x-curr y-curr) (cons x-curr (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
          ; if curr element are bools
          [(and (boolean? (car x)) (boolean? (car y))) 
           (cons (if (car x) '% '(not %)) (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
          ; case when only one element is a list
          [(or (list? x-curr) (list? y-curr))
           (list 'if '% (if (list? x) (replace-all x dictx #t) x-curr) (if (list? y) (replace-all y dicty #t) y-curr))]
          ; all other cases...
          [else (cons (list 'if '% x-curr y-curr) (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))])
        )))

; helper function: given a list x and list of dictionaries, updates all variable names
; note: head indicates we're at beginning of list in order to not override if/lambda at beginning of list
(define (replace-all x dictx-list head)
  (cond
    [(empty? x) '()]
    ; if x is a quotes list, we don't want to rename variables
    [(equal? (car x) 'quote) x]
    ; check if begin with lambda or lambda symbol
    [(and head (or (equal? (car x) 'lambda) (equal? (car x) 'λ)))
     (cons (car x) (cons (car (cdr x)) (replace-all (cdr (cdr x)) (cons (build-dictx (car (cdr x)) (car (cdr x))) dictx-list) #f)))]
    ; check if begin with 'if' 
    [(and head (equal? (car x) 'if)) (cons (car x) (replace-all (cdr x) dictx-list #f))]
    ; check if (car x) is a list
    [(list? (car x)) (cons (replace-all (car x) dictx-list #t) (replace-all (cdr x) dictx-list #f))]
    ; case for boolean, don't want to replace
    [(boolean? (car x)) (cons (car x) (replace-all (cdr x) dictx-list #f))]
    ; all other cases, we look for latest name in dictionary if there exists one
    [else (cons
           (if (equal? (get-latest-name (car x) dictx-list) "Not Found1") (car x) (get-latest-name (car x) dictx-list))
           (replace-all (cdr x) dictx-list #f))]
    ))

; helper function: given list of dictionaries in order of new->old, returns the most recent defined name for a variable
(define (get-latest-name x dict-list)
  (cond
    [(empty? dict-list) "Not Found1"]
    [(not (equal? (hash-ref (car dict-list) x "Not Found1") "Not Found1")) (hash-ref (car dict-list) x "Not Found1")]
    [else (get-latest-name x (cdr dict-list))]
    ))

; helper function: builds dictionary for x
(define (build-dictx x y)
  (cond [(and (empty? x) (empty? y)) (hash)]
        [(equal? (car x) (car y)) (hash-set (build-dictx (cdr x) (cdr y)) (car x) (car x))]
        [else (hash-set (build-dictx (cdr x) (cdr y))
                        (car x) (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))))]
        ))

; helper function: builds dictionary for y
(define (build-dicty x y)
  (cond [(and (empty? x) (empty? y)) (hash)]
        [(equal? (car x) (car y)) (hash-set (build-dicty (cdr x) (cdr y)) (car y) (car y))]
        [else (hash-set (build-dicty (cdr x) (cdr y))
                        (car y) (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))))]
        ))

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
                