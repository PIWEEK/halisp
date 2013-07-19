# halisp

A toy Scheme implementation written in Haskell. Still incomplete.

## Data types

### Number

```scheme
(define answer 42)

(+ 2 3) ; 5

(* 2 3) ; 6

(** 2 10) ; 1024

(max 1 2 3 4 5) ; 5
(min 1 2 3 4 5) ; 1
```

TODO: more numeric types: rational,float

### Boolean

```scheme
(define halisp-won-the-piweek #t)

(&& #t #f) ; #f

(and #t #t #t) ; #t
(or #f #f #t) ; #t
```

### List

```scheme
; notation
(define xs '(0 1 2 3 4))
(define ys (list 9 8 7 6 5))
(define zs (cons 1 (cons 2 (cons 3 '())))) ; (1 2 3)

; first and rest AKA car and cdr
(car '(1 2 3)) ; 1
(cdr '(1 2 3)) ; (2 3)

; foldl AKA reduce
(reduce + '(1 2 3 4 5) 0) ; 15

; mapping and filtering

(map (curry * 2) '(1 2 3)) ; (2 4 6)

(filter odd? '(1 2 3)) ; (1 3)

; unfold
(define (greater-than x) (lambda (y) (> y x)))

(unfold (curry + 1) (greater-than 10) 0) ; (0 1 2 3 4 5 6 7 8 9 10)

; zip

(zip '(1 2 3) '(4 5 6)) ; ((1 4) (2 5) (3 6))
(zip-with + '(1 2 3) '(4 5 6)) ; (5 7 9)

; other useful functions
(reverse '(1 2 3)) ; (3 2 1)

(length '(1 2 3)) ; 3

; ++ and concat
(++ '(1 2 3) '(4 5)) ; (1 2 3 4 5)
(concat '((1 2) (3) (4 5))) ; (1 2 3 4 5)

; applicative

(ap (list (curry + 1) (curry * 10)) (list 1 2 3)) ; (2 3 4 10 20 30)

; monad

(define (dupe x) (list x x))

(bind dupe '(1 2 3)) ; (1 1 2 2 3 3)
```

## Functions

```scheme
; named
(define (add1 x) (+ x 1))

; anonymous
(lambda (x) (+ x 1))
```

### Currying

```scheme
(define add10 (curry + 10))

(add10 32) ; 42
```

### Apply

```scheme
(apply and '(#t #f)) ; #f
```

TODO: eval!

### Closures

```scheme
(define (create-counter inc)
    (lambda (x)
        (set! inc (+ x inc))
        inc))

(define my-counter 0)

(my-counter 1) ; 1
(my-counter 4) ; 5
```

## License

GPLv3
