# halisp

A toy Scheme implementation written in Haskell. Still incomplete.

## Data types

### Number

```scheme
(define answer 42)

; max, min
```

### Boolean

```scheme
(define halisp-won-the-piweek #t)

(&& #t #f) ; #f

; and, or
```

### List

```scheme
; notation
(define xs '(0 1 2 3 4))
(define ys (list 9 8 7 6 5))

(reduce + '(1 2 3 4 5) 0) ; 15

(reverse xs)

; foldr, unfold, map, filter, length, zip, zip-with
; TODO: concat
```

## Functions

```scheme
; named
(define (add-1 x) (+ x 1))

; anonymous
(lambda (x) (+ x 1))
```

### Currying

```scheme
(define add-1 (curry + 1))
```

## License

GPLv3
