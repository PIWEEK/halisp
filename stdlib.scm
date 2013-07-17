(define (not x) (if x #f #t))

(define (null? x) (if (eqv? x '()) #t #f))

(define (list . args) args)

(define (id obj) obj)

(define (flip f) (lambda (x y) (f y x)))

(define (curry f x) (lambda (y) (apply f (cons x (list y)))))

(define (compose f g) (lambda (arg) (f (apply g arg))))

(define (zero? n) (if (equal? n 0) #t #f))

(define (positive? n) (if (> n 0) #t #f))

(define (negative? n) (if (< n 0) #t #f))

(define (odd? n) (/= (mod n 2) 0))

(define (even? n) (= (mod n 2) 0))

(define (foldr func l base) (if (null? l) base (func (car l) (foldr func (cdr l) base))))

(define (foldl func l base) (if (null? l) base (foldl func (cdr l) (func base (car l)))))

(define fold foldl)

(define reduce fold)

(define (unfold func init pred) (if (pred init) '() (cons init (unfold func (func init)  pred))))

(define (sum . xs) (reduce + xs 0))

(define (product . xs) (reduce * xs 1))

(define (and . xs) (reduce && xs #t))

(define (or . xs) (reduce || xs #f))

(define (max x . xs) (foldr (lambda (old new) (if (> old new) old new)) xs x))

(define (min x . xs) (foldr (lambda (old new) (if (< old new) old new)) xs x))

(define (map f xs) (foldr (lambda (x acc) (cons (f x) acc)) xs '()))

(define (filter pred xs) (foldr (lambda (x acc) (if (pred x) (cons x acc) acc)) xs '()))

(define (length xs) (foldr (lambda (x y) (+ 1 y)) xs 0))

(define (append x xs) (foldr cons xs (cons x '())))

(define (reverse xs) (foldr append xs '()))