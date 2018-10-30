;;
;; Scheme 9 from Empty Space, Reimagined
;; By Nils M Holm 2007-2018
;; In the public domain
;; If your country does not have a public domain, the CC0 applies:
;; https://creativecommons.org/share-your-work/public-domain/cc0/
;;

;; Make inlined procedures APPLY-able

(define (cons x y) (cons x y))
(define (car x)    (car x))
(define (cdr x)    (cdr x))
(define (cadr x)   (cadr x))

(define (reverse x) (reverse x))

(define (set-input-port! p) (set-input-port! p))
(define (set-output-port! p) (set-output-port! p))

(define (write-char x . port)
  (if (null? port)
      (write-char x)
      (write-char x (car port))))

(define (abs x) (abs x))

;; Some easy things first

(define (void) (begin))

(define (list . x) x)
(define (string . x) (list->string x))
(define (vector . x) (list->vector x))

;; Essential binding constructs

; Simplified LET without error checking and named LET

(define-syntax (let bs . xs)
  ((lambda (split)
     ((lambda (tmp-split)
        (set! split tmp-split)
        (apply (lambda (vs as)
                 (append
                   (list (append
                           (list 'lambda)
                           (append (list vs)
                                   xs)))
                   as))
               (split bs '() '())))
      (lambda (bs vs as)
        (if (null? bs)
            (list vs as)
            (split (cdr bs)
                   (cons (caar bs) vs)
                   (cons (cadr (car bs)) as))))))
   #f))

; Binary/ternary MAP for now

(define (map f a . b)
  (if (null? b)
      (let ((map1 #f))
        (let ((tmp-map1
                (lambda (a)
                  (if (null? a)
                      '()
                       (cons (f (car a))
                             (map1 (cdr a)))))))
        (set! map1 tmp-map1)
        (map1 a)))
      (let ((map2 #f))
        (let ((tmp-map2
                (lambda (a b)
                  (if (null? a)
                      '()
                       (cons (f (car a) (car b))
                             (map2 (cdr a) (cdr b)))))))
        (set! map2 tmp-map2)
        (map2 a (car b))))))

(define-syntax (letrec bs . xs)
  (let ((ts (map (lambda (x) (gensym)) bs))
        (vs (map car bs))
        (as (map cadr bs)))
    (let ((undefs  (map (lambda (v) (list v #f))
                        vs))
          (tmp-bs  (map list ts as))
          (updates (map (lambda (v t) (list 'set! v t))
                        vs
                        ts)))
      (list 'let
            undefs
            (append '(let)
                    (list tmp-bs)
                    (append updates xs))))))

;; Control constructs

; Simplified COND for now

(define-syntax (cond . cs)
  (if (null? cs)
      #f
      (if (eq? 'else (caar cs))
          (cons 'begin (cdar cs))
          (list 'if (caar cs)
                    (cons 'begin (cdar cs))
                    (cons 'cond (cdr cs))))))

(define-syntax (and . xs)
  (cond ((null? xs) #t)
        ((null? (cdr xs)) (car xs))
        (else (list 'if (car xs)
                        (cons 'and (cdr xs))
                        #f))))

(define-syntax (or . xs)
  (cond ((null? xs) #f)
        ((null? (cdr xs)) (car xs))
        (else (list 'if* (car xs)
                         (cons 'or (cdr xs))))))

; Interlude: simple, non-nestable QUASIQUOTE

(define-syntax (quasiquote x)
  (cond ((vector? x)
          (list 'list->vector
                (list 'quasiquote (vector->list x))))
        ((not (pair? x))
          (list 'quote x))
        ((eq? 'unquote (car x))
          (cadr x))
        ((and (pair? (car x))
              (eq? 'unquote (caar x)))
          (list 'cons (cadar x)
                      (list 'quasiquote (cdr x))))
        ((and (pair? (car x))
              (eq? 'unquote-splicing (caar x)))
          (list 'append (cadar x)
                        (list 'quasiquote (cdr x))))
        (else
          (list 'cons (list 'quasiquote (car x))
                      (list 'quasiquote (cdr x))))))

; Full COND, including (p => procedure) and (p) clauses

(define-syntax (cond . cs)
  (cond ((null? cs) #f)
        ((null? (cdar cs))
          `(if* ,(caar cs)
                (cond . ,(cdr cs))))
        ((eq? '=> (cadar cs))
          (let ((t (gensym)))
            `(let ((,t ,(caar cs)))
               (if ,t (,(caddr (car cs)) ,t)
                   (cond . ,(cdr cs))))))
        ((eq? 'else (caar cs))
          `(begin . ,(cdar cs)))
        ((null? (cdr cs))
          `(if ,(caar cs)
               (begin . ,(cdar cs))))
        (else
          `(if ,(caar cs)
               (begin . ,(cdar cs))
               (cond . ,(cdr cs))))))

; Check binding syntax of LET, LET*, LETREC, and DO

(define (check-bindings who b opt)
  (cond ((null? b))
        ((and (pair? b)
              (pair? (car b))
              (symbol? (caar b))
              (pair? (cdar b))
              (or (null? (cddar b))
                  (and opt
                       (pair? (cddar b))
                       (null? (cdddar b)))))
          (check-bindings who (cdr b) opt))
        (else
          (error (string-append who ": syntax error") b))))

; Split bindings into variables and arguments

(define (split-bindings bs)
  (define (split3 bs vs as opt)
    (cond ((null? bs)
            (list (reverse! vs)
                  (reverse! as)
                  (reverse! opt)))
          (else
            (split3 (cdr bs)
                    (cons (caar bs) vs)
                    (cons (cadar bs) as)
                    (if (null? (cddar bs))
                        (cons (caar bs) opt)
                        (cons (caddar bs) opt))))))
  (split3 bs '() '() '()))

; Full LET, with syntax checking and named LET

(define-syntax let
  (let ((check-bindings check-bindings)
        (split-bindings split-bindings))
    (lambda (a1 a2 . as)
      (cond ((symbol? a1)
              (if (null? as)
                  (error "named let: missing body"
                         `(let ,a1 ,a2 ,@as)))
              (check-bindings "named let" a2 #f)
              (let ((va (split-bindings a2)))
                (let ((v (car va))
                      (a (cadr va)))
                  `((letrec ((,a1 (lambda ,v ,@as)))
                      ,a1) ,@a))))
            (else
               (check-bindings "let" a1 #f)
               (let ((va (split-bindings a1)))
                 (let ((v (car va))
                       (a (cadr va)))
                   `((lambda ,v ,a2 ,@as) ,@a))))))))

; LETREC with syntax checking

(define-syntax letrec
  (let ((check-bindings check-bindings)
        (split-bindings split-bindings))
    (lambda (bs  x . xs)
      (check-bindings "letrec" bs #f)
      (let ((va (split-bindings bs)))
        (let ((ts (map (lambda (x) (gensym)) bs))
              (vs (car va))
              (as (cadr va)))
          (let ((undefs  (map (lambda (v) (list v #f))
                              vs))
                (tmp-bs  (map (lambda (t a) (list t a))
                              ts
                              as))
                (updates (map (lambda (v t) (list 'set! v t))
                              vs
                              ts)))
            `(let ,undefs
               (let ,tmp-bs
                 ,@updates
                 ,x
                 ,@xs))))))))

(define-syntax let*
  (let ((check-bindings check-bindings))
    (lambda (bs x . xs)
      (define (nest-let bs)
        (if (null? bs)
            `(let () ,x ,@xs)
            `(let (,(car bs))
              ,(nest-let (cdr bs)))))
      (check-bindings "let*" bs #f)
      (nest-let bs))))

(define-syntax (case key . cs)
  (define (gen-cases k cs)
    (cond ((null? cs) (void))
          ((or (not (pair? cs))
               (not (pair? (car cs)))
               (not (pair? (cdar cs))))
            (error "case: syntax error" cs))
          ((eq? 'else (caar cs))
            `(begin ,@(cdar cs)))
          (else
            `(if (memv ,k ',(caar cs))
                 (begin ,@(cdar cs))
                 ,(gen-cases k (cdr cs))))))
  (let ((k (gensym)))
    `(let ((,k ,key))
       ,(gen-cases k cs))))

(define-syntax do
  (let ((check-bindings check-bindings)
        (split-bindings split-bindings))
    (lambda (bs test . body)
      (if (or (not (pair? test))
              (not (list? (cdr test))))
          (error "do: syntax error" test))
      (check-bindings "do" bs #t)
      (let ((loop (gensym))
            (v+a+s (split-bindings bs)))
        (let ((vs (car   v+a+s))
              (as (cadr  v+a+s))
              (ss (caddr v+a+s)))
          `(letrec
             ((,loop
                (lambda ,vs
                  (if ,(car test)
                      (begin ,@(cdr test))
                      (begin ,@body (,loop ,@ss))))))
             (,loop ,@as)))))))

(define-syntax (delay expr)
  `(let ((value #f))
     (lambda ()
       (if value
           (car value)
           (let ((x ,expr))
             (if value
                 (car value)
                 (begin (set! value (list x))
                        (car value))))))))

(define (force x) (x))

;; Equality

(define (equal? a b)
  (cond ((eq? a b))
        ((and (pair? a)
              (pair? b)
              (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (string? a)
              (string? b)
              (string=? a b)))
        ((and (vector? a)
              (vector? b)
              (equal? (vector->list a)
                      (vector->list b))))
        (else
          (eqv? a b))))

(define (assoc x a)
  (cond ((null? a) #f)
        ((equal? (caar a) x) (car a))
        (else (assoc x (cdr a)))))

(define (member x a)
  (cond ((null? a) #f)
        ((equal? (car a) x) a)
        (else (member x (cdr a)))))

;; List procedures

; Using Hare and Tortoise

(define (list? x)
  (define (l? x y)
    (cond ((eq? x y) #f)
          ((null? x))
          ((pair? x) (or (null? (cdr x))
                         (and (pair? (cdr x))
                              (l? (cddr x) (cdr y)))))
          (else      #f)))
  (or (null? x)
      (and (pair? x)
           (l? (cdr x) x))))

;; Folding and Mapping

; Auxilary procedures

; Can't name it MAP or indefinite recursion will happen

(define (map-car f a)
  (define (mapcar1 a r)
    (if (null? a)
        (reverse! r)
        (mapcar1 (cdr a)
                 (cons (f (car a)) r))))
  (mapcar1 a '()))

(define car-of
  (let ((map-car map-car))
    (lambda (as)
      (map-car car as))))

(define cdr-of
  (let ((map-car map-car))
    (lambda (as)
      (map-car cdr as))))

(define (any-null as) (memq '() as))

(define fold-left
  (let ((car-of   car-of)
        (cdr-of   cdr-of)
        (any-null any-null))
    (lambda (f b . as)
      (define (fold as r)
        (if (any-null as)
            r
            (fold (cdr-of as)
                  (apply f r (car-of as)))))
      (if (null? as)
          (error "fold-left: too few arguments")
          (fold as b)))))

(define fold-right
  (let ((car-of   car-of)
        (cdr-of   cdr-of)
        (any-null any-null)
        (map-car  map-car))
    (lambda (f b . as)
      (define (foldr as r)
        (if (any-null as)
            r
            (foldr (cdr-of as)
                   (apply f (append (car-of as)
                                    (list r))))))
      (if (null? as)
          (error "fold-right: too few arguments")
          (foldr (map-car reverse as) b)))))

(define map
  (let ((car-of   car-of)
        (cdr-of   cdr-of)
        (any-null any-null))
    (lambda (f . as)
      (define (map2 as r)
        (if (any-null as)
            (reverse! r)
            (map2 (cdr-of as)
                  (cons (apply f (car-of as))
                        r))))
      (if (null? as)
          (error "map: too few arguments")
          (map2 as '())))))

(define (for-each f . as)
  (if (null? as)
      (error "for-each: too few arguments")
      (apply map f as))
  (void))

;; Integer Arithmetics

(define gcd
  (let ((fold-left fold-left))
    (lambda a
      (define (gcd2 a b)
        (cond ((zero? b) a)
              ((zero? a) b)
              ((< a b) (gcd2 a (remainder b a)))
              (else (gcd2 b (remainder a b)))))
      (fold-left gcd2 0 (map abs a)))))

(define lcm
  (let ((fold-left fold-left))
    (lambda a
      (define (lcm2 a b)
        (let ((cd (gcd a b)))
          (* cd (* (quotient a cd)
                   (quotient b cd)))))
      (fold-left lcm2 1 (map abs a)))))

(define (modulo a b)
  (let ((r (remainder a b)))
    (cond ((zero? r) 0)
          ((eq? (negative? a) (negative? b)) r)
          (else (+ b r)))))

;; Input/output procedures

(define (newline . p)
  (apply write-char #\newline p))

(define (call-with-input-file s p1)
  (let ((in (open-input-file s)))
    (let ((r (p1 in)))
      (close-input-port in)
      r)))

(define (call-with-output-file s p1)
  (let ((out (open-output-file s)))
    (let ((r (p1 out)))
      (close-output-port out)
      r)))

(define (with-input-from-file s p0)
  (let ((old (current-input-port))
        (in  (open-input-file s)))
    (set-input-port! in)
    (let ((r (p0)))
      (set-input-port! old)
      (close-input-port in)
      r)))

(define (with-output-to-file s p0)
  (let ((old (current-output-port))
        (out (open-output-file s)))
    (set-output-port! out)
    (let ((r (p0)))
      (set-output-port! old)
      (close-output-port out)
      r)))

;; Real number arithmetics

(define (expt x y)
  (define (square x) (* x x))
  (define (expt2 x y)
    (cond ((zero? y) 1)
          ((even? y) (square (expt2 x (quotient y 2))))
          (else      (* x (square (expt2 x (quotient y 2)))))))
  (cond ((negative? y)
          (/ (expt (exact->inexact x) (- y))))
        ((zero? x)
          (if (inexact? y)
              (if (positive? y)
                  0
                  (/ 1 0))
              (expt2 x y)))
        ((integer? y)
          (expt2 x y))
        (else
          (exp (* y (log x))))))

(define (round x)
  (let ((x+ (+ 0.5 x)))
    (let ((rx (floor x+)))
      (if (and (odd? (inexact->exact rx))
               (= x+ rx))
          (- rx 1)
          rx))))

(define (exp x)
  (define (e-series x i x^y y! r last)
    (if (<= (abs (- last r)) *epsilon*)
        r
        (e-series x
                  (+ 1 i)
                  (* x^y x)
                  (* y! (+ 1 i))
                  (+ r (/ x^y y!))
                  r)))
  (if (>= x 2.0)
      (let ((e^x/2 (exp (/ x 2))))
        (* e^x/2 e^x/2))
      (+ 1 (e-series x 1 x 1 0.0 1.0))))

(define (log x)
  (define (l-series6 x y x^y r last lim)
    (cond ((and lim (zero? lim))
            r)
          ((<= (abs (- last r)) *epsilon*)
            (* 2 r))
          (else
            (l-series6 x
                      (+ 2 y)
                      (* x^y x x)
                      (+ r (/ x^y y))
                      r
                      (if lim (- lim 1) lim)))))
  (define (l-series x y r last lim)
    (let ((x (/ (- x 1) (+ x 1))))
      (l-series6 x y x r last lim)))
  (cond ((not (positive? x))
          (/ 0))
        ((< 0.1 x 5)
          (l-series x 1 0.0 1.0 #f))
        (else
          (let ((approx (l-series x 1 0.0 1.0 5)))
            (let ((a (/ x (exp approx))))
              (+ approx (log a)))))))

; Auxiliary definitions for SIN, COS, TAN, ATAN

(define pi 3.141592653589793238462643383279502884197169399375105820974944)
(define pi/4  (/ pi 4))
(define pi/2  (/ pi 2))
(define 3pi/4 (+ pi/2 pi/4))
(define 3pi/2 (+ pi pi/2))
(define 5pi/4 (+ pi pi/4))
(define 7pi/4 (+ pi 3pi/4))
(define 2pi   (+ pi pi))

(define ->circle
  (let ((2pi 2pi))
    (lambda (x)
      (let* ((x+ (abs x))
             (d  (* 2pi (floor (/ x+ 2pi))))
             (x+ (- x+ d)))
         (if (negative? x)
             (- 2pi x+)
             x+)))))

; Used by SIN, COS, ATAN, and EXP:

(define (factorial x)
  (define (fact2 x y)
    (if (< x 2)
        y
        (let ((q (quotient x 2)))
          (* (fact2 q y)
             (fact2 (- x q) (+ y q))))))
  (fact2 x 1))

(define sine-series 
  (let ((factorial factorial))
    (lambda (x y r add last)
      (if (<= (abs (- last r)) *epsilon*)
          r
          (sine-series x
                       (+ 2 y)
                       (if add
                           (+ r (/ (expt x y)
                                (factorial y)))
                           (- r (/ (expt x y)
                                (factorial y))))
                       (not add)
                       r)))))

(define cos
  (let ((->circle    ->circle)
        (sine-series sine-series)
        (pi          pi)
        (pi/2        pi/2)
        (3pi/2       3pi/2)
        (2pi         2pi))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((= 0     x)       (if (inexact? x) 1.0 1))
              ((= pi/2  x)        0.0)
              ((= pi    x)       -1.0)
              ((= 3pi/2 x)        0.0)
              ((<= 0    x pi/2)  (sine-series    x         2 1.0 #f 0))
              ((<= pi/2 x pi)    (- (sine-series (- pi x)  2 1.0 #f 0)))
              ((<= pi   x 3pi/2) (- (sine-series (- x pi)  2 1.0 #f 0)))
              (else              (sine-series    (- 2pi x) 2 1.0 #f 0)))))))

(define sin
  (let ((->circle    ->circle)
        (sine-series sine-series)
        (pi          pi)
        (pi/2        pi/2)
        (3pi/2       3pi/2)
        (2pi         2pi))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((= 0     x) (if (inexact? x) 0.0 0))
              ((= pi/2  x)  1.0)
              ((= pi    x)  0.0)
              ((= 3pi/2 x) -1.0)
              (else (let ((z (cond ((<= 0    x  pi/2) x)
                                   ((<= pi/2 x  pi)   (- pi x))
                                   ((<= pi   x 3pi/2) (- x pi))
                                   (else              (- 2pi x)))))
                      (if (> x pi)
                          (- (sine-series z 3 z #f 0))
                          (sine-series z 3 z #f 0)))))))))

(define tan
  (let ((->circle ->circle)
        (pi       pi)
        (pi/4     pi/4)
        (3pi/4    3pi/4)
        (5pi/4    5pi/4)
        (7pi/4    7pi/4))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((or (= x 0)     (= x  pi))   (if (inexact? x) 0.0 0))
              ((or (= x  pi/4) (= x 5pi/4))  1.0)
              ((or (= x 3pi/4) (= x 7pi/4)) -1.0)
              (else                         (/ (sin x) (cos x))))))))

(define atan
  (let ((pi/2      pi/2)
        (factorial factorial))
    (letrec
      ((at-series
         (lambda (x y r last)
           (if (<= (abs (- last r)) *epsilon*)
               r
               (at-series x
                          (+ 1 y)
                          (+ r (* (/ (* (expt 2 (+ y y))
                                        (expt (factorial y) 2))
                                     (factorial (+ y y 1)))
                                  (/ (expt x (+ y y 1))
                                     (expt (+ 1 (* x x))
                                           (+ 1 y)))))
                          r)))))
      (lambda (x)
        (cond ((negative? x)
                (- (atan (- x))))
              ((> x 1)
                (- pi/2 (atan (/ x))))
              (else
                (at-series x 0.0 0 1)))))))

(define (asin x)
  (cond ((= 1 x)
          (* 2 (atan x)))
        ((negative? x)
          (- (asin (- x))))
        (else
          (atan (/ x (sqrt (- 1 (* x x))))))))

(define acos
  (let ((pi   pi)
        (pi/2 pi/2))
    (lambda (x)
      (cond ((= -1 x) pi)
            ((=  1 x) 0)
            (else (- pi/2 (asin x)))))))

(define (sqrt square)
  (define (sqrt2 x last)
    (if (<= (abs (- last x)) *epsilon*)
        x
        (sqrt2 (/ (+ x (/ square x)) 2)
               x)))
  (if (negative? square)
      (error "sqrt: negative argument" square)
      (sqrt2 square 0)))

;; String <-> number conversion

(define (number->string n . r)

  ; Number of decimal places in an integer.
  ; R must be zero.
  ; E.g.:  (places 123 0)  ==>  3

  (define (places n r)
      (if (zero? n)
          (if (zero? r) 1 r)
          (places (quotient n 10) (+ 1 r))))

  ; Digits up to base-36.

  (define digits
    (list->vector
      (string->list
        "0123456789abcdefghijklmnopqrstuvwxyz")))

  ; Convert natural number N to base R, return
  ; a list of digits representing N base R.
  ; RES must be '().
  ; E.g.:  (c-nat 123 10 '())  ==>  (#\1 #\2 #\3)

  (define (c-nat n r res)
    (if (zero? n)
        (if (null? res) '(#\0) res)
        (c-nat (quotient n r)
               r
               (cons (vector-ref
                       digits
                       (inexact->exact (remainder n r)))
                     res))))

  ; Convert integer N to list of digits (base R).

  (define (c-int n r)
    (if (negative? n)
        (list->string (cons #\- (c-nat (abs n) r '())))
        (list->string (c-nat n r '()))))

  ; Remove trailing zeros from string.
  ; E.g.:  (trim-zeros "12300")  ==>  "123"
  ;        (trim-zeros "0")      ==>  "0"

  (define (trim-zeros s)
    (let loop ((i (- (string-length s) 1)))
      (if (and (>= i 0)
               (char=? #\0 (string-ref s i)))
          (loop (- i 1))
          (if (negative? i)
              "0"
              (substring s 0 (+ 1 i))))))

  ; Convert real number m*10^e to string in
  ; scientific notation.
  ; E.g.:  (c-sci-real 12345 -3)  ==>  "1.2345e-3"

  (define (c-sci-real m e)
    (let ((m-s (c-int m 10))
          (e-s (c-int e 10))
          (i   (if (negative? m) 2 1)))
      (let ((k (string-length m-s)))
        (string-append (substring m-s 0 i)
                       "."
                       (if (= k i)
                           "0"
                           (trim-zeros (substring m-s i k)))
                       "e"
                       (if (>= e 0) "+" "")
                       e-s))))

  ; String of N zeros.

  (define (zeros n)
    (do ((n n (- n 1))
         (z '() (cons #\0 z)))
      ((<= n 0) (list->string z))))

  ; Convert real number m*10^e to string.
  ; NP is the total number of decimal places
  ; in the mantissa.
  ; E.g.:  (c-exp-real 12345 -3 5)  ==>  "12.345"

  (define (c-exp-real m e np)
    (let ((a (abs m))
          (p (+ e np)))
      (string-append
        (if (negative? m) "-" "")
        (cond ((negative? p) "0.")
              ((zero? p)     "0")
              (else          ""))
        (zeros (- p))
        (let ((a-s (c-int a 10)))
          (if (<= 0 p np)
              (string-append
                (substring a-s 0 p)
                "."
                (substring a-s p np)
                (if (= p np) "0" ""))
              a-s))
        (if (> p np)
            (string-append (zeros (- p np)) ".0")
            ""))))

  ; Convert real number X to string.
  ; If X<0.0001 or X>=1e9, use scientific notation.

  (define (c-real x)
    (let* ((m  (mantissa x))
           (e  (exponent x)))
      (let ((np (places m 0)))
        (if (< -4 (+ e np) 10)
            (c-exp-real m e np)
            (c-sci-real m (+ e np -1))))))

  ; Get optional radix argument.

  (define (radix)
    (cond ((null? r) 10)
          ((<= 2 (car r) 36) (car r))
          (else (error "number->string: invalid radix"
                       (car r)))))

  (let ((r (radix)))
    (cond ((and (inexact? n)
                (not (= 10 r)))
            (error (string-append
                     "number->string: "
                     "real number needs a radix of 10")
                   n))
          ((and (exact? n)
                (integer? n))
            (c-int (inexact->exact n) r))
          (else
            (c-real (exact->inexact n))))))

(define (string->number s . r)

  (define make-inexact #f)  ; #i prefix flag
  (define make-exact   #f)  ; #e prefix flag

  ; Decimal places in an integer.
  ; R = initial count.

  (define (places n r)
      (if (zero? n)
          (if (zero? r) 1 r)
          (places (quotient n 10) (+ 1 r))))

  ; Digits up to base-36.

  (define digits
    (string->list
      "0123456789abcdefghijklmnopqrstuvwxyz"))

  ; Value of digit X.
  ; 36 means not a valid digit given any base 2..36.
  ; E.g.:  (d-value #\g)  ==>  16

  (define (d-value x)
    (define (v x d n)
      (cond ((null? d) 36)
            ((char=? (car d) x) n)
            (else (v x (cdr d) (+ n 1)))))
    (v (char-downcase x) digits 0))

  ; Is C an exponent marker?

  (define (exp-marker c)
    (memv c '(#\d #\D #\e #\E #\f
              #\F #\l #\L #\s #\S)))

  ; RESULT structure with slots VALUE and REST.

  (define result cons)
  (define value  car)
  (define rest   cdr)

  ; Failure indicator and test procedures.
  ; (failed? res) ==> #t means that a string
  ; passed to a procedure is not a valid
  ; number.

  (define FAIL           '(#f . #f))
  (define (ok? res)      (cdr res))
  (define (failed? res)  (not (ok? res)))

  ; Convert characters in LST to natural
  ; number. V is a leading digit, R is
  ; the radix. When LST is (), return (V).
  ; E.g.:  (c-nat-opt '(#\2 #\3) 1 10)  ==>  (123)

  (define (c-nat-opt lst v r)
    (if (null? lst)
        (result v '())
        (let ((d (d-value (car lst))))
          (if (< d r)
              (c-nat-opt (cdr lst)
                     (+ d (* v r))
                     r)
              (result v lst)))))

  ; Convert LST to natural number. There
  ; must be at least one digit in LST.

  (define (c-nat lst r)
    (if (null? lst)
        FAIL
        (c-nat-opt lst 0 r)))

  ; Convert LST to integer (base R).
  ; E.g.:  (c-int '(#\- #\2 #\7) 10)  ==>  (-27)

  (define (c-int lst r)
    (cond ((null? lst)
            FAIL)
          ((char=? #\+ (car lst))
            (c-nat (cdr lst) r))
          ((char=? #\- (car lst))
            (let ((res (c-nat (cdr lst) r)))
              (if (ok? res)
                  (result (- (value res))
                          (rest res))
                  FAIL)))
          (else
            (c-nat lst r))))

  ; Convert integer X to fractional part of real number.
  ; X has leading one attached in order to make leading
  ; zeros significant.
  ; E.g.:  (make-fract 10023)  ==>  0.0023

  (define (make-fract x)
    (let ((d (places x -1)))  ; 123 --> 0.123
      (- (/ x (expt 10.0 d)) 1.0)))

  ; Make real number from a given integer part,
  ; fractional part and exponent.
  ; E.g.:  (make-real 123 1456 0)  ==>  123.456

  (define (make-real int fract e)
    (let ((v (* (+ 0.0 (abs int) (make-fract fract))
                (expt 10.0 e))))
      (if (negative? int) (- v) v)))

  ; Given integer part INT and fractional part FRAC,
  ; parse an exponent (e.g. "10" in "2e10") and return
  ; the corresponding real number.
  ; E.g.:  (c-exp 123 1456 '(#\1))  ==>  (1234.56)

  (define (c-exp int fract lst)
    (if (null? lst)
        FAIL
        (let ((exp (c-int lst 10)))
          (if (failed? exp)
              FAIL
              (result
                (make-real int fract (value exp))
                (rest exp))))))

  ; Given integer part INT parse an fractional part
  ; (e.g. "34" in "12.34") and return the corresponding
  ; real number. Also handle exponents after the
  ; fraction,
  ; E.g.:  (c-frac 12 '(#\3 #\4))  ==>  (12.34)

  (define (c-frac int lst)
    (cond ((null? lst)
            ; trailing #\.
            (result (exact->inexact int) '()))
          ((exp-marker (car lst))
            (c-exp int 10 (cdr lst)))
          (else
            (let ((fract (c-nat-opt lst 1 10)))
              (cond ((null? (rest fract))
                      (result
                        (make-real int (value fract) 0)
                        '()))
                     ((exp-marker (car (rest fract)))
                       (c-exp int
                              (value fract)
                              (cdr (rest fract))))
                     (else
                       FAIL))))))

  ; Make sure that real numbers are base-ten.

  (define (assert-radix r)
    (if (not (= 10 r))
        (error (string-append
                 "string->number: real number"
                 " needs a radix of 10"))))

  ; Is there a mantissa in the input
  ; in front of any the exponent marker?
  ; (Skip over sign and dot characters.)

  (define (mantissa? x)
    (cond ((null? x)               #f)
          ((char-numeric? (car x)) #t)
          ((exp-marker (car x))    #f)
          (else (mantissa? (cdr x)))))

  ; Parse a number.
  ;
  ; First parse an integer. If the rest is zero, return it.
  ; If it is followed by an exponent marker or a dot,
  ; parse the rest of a real number and return it.

  (define (c-number lst r)
    (let ((int (c-int lst r)))
      (cond ((failed? int)
              FAIL)
            ((and (zero? (value int))  ; "" or "e"
                  (not (mantissa? lst)))
              FAIL)
            ((null? (rest int))
              int)
            ((exp-marker (car (rest int)))
              (assert-radix r)
              (c-exp (value int)
                     10
                     (cdr (rest int))))
            ((char=? #\. (car (rest int)))
              (assert-radix r)
              (c-frac (value int)
                      (cdr (rest int))))
            (else
              FAIL))))

  ; Get optional radix argument.

  (define (radix)
    (cond ((null? r) 10)
          ((<= 2 (car r) 36) (car r))
          (else (error "string->number: invalid radix"
                       (car r)))))

  ; Handle various # prefixes (#x, #e, etc) and convert
  ; string to number.

  (let ((r   (radix))
        (lst (string->list s)))
    (if (and (> (string-length s) 1)
                (char=? #\# (car lst)))
        (let ((mod (cadr lst)))
          (set! lst (cddr lst))
          (case mod
                ((#\e) (set! make-exact #t))
                ((#\i) (set! make-inexact #t))
                ((#\d) #t)
                ((#\b) (set! r 2))
                ((#\o) (set! r 8))
                ((#\x) (set! r 16))
                (else  (set! lst '())))))
    (let ((res (cond ((null? lst)
                       FAIL)
                     ((char=? #\- (car lst))
                       (c-number (cdr lst) r))
                     (else
                       (c-number lst r)))))
      (if (null? (rest res))
          (let ((v (if (char=? #\- (car lst))
                       (- (value res))
                       (value res))))
            (cond (make-inexact
                    (exact->inexact v))
                  (make-exact
                    (if (integer? v)
                        (inexact->exact v)
                        #f))
                  (else
                    v)))
          #f))))

;; Error handling

(define-syntax (catch-errors v x . xs)
  (let ((g  (gensym))
        (r  (gensym))
        (et (gensym))
        (ev (gensym)))
    `(let ((,et *error-tag*)
           (,ev *error-value*))
       (let ((,r (catch
                   (lambda (,g)
                     (set! *error-value* ,v)
                     (set! *error-tag* ,g)
                     ,x . ,xs))))
         (set! *error-tag* ,et)
         (set! *error-value* ,ev)
         ,r))))

;; Utilities

(define (print . xs)
  (cond ((null? xs)
          (newline))
        ((null? (cdr xs))
          (write (car xs))
          (newline))
        (else
          (write (car xs))
          (write-char #\space)
          (apply print (cdr xs)))))

(define (locate-file file)
  (let loop ((paths *library-path*))
    (and (not (null? paths))
         (let ((full-path (string-append (car paths) "/" file)))
           (if (file-exists? full-path)
               full-path
               (loop (cdr paths)))))))

(define load-from-library 
  (let ((locate-file locate-file))
    (lambda (file)
      (define (do-load file)
        (if (not *loading*)
            (begin (display "; loading from ")
                   (display file)
                   (newline)))
        (load file))
      (cond ((locate-file file)
              => do-load)
            ((locate-file (string-append file ".scm"))
              => do-load)
            (else (error "cannot locate file" file))))))

(define-syntax (require-extension . xs)
  (define (isect a b)
    (cond ((null? a)
            '())
          ((memq (car a) b)
            (cons (car a) (isect (cdr a) b)))
          (else
            (isect (cdr a) b))))
  (do ((xs xs (cdr xs))
       (na '()))
      ((null? xs)
        (if (not (null? na))
            (error "extension(s) required, but not compiled-in"
                   (reverse! na))))
    (cond ((memq (car xs) *extensions*))
          ((and (pair? (car xs))
                (eq? 'or (caar xs))
                (not (null? (isect (cdar xs) *extensions*)))))
          (else (set! na (cons (car xs) na))))))

;; Compatibility procedures

(define (argv n)
  (if (>= n (length (command-line)))
      #f
      (list-ref (command-line) n)))

(define (environ s) (environment-variable s))
(define (system s)  (system-command s))

;; Make remaining special forms APPLY-able

(define (command-line) (command-line))
(define (current-error-port) (current-error-port))
(define (current-input-port) (current-input-port))
(define (current-output-port) (current-output-port))
(define (gensym) (gensym))
(define (quit) (quit))
(define (symbols) (symbols))

(define (abs x) (abs x))
(define (boolean? x) (boolean? x))
(define (caaaar x) (caaaar x))
(define (caaadr x) (caaadr x))
(define (caaar x) (caaar x))
(define (caadar x) (caadar x))
(define (caaddr x) (caaddr x))
(define (caadr x) (caadr x))
(define (caar x) (caar x))
(define (cadaar x) (cadaar x))
(define (cadadr x) (cadadr x))
(define (cadar x) (cadar x))
(define (caddar x) (caddar x))
(define (cadddr x) (cadddr x))
(define (caddr x) (caddr x))
(define (call-with-current-continuation p)
        (call-with-current-continuation p))
(define call/cc call-with-current-continuation)
(define (catch x) (catch x))
(define (catch-tag? x) (catch-tag? x))
(define (cdaaar x) (cdaaar x))
(define (cdaadr x) (cdaadr x))
(define (cdaar x) (cdaar x))
(define (cdadar x) (cdadar x))
(define (cdaddr x) (cdaddr x))
(define (cdadr x) (cdadr x))
(define (cdar x) (cdar x))
(define (cddr x) (cddr x))
(define (cddaar x) (cddaar x))
(define (cddadr x) (cddadr x))
(define (cddar x) (cddar x))
(define (cdddar x) (cdddar x))
(define (cddddr x) (cddddr x))
(define (cdddr x) (cdddr x))
(define (ceiling x) (ceiling x))
(define (char->integer x) (char->integer x))
(define (char-alphabetic? c) (char-alphabetic? c))
(define (char-downcase c) (char-downcase c))
(define (char-lower-case? c) (char-lower-case? c))
(define (char-numeric? c) (char-numeric? c))
(define (char-upcase c) (char-upcase c))
(define (char-upper-case? c) (char-upper-case? c))
(define (char-whitespace? c) (char-whitespace? c))
(define (char? x) (char? x))
(define (close-input-port x) (close-input-port x))
(define (close-output-port x) (close-output-port x))
(define (delete-file x) (delete-file x))
(define (dump-image x) (dump-image x))
(define (environment-variable x) (environment-variable x))
(define (eof-object? x) (eof-object? x))
(define (eval x) (eval x))
(define (even? x) (even? x))
(define (exact->inexact x) (exact->inexact x))
(define (exact? x) (exact? x))
(define (exponent x) (exponent x))
(define (file-exists? x) (file-exists? x))
(define (floor x) (floor x))
(define (inexact->exact x) (inexact->exact x))
(define (inexact? x) (inexact? x))
(define (input-port? x) (input-port? x))
(define (integer->char x) (integer->char x))
(define (integer? x) (integer? x))
(define (length x) (length x))
(define (list->string x) (list->string x))
(define (list->vector x) (list->vector x))
(define (load x) (load x))
(define (mantissa x) (mantissa x))
(define (negative? x) (negative? x))
(define (not x) (not x))
(define (null? x) (null? x))
(define (number? x) (number? x))
(define (odd? x) (odd? x))
(define (open-append-file x) (open-append-file x))
(define (open-input-file x) (open-input-file x))
(define (open-output-file x) (open-output-file x))
(define (output-port? x) (output-port? x))
(define (pair? x) (pair? x))
(define (positive? x) (positive? x))
(define (procedure? x) (procedure? x))
(define (real? x) (real? x))
(define (reverse! x) (reverse! x))
(define (s9:bytecode x) (s9:bytecode x))
(define (set-input-port! x) (set-input-port! x))
(define (set-output-port! x) (set-output-port! x))
(define (stats x) (stats x))
(define (string->list x) (string->list x))
(define (string->symbol x) (string->symbol x))
(define (string-copy x) (string-copy x))
(define (string-length x) (string-length x))
(define (string? x) (string? x))
(define (symbol->string x) (symbol->string x))
(define (symbol? x) (symbol? x))
(define (system-command x) (system-command x))
(define (truncate x) (truncate x))
(define (vector->list x) (vector->list x))
(define (vector-length x) (vector-length x))
(define (vector? x) (vector? x))
(define (zero? x) (zero? x))

(define (assq x a) (assq x a))
(define (assv x a) (assv x a))
(define (eq? x y) (eq? x y))
(define (eqv? x y) (eqv? x y))
(define (list-ref x y) (list-ref x y))
(define (list-tail x y) (list-tail x y))
(define (memq x a) (memq x a))
(define (memv x a) (memv x a))
(define (quotient x y) (quotient x y))
(define (remainder x y) (remainder x y))
(define (set-car! x y) (set-car! x y))
(define (set-cdr! x y) (set-cdr! x y))
(define (string-fill! x y) (string-fill! x y))
(define (string-ref x y) (string-ref x y))
(define (throw x y) (throw x y))
(define (vector-fill! x y) (vector-fill! x y))
(define (vector-ref x y) (vector-ref x y))

(define (string-set! x y z) (string-set! x y z))
(define (vector-set! x y z) (vector-set! x y z))
(define (substring x y z) (substring x y z))

(define (read . x)
  (if (null? x)
      (read)
      (read (car x))))

(define (read-char . x)
  (if (null? x)
      (read-char)
      (read-char (car x))))

(define (peek-char . x)
  (if (null? x)
      (peek-char)
      (peek-char (car x))))

(define (display x . y)
  (if (null? y)
      (display x)
      (display x (car y))))

(define (error x . y)
  (cond ((null? y)
          (error x))
        ((null? (cdr y))
          (error x (car y)))
        (else
          (error "error: too many arguments"))))

(define (make-string x . y)
  (cond ((null? y)
          (make-string x))
        ((null? (cdr y))
          (make-string x (car y)))
        (else
          (error "make-string: too many arguments"))))

(define (make-vector x . y)
  (cond ((null? y)
          (make-vector x))
        ((null? (cdr y))
          (make-vector x (car y)))
        (else
          (error "make-vector: too many arguments"))))

(define (write x . y)
  (if (null? y)
      (write x)
      (write x (car y))))

(define (vector-copy v . xs)
  (cond ((null? xs)
          (vector-copy v))
        ((null? (cdr xs))
          (vector-copy v (car xs)))
        ((null? (cddr xs))
          (vector-copy v (car xs) (cadr xs)))
        ((null? (cdddr xs))
          (vector-copy v (car xs) (cadr xs) (caddr xs)))
        (else
          (error "vector-copy: wrong number of arguments"))))

(define (append . as)
  (define (conc a r)
    (if (null? a)
        r
        (conc (cdr a)
              (append (car a) r))))
  (if (null? as)
      '()
      (let ((as (reverse as)))
        (conc (cdr as) (car as)))))

(define (bit-op op x y . ys)
  (fold-left (lambda (a b) (bit-op op a b)) x (cons y ys)))

(define (+ . x) (fold-left (lambda (x y) (+ x y)) 0 x))

(define (* . x) (fold-left (lambda (x y) (* x y)) 1 x))

(define (string-append . x)
  (fold-left (lambda (a b) (string-append a b)) "" x))

(define (vector-append . x)
  (fold-left (lambda (a b) (vector-append a b)) #() x))

(define (- x . y)
  (if (null? y)
      (- x)
      (fold-left (lambda (x y) (- x y)) x y)))

(define (/ x . y)
  (if (null? y)
      (/ x)
      (fold-left (lambda (x y) (/ x y)) x y)))

(define (max x . y)
  (if (null? y)
      x
      (fold-left (lambda (x y) (max x y)) x y)))

(define (min x . y)
  (if (null? y)
      x
      (fold-left (lambda (x y) (min x y)) x y)))

(define (%compare op a)
  (let loop ((a a))
    (cond ((null? (cdr a)))
          ((op (car a) (cadr a))
            (loop (cdr a)))
          (else #f))))

(define (< x y . z)
  (%compare (lambda (x y) (< x y))
           (cons x (cons y z))))

(define (<= x y . z)
  (%compare (lambda (x y) (<= x y))
           (cons x (cons y z))))

(define (= x y . z)
  (%compare (lambda (x y) (= x y))
           (cons x (cons y z))))

(define (> x y . z)
  (%compare (lambda (x y) (> x y))
           (cons x (cons y z))))

(define (>= x y . z)
  (%compare (lambda (x y) (>= x y))
           (cons x (cons y z))))

(define (char-ci<=? x y . z)
  (%compare (lambda (x y) (char-ci<=? x y))
           (cons x (cons y z))))

(define (char-ci<? x y . z)
  (%compare (lambda (x y) (char-ci<? x y))
           (cons x (cons y z))))

(define (char-ci=? x y . z)
  (%compare (lambda (x y) (char-ci=? x y))
           (cons x (cons y z))))

(define (char-ci>=? x y . z)
  (%compare (lambda (x y) (char-ci>=? x y))
           (cons x (cons y z))))

(define (char-ci>? x y . z)
  (%compare (lambda (x y) (char-ci>? x y))
           (cons x (cons y z))))

(define (char<=? x y . z)
  (%compare (lambda (x y) (char<=? x y))
           (cons x (cons y z))))

(define (char<? x y . z)
  (%compare (lambda (x y) (char<? x y))
           (cons x (cons y z))))

(define (char=? x y . z)
  (%compare (lambda (x y) (char=? x y))
           (cons x (cons y z))))

(define (char>=? x y . z)
  (%compare (lambda (x y) (char>=? x y))
           (cons x (cons y z))))

(define (char>? x y . z)
  (%compare (lambda (x y) (char>? x y))
           (cons x (cons y z))))

(define (string-ci<=? x y . z)
  (%compare (lambda (x y) (string-ci<=? x y))
           (cons x (cons y z))))

(define (string-ci<? x y . z)
  (%compare (lambda (x y) (string-ci<? x y))
           (cons x (cons y z))))

(define (string-ci=? x y . z)
  (%compare (lambda (x y) (string-ci=? x y))
           (cons x (cons y z))))

(define (string-ci>=? x y . z)
  (%compare (lambda (x y) (string-ci>=? x y))
           (cons x (cons y z))))

(define (string-ci>? x y . z)
  (%compare (lambda (x y) (string-ci>? x y))
           (cons x (cons y z))))

(define (string<=? x y . z)
  (%compare (lambda (x y) (string<=? x y))
           (cons x (cons y z))))

(define (string<? x y . z)
  (%compare (lambda (x y) (string<? x y))
           (cons x (cons y z))))

(define (string=? x y . z)
  (%compare (lambda (x y) (string=? x y))
           (cons x (cons y z))))

(define (string>=? x y . z)
  (%compare (lambda (x y) (string>=? x y))
           (cons x (cons y z))))

(define (string>? x y . z)
  (%compare (lambda (x y) (string>? x y))
           (cons x (cons y z))))

(define (apply f . as)
  (let loop ((as as)
             (a '()))
    (cond ((null? as)
            (error "apply: too few arguments"))
          ((null? (cdr as))
            (if (pair? (car as))
                (apply f (append (reverse a)
                                 (car as)))
                (error "apply: expected list" (car as))))
          (else 
            (loop (cdr as)
                  (cons (car as) a))))))
