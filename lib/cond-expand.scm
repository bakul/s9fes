; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2018
; In the public domain
;
; (cond-expand (symbol expression ...))  ==>  object
;
; This is a subset of SRFI-0 COND-EXPAND. Each argument of COND-EXPAND
; is a clause consisting of a leading "feature ID" (a symbol) and any
; number of trailing expressions (a body). It evaluates to the body of
; the first clause having a feature ID of SCHEME-9-FROM-EMPTY-SPACE,
; S9FES, or ELSE. The body is then evaluated by S9fES. When none
; of the above feature IDs is found, an error is reported.
;
; The new Scheme 9 (Reimagined) also defines the S9FES-REIMAGINED and
; SCHEME-9-FROM-EMPTY-SPACE-REIMAGNED identifiers. When testing for
; Reimagined, the test should come before the generic S9fES test,
; because the new S9 also defines the old identifiers.
;
; Example:   (cond-expand (s9fes (cons 1 2)))               ==>  (1 . 2)
;            (cond-expand (foo (cons 1 2)) (else (+ 1 2)))  ==>  3

(define-syntax (cond-expand . cs)
  (letrec
    ((expand
       (lambda (cs)
         (cond ((null? cs)
                 (error "cond-expand: unfulfilled"))
               ((or (not (pair? cs))
                    (not (pair? (car cs))))
                 (error "cond-expand: syntax error" cs))
               ((or (eq? (caar cs) 's9fes)
                    (eq? (caar cs) 'scheme-9-from-empty-space)
                    (eq? (caar cs) 's9fes-reimagined)
                    (eq? (caar cs) 'scheme-9-from-empty-space-reimagined)
                    (eq? (caar cs) 'else))
                 `(begin ,@(cdar cs)))
               (else
                 (expand (cdr cs)))))))
    (expand cs)))
