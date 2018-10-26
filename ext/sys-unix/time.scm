; Scheme 9 from Empty Space, Unix Function Library
; By Nils M Holm, 2010, 2018
; In the public domain
;
; (time* expression)  ==>  object
; (time  form)        ==>  object
;
; The TIME* procedures evaluates EXPRESSION, measuring the number
; of allocations and garbage collections with the STATS procedure.
; It also measures the time spent reducing FORM. When finished, it
; prints some interesting data and returns the normal form of FORM.
; The FORM must be quoted or it will be reduced *before* running
; TIME*.
;
; The TIME special form is like TIME*, but does not require its
; argument to be quoted.
;
; (Example): (time (begin (expt 2 10000) #t))  ==>  #t
;            ;    0.4086 seconds
;            ; 8,334,993 total nodes allocated
;            ; 8,334,993 conses allocated
;            ;     3,456 vector cells allocated
;            ;        22 garbage collections

(require-extension sys-unix)

(load-from-library "format.scm")

(define (time* form)
  (letrec
    ((sval->integer
       (lambda (sval)
         (let loop ((sval sval)
                    (int  0))
           (if (null? sval)
               int
               (loop (cdr sval)
                     (+ (* 1000 int)
                        (car sval)))))))
     (seconds
       (lambda (t0 tn)
         (let ((d (- (car tn) (car t0))))
           (if (< (cadr tn) (cadr t0))
               (- d 1)
               d))))
     (useconds
       (lambda (t0 tn)
         (let* ((d (if (< (cadr tn) (cadr t0))
                       (- 1000000 (- (cadr t0) (cadr tn)))
                       (- (cadr tn) (cadr t0))))
                (s (number->string d))
                (k (string-length s))
                (s (string-append (make-string (- 6 k) #\0) s)))
           (substring s 0 4)))))
  (let* ((t0     (sys:gettimeofday))
         (result (apply stats form '()))
         (sval*  (cdr result))
         (tn     (sys:gettimeofday)))
    (format #t "; ~15@A.~4,,,'0:A seconds~%~
                ; ~20,,:D total nodes allocated~%~
                ; ~20,,:D conses allocated~%~
                ; ~20,,:D vector cells allocated~%~
                ; ~20,,:D garbage collections~%"
               (seconds t0 tn)
               (useconds t0 tn)
               (sval->integer (car sval*))
               (sval->integer (cadr sval*))
               (sval->integer (caddr sval*))
               (sval->integer (cadddr sval*)))
    (car result))))

(define-syntax (time form)
  `(time* ',form))
