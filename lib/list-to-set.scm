; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2018
; In the public domain
;
; (list->set list)  ==>  list
;
; (load-from-library "list-to-set.scm")
;
; Convert list to set. A set is a list containing unique members.
;
; Example:   (list->set '(a b c b c))  ==>  (a b c)

(load-from-library "hash-table.scm")

(define (list->set a)
  (define (l->s a r)
    (cond ((null? a)
            (reverse! r))
          ((member (car a) r)
            (l->s (cdr a) r))
          (else
            (l->s (cdr a)
                  (cons (car a) r)))))
  (define (l->s/hash a r)
    (let ((h (make-hash-table)))
      (let loop ((a a) (r r))
        (cond ((null? a)
                (reverse! r))
              ((hash-table-ref h (car a))
                (loop (cdr a) r))
              (else
                (hash-table-set! h (car a) #t)
                (loop (cdr a)
                      (cons (car a) r)))))))
  (if (> (length a) 500)
      (l->s/hash a '())
      (l->s a '())))
