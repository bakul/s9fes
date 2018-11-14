; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2018
; In the public domain
;
; (make-help-index)  ==>  list
;
; Return a comprehensive list of topics found in the online help
; database. Topics include procedures (like CAR and +), syntactic
; keywords (like IF and DEFINE) as well as special variables, such
; as *LIBRARY-PATH*. The returned list will be lexicographically
; sorted and free of duplicates.
;
; (Example): (make-help-index)  ==>  (list of procedures and keywords)

(load-from-library "flatten.scm")
(load-from-library "list-to-set.scm")
(load-from-library "mergesort.scm")

(define (make-help-index)

  (define index '())

  (let dirloop ((dirs *library-path*))
    (let extloop ((exts (cons "" (map symbol->string
                                      *extensions*))))
      (cond ((null? dirs)
              (mergesort (lambda (a b)
                           (string<=? (symbol->string a)
                                      (symbol->string b)))
                         (list->set (flatten index))))
            ((null? exts)
              (dirloop (cdr dirs)))
            (else
              (let ((path (string-append
                            (car dirs)
                            (if (string=? "" (car exts))
                                "/help"
                                "/help/")
                            (car exts)
                            "/INDEX")))
                (set! index
                      (append (if (file-exists? path)
                                  (with-input-from-file path read)
                                  '())
                              index))
                (extloop (cdr exts))))))))
