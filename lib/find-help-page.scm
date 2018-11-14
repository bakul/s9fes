; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2018
; In the public domain
;
; (find-help-page string)  ==>  string | #f
;
; Search the online help database for a file explaining the
; given topic. STRING may contain a Scheme 9 keyword, a
; procedure name, or the name of a special variable. When
; a file describing the desired topic exists, its full path
; will be returned. When no such file exists, the procedure
; returns #F.
;
; (Example): (find-help-page "help")  ==>  "path to help-page"

(load-from-library "name-to-file-name.scm")

(define (find-help-page s)

  (define (find-topic path s)
    (let ((index (string-append path "/INDEX")))
      (let loop ((refs (if (file-exists? index)
                           (with-input-from-file index read)
                           '())))
        (cond ((null? refs) #f)
              ((memq s (car refs))
                (let ((path (string-append
                              path
                              "/"
                              (name->file-name
                                (symbol->string (caar refs))))))
                  (if (file-exists? path)
                      path
                      #f)))
              (else
                (loop (cdr refs)))))))

  (let ((s (if (string? s) (string->symbol s) s)))
    (let dirloop ((dirs *library-path*))
      (let extloop ((exts (cons "" (map symbol->string
                                        *extensions*))))
        (cond ((null? dirs)
                #f)
              ((null? exts)
                (dirloop (cdr dirs)))
              (else
                (let ((path (string-append
                                (car dirs)
                                (if (string=? "" (car exts))
                                    "/help"
                                    "/help/")
                                (car exts))))
                  (cond ((find-topic path s))
                        (else (extloop (cdr exts)))))))))))
