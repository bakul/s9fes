; Scheme 9 from Empty Space, Unix Function Library
; By Nils M Holm, 2010,2018
; In the public domain
;
; (scan-help-pages string1)          ==>  list
; (scan-help-pages string1 string2)  ==>  list | unspecific
;
; Search the online help database for entries containing the
; word STRING1 and return a list the names of all pages that
; contain the word. When STRING2 is passed to SCAN-HELP-PAGES,
; it is interpreted as a string of options. The following
; options will be evaluated:
;
;         a  Search for substrings instead of full words.
;            This options typically yields more results.
;         c  Find missing pages, i.e. pages that are linked
;            in the help index but not present in the file
;            system. Ignore STRING1.
;         l  Return not only the page names but also the
;            context (up to three lines) in which the match
;            was found: (page-name (line1 line2 line3)).
;         p  Print the results instead of returning them.
;            In this case, the result of FIND-HELP is
;            unspecific.
;
; Unknown option characters will be ingored.
;
; (Example): (scan-help-pages "help")  ==>  ("help" "locate-file")

(load-from-library "read-line.scm")
(load-from-library "string-find.scm")
(load-from-library "mergesort.scm")
(load-from-library "displaystar.scm")
(load-from-library "name-to-file-name.scm")
(load-from-library "mergesort.scm")
(load-from-library "basename.scm")

(define (search-help-page page what any long)
  (with-input-from-file
    page
    (lambda ()
      (let loop ((line     (read-line))
                 (prev     '()))
        (cond ((eof-object? line)
                '())
              (((if any string-ci-find string-ci-find-word) what line)
                (if long
                    `(,(list (basename page)
                             (append prev
                                     (list line)
                                     (let ((next (read-line)))
                                       (if (eof-object? next)
                                           '()
                                           (list next))))))
                   `(,(basename page))))
              (else
                (loop (read-line) (list line))))))))

(define (print-results pages)
  (for-each (lambda (match)
              (cond ((pair? match)
                      (display* (car match) #\newline)
                      (for-each (lambda (desc)
                                  (if (not (string=? "" desc))
                                      (display* "    " desc #\newline)))
                                (cadr match))
                              (newline))
                    (else
                      (display* match #\newline))))
            pages))


(define (find-help-pages)

  (define index '())

  (let dirloop ((dirs *library-path*))
    (let extloop ((exts (cons "" (map symbol->string
                                      *extensions*))))
      (cond ((null? dirs)
              (mergesort string<=? index))
            ((null? exts)
              (dirloop (cdr dirs)))
            (else
              (let ((path (string-append
                            (car dirs)
                            (if (string=? "" (car exts))
                                "/help"
                                "/help/")
                            (car exts))))
                (set! index
                      (append
                        (if (file-exists? path)
                            (map (lambda (x)
                                   (string-append
                                     path
                                     "/"
                                     (name->file-name
                                       (symbol->string x))))
                                 (map car (with-input-from-file
                                            (string-append
                                              path
                                              "/INDEX")
                                            read)))
                            '())
                        index))
                (extloop (cdr exts))))))))

(define scan-help-pages
  (let ((find-help-pages  find-help-pages)
        (search-help-page search-help-page)
        (print-results    print-results))
    (lambda (what . opts)
      (let ((opts (if (null? opts)
                      '()
                      (string->list (car opts)))))
        (let loop ((pages (find-help-pages))
                   (found '()))
          (cond ((and (null? pages) (memv #\c opts))
                  found)
                ((null? pages)
                  (let ((result (apply
                                  append
                                  (map (lambda (page)
                                         (search-help-page
                                           page
                                           what
                                           (memv #\a opts)
                                           (memv #\l opts)))
                                       (mergesort string<? found)))))
                    (if (memv #\p opts)
                        (print-results result)
                        result)))
                ((file-exists? (car pages))
                  (loop (cdr pages)
                        (if (memv #\c opts)
                            found
                            (cons (car pages) found))))
                ((memv #\c opts)
                  (loop (cdr pages)
                        (cons (car pages) found)))
                (else
                  (loop (cdr pages)
                        found))))))))
