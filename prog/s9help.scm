#! /usr/local/bin/s9

; s9help -- find and display S9fES help pages
; by Nils M Holm, 2010,2015,2018
; In the public domain
;
; Usage: s9help [-als] topic ...
;
; Options:
;
; -a  find any match (default: full words only)
; -c  check help database for missing files
; -h  print help on help
; -l  long search results (including context)
; -s  search help pages (default: display)

(require-extension sys-unix)

(load-from-library "find-help-page.scm")
(load-from-library "scan-help-pages.scm")
(load-from-library "read-line.scm")
(load-from-library "parse-optionsb.scm")

(define search     (option #\s #f))
(define long       (option #\l #f))
(define any-match  (option #\a #f))
(define check      (option #\c #f))
(define show-help  (option #\h #f))
(define options    `(,search
                     ,long
                     ,any-match
                     ,check
                     ,show-help))

(define (usage)
  (display "Usage: s9help [-achls] topic ...")
  (newline))

(define (display-help topic)
  (let ((path (find-help-page topic)))
    (if (not path)
        (begin (display* "s9help: " topic ": help file not found" #\newline)
               (sys:exit 1))
        (with-input-from-file
          path
          (lambda ()
            (newline)
            (let print ((line (read-line)))
              (if (eof-object? line)
                  (newline)
                  (begin (display line)
                         (newline)
                         (print (read-line))))))))))

(let ((topics (parse-options! (command-line) options usage)))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Find and display S9fES help pages"
                 ""
                 "-a  find any match (default: full words only)"
                 "-c  check help database for missing files"
                 "-l  print help on help"
                 "-l  long search results (including context)"
                 "-s  search help pages (default: display)"
                 ""))
      (sys:exit 0)))
  (if (opt-val check)
      (for-each (lambda (x) (display* "missing: " x #\newline))
                (scan-help-pages "" "c"))
      (let ((topics (if (null? topics)
                        (begin (usage)
                               (sys:exit 1))
                        topics)))
        (for-each (lambda (topic)
                    (if (opt-val search)
                        (scan-help-pages
                          topic
                          (string-append
                            "p"
                            (if (opt-val long) "l" "")
                            (if (opt-val any-match) "a" "")))
                        (display-help topic)))
                  topics))))
