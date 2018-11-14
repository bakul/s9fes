; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2018
; In the public domain
;
; (troffify-char char)      ==>  string
; (troffify-string string)  ==>  string
;
; TROFFIFY-CHAR converts a char to a string that is save for
; inclusion in a TROFF document. TROFFIFY-STRING does the
; same for a string.
;
; Note: the conversion "$" -> "$dollar$" is a local idiosyncrasy.
;
; Example:   (troffify-char #\$)           ==>  "$dollar$"
;            (troffify-string "\"a\\b\"")  ==>  "\"a\\\\b\""

(define (troffify-char c)
  (cond ((char=? c #\\) "\\\\")
        ((char=? c #\$) "$dollar$")
        (else           (string c))))

(define (troffify-string s)
  (apply string-append
         (map troffify-char (string->list s))))
