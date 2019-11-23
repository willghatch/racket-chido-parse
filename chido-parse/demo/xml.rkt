#lang chido-parse/bnf-syntactic

;document : prolog element Misc*
;; For now let's do a simplified version...
document : element
element : EmptyElemTag
        | STag content ETag

EmptyElemTag : "<" Name Attribute* "/>"
STag : "<" Name Attribute* ">"
ETag : "</" Name ">"
Attribute : Name "=" AttValue
AttValue : "\"" ($(char-not-in "<&\"") | Reference)* "\""
         | "'" ($(char-not-in "<&'") | Reference)* "'"


;; 	content	   ::=   	CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
content	:CharData? @ ((element | Reference | CDSect | PI | Comment) CharData?)*


;;CDSect : CDStart CData CDEnd
;;CDStart : "<![CDATA["
;;CDEnd : "]]>"
CDSect : "<![CDATA[" CData "]]>"
;;;; CData and CharData are built below as a procedure
;;CData : (Char* - (Char* "]]>" Char*))
;; CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)

Reference : EntityRef | CharRef
EntityRef :  "&" Name ";"
CharRef : "&#" $(cr "09")+ ";"
        | "&#x" ($(cr "09") | $(cr "af") | $(cr "AF"))+ ";"

;NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
NameStartChar : ":" | $(cr "AZ") | "_" | $(cr "az") | $(cr "#xC0#xD6") | $(cr "#xD8#xF6") | $(cr "#xF8#x2FF") | $(cr "#x370#x37D") | $(cr "#x37F#x1FFF") | $(cr "#x200C#x200D") | $(cr "#x2070#x218F") | $(cr "#x2C00#x2FEF") | $(cr "#x3001#xD7FF") | $(cr "#xF900#xFDCF") | $(cr "#xFDF0#xFFFD") | $(cr "#x10000#xEFFFF")
;	NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
NameChar : NameStartChar | "-" | "." | $(cr "09") | "#xB7" | $(cr "#x0300#x036F") | $(cr "#x203F#x2040")
Name : NameStartChar NameChar*
Names : Name ("#x20" Name)*

Comment : "<!--"
          ($(char-not-in "-") | ($(char-parser "-") $(char-not-in "-")))
          "-->"

;;PI	   ::=   	'<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
;;PITarget	   ::=   	Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
PI : "<?" PITarget $PI-text "?>"
;; Ignore the reservation of the name "XML".
PITarget : Name

;;Misc : Comment | PI | <whitespace>
Misc : Comment | PI

;;;; Prolog stuff
;;[22]   	prolog	   ::=   	XMLDecl? Misc* (doctypedecl Misc*)?
;;[23]   	XMLDecl	   ::=   	'<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
;;[24]   	VersionInfo	   ::=   	S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
;;[25]   	Eq	   ::=   	S? '=' S?
;;[26]   	VersionNum	   ::=   	'1.' [0-9]+

#:definitions
(require racket/string)

(define (not-dash? c)
  (not (equal? c (string-ref "-" 0))))
(define cr char-range-parser)

(define (make-cdata excluding-regexp)
  (proc-parser
   #:promise-no-left-recursion? #t
   (λ (port)
     (let loop ([chars '()])
       (define valid-char? (regexp-match-peek (pregexp "^[^<&]") port))
       (define invalid-prefix?
         (and excluding-regexp (regexp-match-peek excluding-regexp port)))
       (define eof? (eof-object? (peek-char port)))
       (if (and valid-char? (not eof?) (not invalid-prefix?))
           (loop (cons (read-char port) chars))
           (apply string (reverse chars)))))))
(define CharData (make-cdata (pregexp "^\\]\\]>")))
(define PI-text (make-cdata (pregexp "^\\?>")))
(define CData (make-cdata #f))


(define Char any-char-parser)
(define (char-not-in str)
  (result-filter any-char-parser (λ (c) (string-contains? str (string c)))))


(module* main racket/base
  (require
   (submod "..")
   racket/cmdline
   chido-parse
   racket/stream
   )

  (define file
    (command-line #:args (xml-file) xml-file))
  (define port (open-input-file file))
  (define parse-result
    (whole-parse* port
                  (bnf-parser->with-surrounding-layout
                   parser)))
  (cond [(parse-failure? parse-result)
         (printf "~a\n" (parse-failure->string/chain parse-result))]
        [(stream-empty? (stream-rest parse-result))
         (printf "~v\n" (parse-derivation-result (stream-first parse-result)))]
        [else
         (printf "AMBIGUOUS PARSE!\n\n")
         (printf "Result 1:\n")
         (printf "~v\n" (parse-derivation-result (stream-first parse-result)))
         (printf "\n\nResult 2:\n")
         (printf "~v\n" (parse-derivation-result (stream-first (stream-rest
                                                                parse-result))))
         (printf "\n\n total number of results...\n")
         (printf "~v\n" (stream-length parse-result))
         ])
  )
