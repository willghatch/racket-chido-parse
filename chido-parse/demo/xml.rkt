#lang chido-parse/bnf-syntactic

;document : prolog element Misc*
;; For now let's do a simplified version...
% document : prolog element
% element : EmptyElemTag
          | n = STag
            @content
            /$(result-filter ETag (λ (r) (equal? (car (syntax->datum r))
                                                 (car (syntax->datum
                                                       (parse-derivation-result n))))))

/EmptyElemTag : /"<" Name Attribute* /"/>"
/STag : /"<" Name Attribute* /">"
/ETag : /"</" Name /">"
% Attribute : Name /"=" AttValue
% AttValue : /"\"" @($(char-not-in "<&\"") | Reference)* /"\"" :: chars->string
           | /"'" @($(char-not-in "<&'") | Reference)* /"'" :: chars->string


;; 	content	   ::=   	CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
/%content : @ $non-null-CharData ? @@ ((element | Reference | CDSect | PI | Comment) @$non-null-CharData ?)*


;;CDSect : CDStart CData CDEnd
;;CDStart : "<![CDATA["
;;CDEnd : "]]>"
% CDSect : "<![CDATA[" CData "]]>"
;;;; CData and CharData are built below as a procedure
;;CData : (Char* - (Char* "]]>" Char*))
;; CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)

Reference : EntityRef | CharRef
% EntityRef :  "&" Name ";"
% PEReference : "%" Name ";"
% CharRef : "&#" $(cr "09")+ ";"
          | "&#x" ($(cr "09") | $(cr "af") | $(cr "AF"))+ ";"

;;; TODO !!!! Name is parsing wrong due to auto-layout insertion.  For several productions I need a switch to turn it off.  This should be easy, since I think the option exists in bnf-s-exp, but it is not threaded through to bnf-syntactic.

;NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
/ NameStartChar : ":" | $(cr "AZ") | "_" | $(cr "az") | $(cr "#xC0#xD6") | $(cr "#xD8#xF6") | $(cr "#xF8#x2FF") | $(cr "#x370#x37D") | $(cr "#x37F#x1FFF") | $(cr "#x200C#x200D") | $(cr "#x2070#x218F") | $(cr "#x2C00#x2FEF") | $(cr "#x3001#xD7FF") | $(cr "#xF900#xFDCF") | $(cr "#xFDF0#xFFFD") | $(cr "#x10000#xEFFFF")
;	NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
/ NameChar : @NameStartChar | "-" | "." | $(cr "09") | "#xB7" | $(cr "#x0300#x036F") | $(cr "#x203F#x2040")
/ % Name : @NameStartChar @@NameChar*
:: (λ cs (string->symbol (apply string (map ->char cs))))

% Names : Name ("#x20" Name)*

% Comment : /"<!--"
          @($(char-not-in "-") | ($(char-parser "-") $(char-not-in "-")))*
          /"-->" :: (λ cs (list 'Comment (apply chars->string cs)))

;;PI	   ::=   	'<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
;;PITarget	   ::=   	Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
PI : "<?" PITarget $PI-text "?>"
;; Ignore the reservation of the name "XML".
PITarget : Name

;;Misc : Comment | PI | <whitespace>
Misc : Comment | PI


prolog : XMLDecl? Misc* (doctypedecl Misc*)?
XMLDecl : "<?xml" VersionInfo EncodingDecl? SDDecl? "?>"
VersionInfo : "version" "=" (("'" VersionNum "'") | ("\"" VersionNum "\""))
% VersionNum : "1." $(cr "09")+
doctypedecl : "<!DOCTYPE" Name ExternalID? ("[" intSubset "]")? ">"
;; TODO - elem-alts need to support sequences directly, rather than needing them to be put into parens.
EncodingDecl : "encoding" "=" (("\"" EncName "\"") | ("'" EncName "'"))
EncName : ($(cr "az") | $(cr "AZ")) ($(cr "az") | $(cr "AZ") | $(cr "09") | $(char-in "._-"))*
SDDecl : "standalone" "=" (("'" ("yes" | "no") "'") | ("\"" ("yes" | "no") "\""))

;; TODO - There are several more productions related to the prolog, but I want to start testing, and most xml does not include this crap.
;% intSubset : (markupdecl | DeclSep)*
intSubset : ""
;markupdecl : elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
;;markupdecl : elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment

ExternalID : "SYSTEM" SystemLiteral
           | "PUBLIC" PubidLiteral SystemLiteral
SystemLiteral : "'" $(char-not-in "'")* "'"
              | "\"" $(char-not-in "\"")* "\""
PubidLiteral : "'" PubidChar* "'"
             | "\"" PubidChar* "\""
PubidChar : "#x20" | "#xD" | "#xA" | $(cr "az") | $(cr "AZ") | $(cr "09") | $(char-in "-'()+,./:=?;!*#@$_%")
;; TODO - all these "#x20" strings need to be changed to Racket's string escape format

#:definitions
(require racket/string)

(define (not-dash? c)
  (not (equal? c (string-ref "-" 0))))
(define cr char-range-parser)

(define (->char x)
  (cond [(syntax? x) (->char (syntax-e x))]
        ;[(and (list? x) (null? (cdr x))) (->char (car x))]
        [(char? x) x]
        [(and (string? x) (eq? (string-length x) 1)) (string-ref x 0)]
        ;; TODO - CharRef and EntityRef
        [else
         (eprintf "not char: ~v\n" x)
         (error '->char "can't convert to char: ~v\n" x)]))

(define chars->string (λ cs (apply string (map ->char cs))))

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
(define non-null-CharData (result-filter CharData (λ (r) (not (equal? "" r)))))
(define PI-text (make-cdata (pregexp "^\\?>")))
(define CData (make-cdata #f))


(define Char any-char-parser)
(define (char-not-in str)
  (result-filter any-char-parser
                 (λ (c)
                   (not (string-contains? str (string c))))))
(define (char-in str)
  (result-filter any-char-parser
                 (λ (c)
                   (string-contains? str (string c)))))

(module* test racket/base
  (require
   (submod "..")
   rackunit
   chido-parse
   racket/dict
   "../private/test-util-3.rkt"
   )

  (define name-parser (bnf-parser->arm-parser parser 'Name))
  (define attr-parser (bnf-parser->arm-parser parser 'Attribute))
  (define attvalue-parser (bnf-parser->arm-parser parser 'AttValue))
  (define content-parser (bnf-parser->arm-parser parser 'content))

  (check se/datum?
         (wp*/r "hello" name-parser)
         (list #'hello))
  (check se/datum?
         (wp*/r "hello1" name-parser)
         (list #'hello1))
  (check-pred parse-failure?
              (parse* "1hello" name-parser))
  (check se/datum?
         (wp*/r "'foo'" attvalue-parser)
         (list #'"foo"))
  (check se/datum?
         (wp*/r "hello='foo'" attr-parser)
         (list #'(Attribute hello "foo")))
  (check se/datum?
         (wp*/r "<a/>" parser)
         (list #'(document (prolog () () ())
                           (element (a ())))))
  (check se/datum?
         (wp*/r "<a></a>" parser)
         (list #'(document (prolog () () ())
                           (element (a ())))))
  (check se/datum?
         (wp*/r "</a><b/>" (sequence
                            (bnf-parser->arm-parser parser 'ETag)
                            (bnf-parser->arm-parser parser 'EmptyElemTag)))
         (list #'((a) (b ()))))
  (check se/datum?
         (wp*/r "</a>" (bnf-parser->arm-parser parser 'ETag))
         (list #'(a)))
  (check se/datum?
         (wp*/r "<tag_a/><tag_b/>" content-parser)
         (list #'((element (tag_a ()))(element (tag_b ())))))
  (check se/datum?
         (wp*/r "hello <tag/>  test" content-parser)
         (list #'("hello " (element (tag ())) "  test")))
  (check se/datum?
         (wp*/r "hello <!-- this is a comment --> test" content-parser)
         (list #'("hello " (Comment " this is a comment ") " test")))
  (check se/datum?
         (wp*/r "<a><b></b></a>" parser)
         (list #'(document (prolog () () ())
                           (element (a ()) (element (b ()))))))
  (check se/datum?
         (wp*/r "<a><b></b> </a>" parser)
         (list #'(document (prolog () () ())
                           (element (a ()) (element (b ())) " "))))
  )

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
         (printf "~v\n" (syntax->datum
                         (parse-derivation-result (stream-first parse-result))))
         (printf "\n\nResult 2:\n")
         (printf "~v\n" (syntax->datum
                         (parse-derivation-result (stream-first (stream-rest
                                                                 parse-result)))))
         (printf "\n\n total number of results...\n")
         (printf "~v\n" (stream-length parse-result))
         ])
  )
