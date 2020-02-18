#lang chido-parse/demo/parenhp
<html>
<!-- Use this incantation to make the page display decently on mobile. -->
<meta name="viewport" content="width=device-width, initial-scale=1.0" />
<p>This is a paragraph
<¿`(i ()
      ,(string-append "with a " "racket ")
      ,#<b>escape<¿(list 'a '((href "https://docs.racket-lang.org")
                              (style "color: #ff0000;"))
                         "!!!"
                         #<div style="background-color: #00ff00">??</div>)¿></b>
      ,#<b>!!!</b>)¿>
</p>
</html>
