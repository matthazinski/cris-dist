; Test .error directive.
;  { dg-do assemble { target cris-*-* } }
 .error "an error message"	; { dg-error "an error message" }
 .error an error message	; { dg-error ".error argument must be a string" }
 .error				; { dg-error "(unspecified)" }
 .error "(unspecified)"		; { dg-error "(unspecified)" }
