; Test .warning directive.
;  { dg-do assemble { target cris-*-* } }
 .warning "a warning message"	; { dg-warning "a warning message" }
 .warning a warning message	; { dg-error ".warning argument must be a string" }
 .warning			; { dg-warning "(unspecified)" }
 .warning "(unspecified)"	; { dg-warning "(unspecified)" }
