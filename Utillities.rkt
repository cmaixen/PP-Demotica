#lang racket


(#%provide send)

;Hulpprocedures
;--------------
; Zend een boodschap (met eventuele parameters) naar een object georiëenteerd gebaseerde implementatie van een ADT.

(define (send object message . parameters)
  (let ((procedure (object message))) 
    ;Er van uit gaande dat het object zijn dispatcher altijd een procedure teruggeeft.
    (apply procedure parameters)))

