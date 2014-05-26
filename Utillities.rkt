;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Domotica Energie Monitoring Systeem                  *-*-
;-*-*                        Yannick Merckx                           *-*-
;-*-* Programmeerproject 2013-2014 2de Bachelor Computerwetenschappen *-*-
;-*-*           Student van Vrije Universiteit Brussel                *-*-
;-*-*                                                                 *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; Beschrijving: Deze file bevat hulpprocedures die over heel het programmma worden gebruikt.

; Procedures:

; - send: argumenten: object (een object, in realiteit is dit een dipatch-procedure), message (de boodschap je wil sturen naar het object)(symbol)
;         output: any (resultaat van boodschap aan object)

; - list_size: argumenten: lijst (pair)
;              output: number (de grootte van de lijst)

; - list_neutralizer: argumenten: query (query die een resultaatlijst teruggeeft met vectors daarin), element (number)(positie van de kolom waar je de informatie uitwilt) 
;                     output: (listof string? ...) (lijst met de gevraagde element in)

; - reverse-list:    argumenten: lijst (pair)
;                     output: geeft de omgekeerde lijst terug


; - list_devices_spectype:  procedure is een generaliserende functie om duplicatie te vermijden
;                           argumenten: list_w_stewards (listof steward-object ...),  type (string) (duidt op het type van toestellen dat je wil) message (de boodschap))  
;                           Output: (listof string?) (lijst met de gevraagde informate van alle toestellen van het gegeven type)


; Commentaar: geen

#lang racket
(#%require "Constants.rkt")
(provide (all-defined-out))

;Hulpprocedures
;--------------
; Zend een boodschap (met eventuele parameters) naar een object georiëenteerd gebaseerde implementatie van een ADT.

(define (send object message . parameters)
  (let ((procedure (object message))) 
    ;Er van uit gaande dat het object zijn dispatcher altijd een procedure teruggeeft.
    (apply procedure parameters)))



;Berekent de lengte van een lijst

(define (list_size lst)
  
  (define (loop lst counter)
    
    (cond  ((empty? lst) 0)
           ((empty? (cdr lst)) counter )
       
           (else  (loop (cdr lst) (+ counter 1)))))
  (loop lst 1))


;zorgt er voor dat de resultaat lijst van de query met daarin vectors  ongevormt wordt  naar een lijst met daarin het gevraagde elementen
(define (list_neutralizer query el)
  
      (define (loop lst neut_lst)
        (if (empty? lst) 
            neut_lst
            (let* ((vector (car lst))
                   (name (vector-ref vector el)))
              (loop (cdr lst) (cons name neut_lst)))))
        (loop query '()))

(define (reverse-list lst)
  (define (loop lstt res)
     (if (empty? lstt)
         res
         (loop (cdr lstt) (cons (car lstt) res))))
  (loop lst '()))


    ;procedure bepaalt welke standaardwaarde er moet gegeven  worden
(define (mesurement_default type)
  (if (equal? type temperaturesensor_type)
      temperaturesensor_default_value
      lightswitch_default_value))


;generieke functie om stewardoverschrijdend informatie op te vragen 

;VEREISTEN! MESSAGE MOET EEN LITERAL ZIJN!

  (define (list_devices_spectype list_w_stewards type message majordomo)
     (define (loop stewardlist result)
       (if (empty? stewardlist)
       result
       (let ((first-steward (car stewardlist)))
         ;de majordomo moet weten om welke steward het gaat
         (send majordomo 'set-current-steward first-steward)
       (newline) 
         (display "type spectype")
         (display type)
         (newline)
         (display message)
         (newline)
      (loop (cdr stewardlist) (append result (send majordomo message type))))))
      (loop list_w_stewards '()))

  
      (define (send-over-tcp message steward . type)
      (let* ((in '())
             (out '())
             (ip (get_ip steward))
             (port (get_port steward))
             (message (cons message type)))
        (let-values ([(pi po) (tcp-connect ip port)])
          (display "Connecting with ip ....")
          (newline)
          (set! in pi)
          (set! out po))
        (display (read in))
        (newline)
        (write message out)
        (flush-output out)
        (read in)))
      
         ;geeft ip van de steward
    (define (get_ip steward)
      (let ((serveradress (cadr steward)))
        (car serveradress)))
    
    ;geeft port van de steward
    (define (get_port steward)
      (let ((serveradress (cadr steward)))
        (cdr serveradress)))
    
    
;reply_value bij een error is een string
;ander kan het eender wat zijn
;ATTENTION: does not return a boolean
(define (acknowledged reply)
  (let* ((raw_output reply)
         (acknowledge_sign  (car raw_output))
         (reply_value (cadr raw_output)))
    (if (eq? 'ACK acknowledge_sign)
        reply_value
        (begin (display reply_value)
        (error reply_value)))))
