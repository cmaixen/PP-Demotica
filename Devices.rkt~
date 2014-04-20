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


; Beschrijving: ADT DEVICE ; Argumenten: het type van toestel (string?), naam (string?) , serienummer (string?), steward-object
;      

; Output: Device object

; Messages:
; - get-name-sim: argumenten: geen
;                 output: naam van het toestel (string?)
;                 ("-sim" duidt op het feit dat de naam van het toestel niet via het communicatieprotocol wordt verkregen maar via het disptachingsysteem)

; - get-device-output-port: argumenten: geen
;                 output: outputport

; - get-device-input-port: argumenten: geen
;                          output: inputport

;
; Commentaar: geen


#lang racket
(#%require "Utillities.rkt")


(#%provide make-device)

(define (make-device type name serialnumber steward majordomo given_status mesurement)
  (let-values ([(input output) (make-pipe)]  ;aanmaken van outputfile bij het aanmaken van het object 
               [(current-mesurement) mesurement]   ;je mag er vanuit gaan dat je eerst een outputport hebt aangezien je eerst wil schrijven 
               [(status) given_status]
               [(type) type]
               [(location) (send steward 'get-location)]
               [(unit_of_value) "unknown"])


;correct inintialize

(define (initialize)
  (let ((giventype (string->symbol type)))
    (case giventype
      ((Temperaturesensor) (initialize-tempsensor))
      ((Lightswitch) (initialize-lightswitch))
      (else (error 'devicemaker "Type ~a not recognized!" type)))))


(define (initialize-tempsensor)
  (set! unit_of_value " °C"))


(define (initialize-lightswitch)
  (set! unit_of_value " W"))


(define (get-name-sim)
  name)

(define (get-mesurement_w_v)
  (let ((conv-mesur (string-append (number->string current-mesurement) unit_of_value)))
    `(ACK (MESUREMENT ,conv-mesur))))

(define (get-mesurement)
  (let ((conv-mesur (number->string current-mesurement)))
    `(ACK (MESUREMENT ,conv-mesur))))

(define (get-status)
  `(ACK (STATUS ,status)))


(define (get-name)
  `(ACK (NAME  ,name)))

(define (get-location)
  `(ACK (LOCATION ,location)))

(define (get-type)
  `(ACK (TYPE ,type)))

(define (get-serial) 
  `(ACK (SERIAL ,serialnumber)))


;verandert de meting van het toestel
 ( define (change-mesurement value)
  (set! current-mesurement value)
  '(ACK (NEW-MESUREMENT ,value)))


(define (change-status value)
  (set! status value)
  '(ACK (NEW-STATUS ,value)))

(define (generate-answer)
  (let*((commando (read input))
        (prefix (car commando))  ;wat er wordt verwacht
        (asked (cadr commando)));wat er gevraagd is
    (cond ( (eq? 'get prefix)
            (case asked
              ((name) (get-name))
              ((serial) (get-serial))
              ((status) (get-status))
              ((type) (get-type))
              ((location) (get-location))
              ((mesurement) (get-mesurement))
              ((mesurement_w_v) (get-mesurement_w_v))
              (else `(NACK , commando))))
          ((eq? 'set prefix)
           
           (case asked 
             ((mesurement) (change-mesurement (caddr commando)))
             ((status) (change-status (caddr commando) ))
             (else  `(NACK , commando)))
           `(NACK , commando)))))


(define (get-device-output-port)
  output)



(define (get-device-input-port)
  (make-input-port
   'name 
   (lambda (ignore)
     (let-values (((in out) (make-pipe))
                  ((answer) 'none))
       (set! answer (generate-answer))
       (write answer out)
       in))
   #f
   (lambda () 'closed)))



(define (dispatch message)
  (case message
    ((get-name-sim) get-name-sim)
    ((get-device-output-port) get-device-output-port)
    ((get-device-input-port) get-device-input-port)
    (else (error 'Device "unknown message ~a" message))))

(initialize)

dispatch))

