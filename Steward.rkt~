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

; Beschrijving: ADT Steward ; Argumenten: Majordomo-object, logsystem-object , steward-table (naam van table waar men de informatie voor de devices kan vinden) (string?)
; Output: Steward object
; Messages:

; - add-device:  argumenten: geen
;                output: void

; - delete_device: argumenten: geen
;                  output: void

; - get-location: argumenten: geen
;                 output: locatie van de steward (string?)


; - open-output-device: argumenten: de naam van het toestel (string?)
;          output: outputport van toestel

; - open-input-device: argumenten: de naam van het toestel (string?)
;                 output: inputport van het toestel

; - list_devices_names: argumenten: 1) no argument
;                                   2) optional argument: type (string?) 
;                        output: 1) pair (lijst met namen van de toestellen) j
;                                2) pair (lijst met de namen van de toestellen van het gegeven type)

; - list_devices_status: argumenten: 1) no argument
;                                   2) optional argument: type  (string?) 
;                        output: 1) pair (lijst met statussen van de toestellen)
;                                2) pair (lijst met de statussen van de toestellen van het gegeven type)

; - list_devices_type: argumenten: 1) no argument
;                                  2) optional argument: type  (string?) 
;                      output: 1) pair (lijst met types van de toestellen)
;                                2) pair (lijst met types van de toestellen van het gegeven type)

; - list_devices_com_adress: argumenten: 1) no argument
;                                        2) optional argument: type  (string?) 
;                             output: 1) pair (lijst met communicatieadressen van de toestellen)
;                                     2) pair (lijst met communicatieadressen van de toestellen van het gegeven type)

; - list_devices_serial:  argumenten: 1) no argument
;                                     2) optional argument: type  (string?) 
;                          output:    1) pair (lijst met serienummers van de toestellen)
;                                     2) pair (lijst met serienummers van de toestellen van het gegeven type)

; -list_devices_location:    argumenten: 1) no argument
;                                        2) optional argument: type  (string?) 
;                             output: 1) pair (lijst met locaties van de toestellen)
;                                     2) pair (lijst met locaties van de toestellen van het gegeven type)

; -  list_devices_mesurement: argumenten: 1) no argument
;                                         2) optional argument: type  (string?) 
;                             output: 1) pair (lijst met metingen van de toestellen)
;                                     2) pair (lijst met metingen van de toestellen van het gegeven type)
;
; - list_devices_mesurement_with_value:  argumenten: 1) no argument
;                                                    2) optional argument: type  (string?) 
;                                        output: 1) pair (lijst met metingen met eenheden van de toestellen)
;                                                2) pair (lijst met metingen met eenheden van de toestellen van het gegeven type)
; - change-mesurement: argumenten: naam van het toestel (string?) , value (de waarde waar de status naar moet worden aangepast) (string?)
;          output: void

; - change-status: argumenten: naam van het toestel (string?) , value (de waarde waar de status naar moet worden aangepast) (bool)
;                 output: void

;
; Commentaar: list_devices_location en list_devices_type hebben nog de mogelijkheid om performanter geschreven te worden, voorlopig blijven ze zo staan vanwege vereenvoudiiging. 
;              Verder aanpassing hieraan zal in de tweede fase nog volgen.



#lang r5rs
(#%require racket/tcp)
(#%require racket/base)
(#%require "Utillities.rkt")
(#%require "Devices.rkt")
(#%require "Logsystem.rkt")
(#%require "xbee_utils.rkt")
(#%require "xbee.rkt")
(#%require "queue.rkt")
(#%provide make-steward)









;parameters logsystem location , maj vallen weg

(define (make-steward location port)
  (let ((location location)
        (already_initialized #f)
        (steward_buffer (make-queue))
        (list_of_devices '()))


     ;initialiseren van het xbee-object
 (define (xbee-initialise XBEE baudrate)
;het maken van een xbee object
  (make-xbee))
 ;xbee object initialiseren
 (define xbee (xbee-initialise XBEE_name 9600))

;De Lokale xbee bepalen
 (define XBEE_name #"/dev/ttyUSB0")

 (define (xbee-list-nodes xbee)
  (send xbee 'xbee-list-nodes))
    
    
    (define (xbee-discover-nodes xbee)
      (send xbee 'xbee-discover-nodes))
    
    
   ;inlezen van xbee buffer 
  (define (xbee-tick xbee)
  (set! steward_buffer (send xbee 'xbee-tick))
    (newline)
    (newline)
    (display "steward bufer empty?: ")
             (display (send steward_buffer 'queue-empty?)))
    
    
    ;message is al geconverteerd naar vector
    ;converteren gebeurd op de steward
    (define (xbee-write xbee device-adress message)
      (send xbee 'xbee-write device-adress message))

    
    ;geeft locatie terug 
    (define (get-location)
      location)
    
   ;converteerd het get_command naar de gepaste vorm
    (define (get_message)
      (string->vector "GET\n"))
   
    ;geeft het adress van het gevraagde toestel
    (define (get-device name) 
      (define (loop lst)
        (if (null? lst)
            (begin 
              (display "the device doesn't exist!")
              #f)
            (let* ((first-object (car lst))
                   (name-object (car first-object))
                   (adress-object (cadr first-object)))
              (if (equal? name-object  name )
                  adress-object
                  (loop (cdr lst))))))
      (loop list_of_devices))
    

    ;initiliazatie
    ;gaat de nodes ontdenken met de xbee
    ;deze worden doorgegeven aan de majordomo en deze gaat dan zijn database updaten
    (define (initialize)
      (display "initialize")
     (xbee-discover-nodes xbee)
      (set! already_initialized #t)
      (set! list_of_devices  (xbee-list-nodes xbee))
      list_of_devices)
    
    
    
    ;DISPATCHER over TCP/IP
    (define (dispatch message out)
      (display "dispatching...")
      (let* ((command (car message))
             (arguments (cdr message))
             (answer (generate-answer command arguments)))
        (begin 
          (write answer out)
          (newline out)
          (flush-output out))
        ))
    
    
    (define (get_acknowledgement vector)
     (vector-loop 1 3))
    
    (define (get_info command)
     (let* ((receive_frame (send steward_buffer 'dequeue!)))
       (display receive_frame)
       (if (equal? (frame_type receive_frame) transmit_answer)
           (let((received_data (vector-loop 13 (- (vector-length receive_frame) 2) receive_frame))) 
         (vector->string received_data))
           (display "ERROR: given frame is not a receive frame!!"))))
    
 
    
    (define (get_device_adress given_device_name)
      (newline)
      (newline)
   (display (list? list_of_devices))
      
      (define (loop lst)
        (if (null? lst)
            (begin (newline)
                   (display "Given Devicename is not linked to device-adress"))
            (let* ((device_pair (car lst))
                  (device_name (car device_pair))
                  (device_adress (cadr device_pair)))
              (if (equal? device_name given_device_name)
                  device_adress
                  (loop (cdr lst))))))
      (loop list_of_devices))
        

    (define (generate-answer command arguments)
      ;argumenten zitten in de lijst in de volgorde van de argumenten van
      (cond ((equal? command 'get-location)
             location)
            ((equal? command 'initialize)
             (initialize))
            ((equal? command 'list_of_devices)
             (display "list of devices caleeed")
             list_of_devices)
            (else
             (let* ((device_name (car arguments))
                    (device_adress (get_device_adress device_name)))
               (push_command command device_adress) ;car arguments stelt het device_adress voor
                  (get_info command)))))
    
    ;Gaat zolang het moet xbee-write doen
    ;packet komen niet altijd aan 
    (define (push_command command device_adress)
      (let ((converted_command (string->vector command)))
        (define (loop keep_on_looping) 
        (if keep_on_looping
        (begin   (xbee-write xbee device_adress converted_command)
                 (xbee-tick xbee)
                 (if (check_on_receive)
                     (loop #f)
                     (loop #t)))
          'done))
        (loop #t)))
  
  
  (define (frame_type frame)
    (vector-ref frame 0))
 
    (define (delivery_status frame)
      (vector-ref frame 4))
  
  
  ;check on receive
  (define  (check_on_receive)
    (newline)
    (newline)
    (display "print queue:")
    (send steward_buffer 'print-queue)
    (newline)
    (let* ((status_frame (send steward_buffer 'dequeue! ))
          (frame_type (frame_type status_frame)))
        (if (equal? frame_type Transmit_status)
            (let ((delivery_status (delivery_status status_frame)))
               (newline)
                (newline)
              (display "STATUS IS INORDE")
              (newline)
                (newline)
              (display  delivery_status)
              (display delivery_status_succes)
          (if (equal? delivery_status delivery_status_succes)
              #t
              #f))
          (begin
            (newline)
            (display "frametype:")
            (display frame_type)
            (newline)
            (display "statusframe:")
            (display status_frame)
            (newline)
            (display "Received frame is not a statusframe")
                  #f))))
          
          

    (define (server listener)
      (let ((in '())
            (out '()))
        (let-values (((pi po) (tcp-accept listener)))
          (set! in pi)
          (set! out po)
          (write "===>> Connection Established! <<===" out)
          (newline out)
          (flush-output out)
          (dispatch (read in)  out)
          )))
    
    ;oneindige loop die zorgt dat je steward blijft gaan
    (define (infinite-loop procedure listener)
      (display "Steward avaible")
      (newline)
      (procedure listener)
      (infinite-loop procedure listener))
    
    
    (infinite-loop server (tcp-listen port 10 #t))
    
))



