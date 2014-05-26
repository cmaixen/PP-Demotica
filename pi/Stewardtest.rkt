#lang r5rs
(#%require racket/tcp)
(#%require racket/base)
(#%require "xbee_utils.rkt")
(#%require "queue.rkt")
(#%require "RacketFFI.rkt")
(#%provide make-steward)

(#%require rnrs/bytevectors-6)










(define (make-steward location port)
  (let ((location location)
        (already_initialized #f)
        (steward_buffer (make-queue))
        (list_of_devices '()))
    
    
    ;De Lokale xbee bepalen
    (define XBEE_name #"/dev/ttyUSB0")
    ;xbee object initialiseren
    (define xbee (xbee-initialise XBEE_name 9600))
    
    
    
    
    
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
      (sleep 10)
      (xbee-tick xbee)
      (display (xbee-list-nodes))
      (set! already_initialized #t)
      (set! list_of_devices  (xbee-list-nodes))
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
            ((equal? command `initialize)
             (initialize))
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
                       (send steward_buffer 'enqueue! (xbee-read-frame xbee))
                       (if (check_on_receive)
                           (loop #f)
                           (begin (sleep 1)
                                  (loop #t))))
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
      
      (let* ((status_frame  (send steward_buffer 'dequeue! )))
        
        (if (equal? status_frame #())
            #f
            
            (let ((frame_type (frame_type status_frame)))
              
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
                        (begin 
                          (send steward_buffer 'enqueue! (xbee-read-frame xbee))
                           (display "print queue:")
                           (send steward_buffer 'print-queue)
                          #t)
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
                    #f))
              )
            )))
    
    
    
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

(define (send object message . parameters)
  (let ((procedure (object message))) 
    ;Er van uit gaande dat het object zijn dispatcher altijd een procedure teruggeeft.
    (apply procedure parameters)))

(make-steward "Livingroom" 6666)