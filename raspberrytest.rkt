#lang racket


;DISPATCHER over TCP/IP
(define (dispatch message out)
  (let* ((command (car message))
         (answer (generate-answer command)))
         (begin 
           (write answer out)
                (newline out)
                (flush-output out))
         ))

;Returns '(ACK ...) of '(NACK ...)
(define (generate-answer command)
  (display command)
  (newline)
  (cond ((equal? command 'add-device)
         ;needs to be done
         'add-device)
        ((equal? command 'get-location)
         ;needs to be done
         'get-location)
        ((equal? command 'list_devices_names)
         ;needs to be done
         'done)
        ((equal? command 'open-output-device)
         ;needs to be done
         'done)
        ((equal? command 'list_devices_status)
         ;needs to be done
         'done)
        ((equal? command 'open-input-device)
         ;needs to be done
         'done)  
        ((equal? command 'list_devices_type)
         ;needs to be done
         'done)
        ((equal? command 'list_devices_com_adress)
         ;needs to be done
         'done)
        ((equal? command 'list_devices_serial)
         ;needs to be done
         'done) 
        ((equal? command 'list_devices_location)
         ;needs to be done
         'done)
        ((equal? command 'list_devices_mesurement)
         ;needs to be done
         'done)
        ((equal? command 'list_devices_mesurement_with_value)
         ;needs to be done
         'done)
        ((equal? command 'delete_device)
         ;needs to be done
         'done)
        ((equal? command 'change-mesurement)
         ;needs to be done
         'done)
        ((equal? command 'change-status)
         ;needs to be done
         'done)
        (else '(NACK "Invalid Command"))))






(define (server listener)
  (let ((in '())
        (out '())
        )
    (let-values ([(pi po) (tcp-accept listener)])
      (set! in pi)
      (set! out po)
      (write "===>> Connection Established! <<===" out)
      (newline out)
      (flush-output out)
      (dispatch (read in)  out))))


; (generate answer) ;bevat (read in)


(define (infinite-loop procedure listener)
  (display "Steward avaible")
  (newline)
  (procedure listener)
  (infinite-loop procedure listener))



(infinite-loop server (tcp-listen 6664 10 #t))