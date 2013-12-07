#lang racket

(require racket/date)
(#%require "Utillities.rkt")
(provide make-logsystem)

(define (make-logsystem)
  (let ((adress-logfile "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/Logfile.txt"))
    
    ;instellingen:
    
    ;format van de datum   
    (date-display-format 'iso-8601) 
    
    
    ;public:
    
    ;schrijven van delete logs
    
    (define (delete-device device-name location)
      (write-log (~a "DELETE: Device" "'" device-name "'" "from " location "Deleted" #:separator " ")))
    
    ;schrijven van add logs
    
    (define (add-device device-name location)
      (write-log  (~a "ADD: Device" "'" device-name "'" "added in" location #:separator " ")))
    
    ;schrijven van update logs
    
    
    (define (status-update location)
    (write-log (~a "UPDATE STATUS: Status of devices of steward at" location "updated" #:separator " ")))
    
    
   ;schrijven Login logs
    
    (define (succesfull-login username)
      (write-log  (~a "LOGIN SUCCESFULL: You succefull logged in with" username #:separator " ")))
    
    
    (define (failed-login username)
      (write-log (~a "LOGIN FAILED: You Failed to logged in with" username #:separator " ")))
    
    
    ;private: 
    
    (define (write-log string)
      (let ((output (open-output-file adress-logfile #:exists 'append)))
        (display "- " output)
        (display (date->string (current-date) #t) output)
        (display ":  " output)
        (display string output)
        (newline output)
        (close-output-port output)))
    
    (define (dispatch message)
      (case message 
        ((delete-device) delete-device)
        ((add-device) add-device)
        ((status-update) status-update)
        ((succesfull-login) succesfull-login)
        ((failed-login) failed-login)
        (else (error 'logsystem  "unknown message ~a" message))))
    
    
    dispatch))
  
  
  
  (define log (make-logsystem))
  
  
  