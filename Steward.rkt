#lang racket
(#%require "Utillities.rkt")
(#%require "Devices.rkt")

(require db)

(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite")


(define (make-steward)
  
  (let ((db (sqlite3-connect  #:database db-pad
                              #:mode 'read/write)))
    
    (define (connect?)
      (connected? db))
    
    
    ;je bekijkt de naam als primary key, je let er ook op dat 
    
    (define (add-device name serialnumber com-adress)
      ;apparaat en port in database toevoegen 
      (cond ((query-exec db "select name from the_devices where name = $1" name)    ;we namen de lamp als primary key
             (error 'steward "You can not add two devices with the same name!"))
            ((query-exec db "select serialnumber from the_devices where serialnumber = $1" serialnumber)
             (error 'steward "serialnumber must be unique!"))
            (else
            (query-exec db "insert into the_devices ($1, $2 , S3)" name serialnumber com-adress))))
    
    
    (define (open-io-device name)
      (if (query-exec db "select name from the_devices where name = $1" name) 
          (send name 'get-device-port)
          (error 'steward "the device ~a doesn't exist!" name)))
    
    
    
    (define (dispatch message)
      (case message
        ((connect?) connect?)
        ((add-device) add-device)
        ((open-io-device) open-io-device)
        (else (error 'steward "unknown message ~a" message))))
    
    dispatch ))

(define test-steward (make-steward))
(define testsensor (make-tempsensor "test" "12345"))

(send test-steward 'add-device "test" "12345" "comadress")

(define testport (send test-steward 'open-io-device))




