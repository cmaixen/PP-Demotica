#lang racket
(#%require "Utillities.rkt")
(#%require "Devices.rkt")

(require db)
(require scheme/mpair)

(provide make-steward)
(#%require "Logsystem.rkt")

(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite")


(define (make-steward location logsystem)
  
  (let ((db (sqlite3-connect  #:database db-pad
                              #:mode 'read/write))
        (list-of-objects '())
        (location location)
        (logsystem (make-logsystem)))
    
    ;Private Hulpfunctie: parse-answer
    ;zorgt ervoor dat je het de eigelijke informatie dat je nodig hebt terug krijgt en anders false geeft
    
    (define (parse-answer lst)
      (let ((ACK-sign (car lst))
            (commando (cadr lst)))
        (if (eq? ACK-sign 'NACK)
            (error 'device "Commando ~a not acknowledged!" commando )
            (let*((result (cadr lst))
                  (resulttype (car result))
                  (resultval (cadr result)))
              resultval))))
    
    
    
    (define (connect?)
      (connected? db))
    
    (define (get-location)
      location)
    
    ;communiceren met database voor informatie
    

    
    (define (list_devices_names . type) 
      (if (null? type)
      (list_neutralizer (query-rows db "select * from the_devices") 0)
      (list_neutralizer (query-rows db  "select * from the_devices where type = $1" (car type)) 0)))
    
    (define (list_devices_serial . type)
      (if (null? type)
      (list_neutralizer (query-rows db "select * from the_devices") 1)
       (list_neutralizer (query-rows db "select * from the_devices where type = $1" (car type)) 1)))
    
    (define (list_devices_com_adress . type)
      (if (null? type)
      (list_neutralizer (query-rows db "select * from the_devices") 2)
      (list_neutralizer (query-rows db  "select * from the_devices where type = $1" (car type)) 2)))
    
    (define (list_devices_type . type)
      (if (null? type)
       (list_neutralizer (query-rows db "select * from the_devices") 3)
      (list_neutralizer (query-rows db "select * from the_devices where type = $1" (car type)) 3)))
    
    ;communiceren met devices voor informatie
    
    ;let op type moeten worden meegeven als een character om de vergelijk te doen slagen, parse answer geeft namelijke charachtesr terug
    
    (define  (list_devices_info command . type)
      (define (loop-all lst result)
        (if (empty? lst)
            result 
            (let* ((first-obj (car lst))
                   (output (send first-obj 'get-device-output-port))
                   (input (send first-obj 'get-device-input-port)))
              (write `(get ,command) output)
              (loop-all (cdr lst) (cons (parse-answer (read input)) result)))))
      
        (define (loop-spec lst result)
        (if (empty? lst)
            result 
            (let* ((first-obj (car lst))
                   (output (send first-obj 'get-device-output-port))
                   (input (send first-obj 'get-device-input-port)))
              (write `(get type) output)
           (let ((answer (parse-answer (read input))))
             (newline)
             (display answer)
             (newline)
             (display (car type))
             (display (string? answer)) 
             (display (string? (car type))) 
              (if (equal? answer (car type))
              (loop-spec (cdr lst) (cons answer result))
              (loop-spec (cdr lst) result))))))
      
      (if (null? type)
           (loop-all (reverse list-of-objects) '())
      (loop-spec (reverse list-of-objects) '())))
  
     
    
    (define (list_devices_status . type)
      (send logsystem 'status-update location)
      (if (null? type)
      (list_devices_info 'status)
      (list_devices_info 'status (car type))))
    
    
    
    ;voegt een gegeven device toe aan de deviceslist
    
    (define (update-devices type name serialnumber)
      (set! list-of-objects  (cons (make-device type name serialnumber) list-of-objects)))
    
    ;je bekijkt de naam als primary key, je let er ook op dat er geen dubbele serial in staat 
    
    (define (add-device type name serialnumber com-adress)
      ;apparaat en port in database toevoegen 
      (cond ((query-maybe-row db "select name from the_devices where name = $1" name);we namen de lamp als primary key
             (error 'steward "You can not add two devices with the same name!"))
            ((query-maybe-value db "select serialnumber from the_devices where serialnumber = $1" serialnumber)
             (error 'steward "serialnumber must be unique!"))
            (else
             (query-exec db "insert into the_devices values ($1, $2 , $3 , $4)" name serialnumber com-adress type)
             (update-devices type name serialnumber)
             (send logsystem 'add-device name location))))
    
    
    (define (delete_device device-name)
      ;uit database verwijderen
      (query-exec db "DELETE from the_devices where name = $1" device-name)
      ;uit objectenlijst verwijderen
      (set! list-of-objects (filter (lambda (x) (not (equal? device-name (send x 'get-name-sim)))) list-of-objects))
      ;update logfile
      (send logsystem 'delete-device device-name location))
    
    
    
    (define (look-up-device name arg) 
      (define (loop lst)
        (if (empty? lst)
            (begin 
              (error 'steward  "the device ~a doesn't exist!" name)
              #f)
            (let* ((first-object (car lst))
                   (name-object (send first-object 'get-name-sim)))
              (if (equal? name-object  name )
                  (send first-object arg)
                  (loop (cdr lst))))))
      
      (loop list-of-objects))
    
    
    
    
    (define (open-output-device name)
      (look-up-device name 'get-device-output-port))
    
    
    (define (open-input-device name)
      (look-up-device name 'get-device-input-port))
    
    
    (define (initialize device_list)
      (define (loop lst)
        (if (eq? (list_size lst) 0)
            (display "initialization done")
            (let* ((devicevector (car lst))
                   (device_name (vector-ref devicevector 0))
                   (device_serial (vector-ref devicevector 1))
                   (device_comadress (vector-ref devicevector 2))
                   (device_type (vector-ref devicevector 3)))
              (set! list-of-objects (cons (make-device device_type device_name device_serial) list-of-objects))
              (loop (cdr lst)))))
      (loop device_list))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    (define (dispatch message)
      (case message
        ((connect?) connect?)
        ((add-device) add-device)
        ((get-location) get-location)
        ((list_devices_names) list_devices_names)
        ((open-output-device) open-output-device)
        ((open-input-device) open-input-device)
        ((list_devices_status) list_devices_status)
        ((list_devices_type) list_devices_type)
        ((list_devices_com_adress) list_devices_com_adress)
        ((list_devices_serial) list_devices_serial)
        ((delete_device) delete_device)
        (else (error 'steward "unknown message ~a" message))))
    
    
    ;initialization of Steward
    ;Het genereren van de objectlijst
    (initialize  (query-rows db "select * from the_devices"))
    
    dispatch ))
