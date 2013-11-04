#lang racket
(require db)

(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite")
(define db (sqlite3-connect  #:database db-pad
                             #:mode 'read/write))

(query-exec db "create table the_numbers (n integer, d varchar(20))")


;Simulator
;Alles is waar de steward met communiceert heeft een naam serienummmer dus hier maken we een superklasse voor

(define device%
  (class object%
    (super-new)
    
    (init-field serialnumber)
    
    (define/public (get-serialnumber) serialnumber)
    
    ))



(define steward%
  (class object% 
    (super-new)
    (define/public (add-device name serialnumber portadress)
      ;apparaat en port in database toevoegen 
      ;checken op duplicatie
      (query-exec db "insert into the_devices ($1, $2 , S3)" name serialnumber portadress) 
      ;bevestiging
         (display "device succefully added!"))
        
        (define/public (open-port name)
          (let ((result (query-exec db "select portadress from the_devices where name = $1" name)))
         (if result
             (query-exec db "select portadress from the_devices where name = $1" name)
             (display "device is not added!"))))
             ))
    
  

                        