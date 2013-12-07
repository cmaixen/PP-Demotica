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
    

+;Testing van object
+
+(define test (make-object device% "yannick"))
+(send test get-serialnumber)

(define pgc (sqlite3-connect  #:database "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite"
#:mode 'read/write))

(connected? pgc)

(query-exec pgc
   "create temporary table the_devices (naam text, serial string ,port varchar(20)") 


(query-exec pgc
    "insert into the_devices values ("yannick","123","test device" )
 (query-exec pgc
    "insert into the_numbers values (1, 'the loneliest number')") 
 (query-exec pgc
    "insert into the_numbers values (2, 'company')")

(query-row pgc "select * from the_numbers where n = 0")
  






                        