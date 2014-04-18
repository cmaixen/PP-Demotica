#lang racket
(require db)

(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite")

(define db (sqlite3-connect  #:database db-pad
                              #:mode 'read/write))
(define Bedroom "Bedroom")

(define bathroom "Bathroom")

;(define livingroom "Livingroom")

 (query-exec db
  "create table the_devices (name varchar(20),serialnumber varchar(20),communicationadress varchar(20), type varchar(20))")
 
; (query-exec db
;    "insert into user_system values ( 'admin' , 'admin', '27/11/2012' , 1)"
;    )
 
; (query-exec db
;    "insert into user_system values ('vub','vub', '27/11/2012', 2)" )
    
