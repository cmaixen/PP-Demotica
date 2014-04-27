#lang racket
(require db)

(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite")

(define db (sqlite3-connect  #:database db-pad
                              #:mode 'read/write))
(define Bedroom "Bedroom")

(define Bathroom "Bathroom")

(define Livingroom "Livingroom")

; (query-exec db "create table the_stewards (location varchar(20),stewardDB varchar(20))")
 
;(query-exec db "insert into the_stewards values ($1 , $2)" Bedroom 'Bedroom_devices')

;(query-exec db "insert into the_stewards values ($1 , $2)" Bathroom 'Bathroom_devices')

;(query-exec db "insert into the_stewards values ($1 , $2)" Livingroom 'Livingroom_devices')
 
; (query-exec db
;"insert into user_system values ('vub','vub', '27/11/2012', 2)" )
    

(query-exec db "create table Bedroom_devices (name varchar(20),serialnumber varchar(20),comadresse varchar(20),type varchar(20), status varchar(20), mesurement varchar(20))")
(query-exec db"create table Bathroom_devices (name varchar(20),serialnumber varchar(20),comadresse varchar(20),type varchar(20), status varchar(20), mesurement varchar(20))")
(query-exec db"create table Livingroom_devices (name varchar(20),serialnumber varchar(20),comadresse varchar(20),type varchar(20), status varchar(20), mesurement varchar(20))")
 

 
; (query-exec db
;    "insert into user_system values ('vub','vub', '27/11/2012', 2)" )
    
;(name varchar(20),serialnumber varchar(20),com-adresse varchar(20),type varchar(20))