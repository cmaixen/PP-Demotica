#lang racket
(require db)

 
; (query-exec db
;"insert into user_system values ('vub','vub', '27/11/2012', 2)" )
    

;(query-exec db "create table Bedroom_devices (name varchar(20),serialnumber varchar(20),comadresse varchar(20),type varchar(20), status varchar(20), mesurement varchar(20))")
;(query-exec db"create table Bathroom_devices (name varchar(20),serialnumber varchar(20),comadresse varchar(20),type varchar(20), status varchar(20), mesurement varchar(20))")
;(query-exec db"create table Livingroom_devices (name varchar(20),serialnumber varchar(20),comadresse varchar(20),type varchar(20), status varchar(20), mesurement varchar(20))")
 

 
; (query-exec db
;    "insert into user_system values ('vub','vub', '27/11/2012', 2)" )
    
;(name varchar(20),serialnumber varchar(20),com-adresse varchar(20),type varchar(20))



(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/1ste Trimester/Programmeerproject 2/PP-Demotica/Sources/pp_db.sqlite")


(define db (sqlite3-connect  #:database db-pad
                              #:mode 'read/write))


;Values initialize database
(define Bedroom "Bedroom")
(define Bathroom "Bathroom")
(define Livingroom "Livingroom")

(define localhost "localhost")

(define Bedroom_devices "Bedroom_devices")
(define Bathroom_devices "Bathroom_devices")
(define Livingroom_devices "Livingroom_devices")

;create database
(query-exec db "create table the_stewards (location varchar(20),stewardDB varchar(20), ip varchar(20), port integer)")

(query-exec db "insert into the_stewards values ($1, $2, $3, $4)" Bedroom Bedroom_devices localhost 6666)
(query-exec db "insert into the_stewards values ($1 , $2, $3, $4)" Bathroom Bathroom_devices localhost 6667)
(query-exec db "insert into the_stewards values ($1 , $2, $3, $4)" Livingroom Livingroom_devices localhost 6668)