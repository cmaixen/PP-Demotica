#lang racket

(provide (all-defined-out))


;Source files
;------------

(define light-graph-file "Sources/graph-light" )
(define temp-graph-file "Sources/graph-temp" )
(define logfile "Sources/Logfile.txt")

;Constants ADT Major Domo
;------------------------


;positie van de kolommen in steward_table 
(define steward_table_locationcolummn 0)
(define steward_table_steward_database_column 1)
(define steward_table_ip_column 2)
(define steward_table_port_column 3)


;locatie van de database
(define db-pad "Sources/pp_db.sqlite")

;Sleep interval van Lichtswitch grafiek
(define light-graph-sleep 2)
(define temp-graph-sleep 3)


;representatie online/offline
(define online 1)
(define offline 0)

;verkrijgen van toesteltype
(define productID_offset  5)
(define productID_end 11)

;Permissionlevels
(define admin_level 1)


;Constants GUI
;-------------

;standaardwaarden knop
(define standaardmargin_button 20)


;standaardwaarden dialoog
(define min_width_dialog 400)
(define min_height_dialog 40)

;standaarwaarden paneel
(define standard_spacing 10)

;waarden mainframe
(define mainframe_width 800)
(define mainframe_height 600)


;waarden lightslider
(define min_value_ligthslider 50)
(define max_value_ligthslider 1000)

;waarden tempslider
(define min_value_tempslider 15)
(define max_value_tempslider 30)

;waarden die wordt gegeven aan het device als het afstaat
(define device_off_mesurement 0)


;waarden button-panel
(define min_width_button_panel 100 )
(define min_height_button_panel 20)

;Constants ADT Steward
;---------------------

(define offlinecounterlimit 10)



