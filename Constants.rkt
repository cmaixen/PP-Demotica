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


;locatie van de database
(define db-pad "Sources/pp_db.sqlite")

;Sleep interval van Lichtswitch grafiek
(define light-graph-sleep 2)
(define temp-graph-sleep 3)



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

;positie van de kolommen in de device_tables
(define device_table_name_column 0)
(define device_table_serial_column 1)
(define device_table_com_adress_column 2)
(define device_table_type_column 3)
(define device_table_status_column 4)
(define device_table_mesurement_column 5)


(define temperaturesensor_type "Temperaturesensor")
(define lightswitch_type "Lightswitch")

;standaardwaarde van status
(define status_default_value "on")

(define temperaturesensor_default_value 25)
(define lightswitch_default_value 50)



