;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Domotica Energie Monitoring Systeem                  *-*-
;-*-*                        Yannick Merckx                           *-*-
;-*-* Programmeerproject 2013-2014 2de Bachelor Computerwetenschappen *-*-
;-*-*           Student van Vrije Universiteit Brussel                *-*-
;-*-*                                                                 *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


; Beschrijving: de file bevat procedures die alle GUI_ADT's gebruiken
;      
; Procedures:

; - determine-choice:  argumenten: lijst (pair) selection (number)(het hoeveelste element van de lijst wil je hebben)
;                      output: element dat op plaats selection staat in de gegeven lijst
;                 

; - update-list-box: argumenten: list-box (list-box%)(listbox moet bestaan uit de kolommen: naam - type - status - serienummer - meting) , steward-object
;                    output: void (listbox geupdate volgens de informatie van de steward) 

; - set-data-for-listbox:  argumenten: listbox(listbox%)(listbox moet bestaan uit de kolommen: naam - type - status - serienummer - meting) , lijst met namen (pair), lijst met statussen (pair), lijst met locaties (pair) , lijst met serienummers(pair), lijst met metingen (pair)
;                          output: void (verbind aan iedere rij van de listbox een lijst met de elementen van de lijsten)

; - list-box-tab:  argumenten: naam van de listbox (string?), kolomlijst (lijst met de verschillende namen voor de kolommen) (listof string? ...), tabpanel (tab-panel%)
;                  output: list-box% (genereerd een list-box met de gegeven naam en kolomlijst en gebonden aan het gegeven tabpaneel)

; - list_devices_mesurement_spectype :  argumenten: list_w_stewards (listof steward-object ...),  type (string?) (duidt op het type van toestellen dat je wil)
;                                       Output: (listof string?) (lijst met de metingen van alle toestellen van het gegeven type)
;
; - list_devices_locations_spectype :  argumenten: argumenten: list_w_stewards (listof steward-object ...),  type (string?) (duidt op het type van toestellen dat je wil)
;                                       Output: (listof string?) (lijst met de locaties van alle toestellen van het gegeven type)

; - list_devices_serialnumbers_spectype :  argumenten: list_w_stewards (listof steward-object ...),  type (string?) (duidt op het type van toestellen dat je wil)
;                                          Output: (listof string?) (lijst met de serienummers van alle toestellen van het gegeven type)
;
; - list_devices_names_spectype :  argumenten:list_w_stewards (listof steward-object ...),  type (string?) (duidt op het type van toestellen dat je wil)
;                                   Output: (listof string?) (lijst met de namen van alle toestellen van het gegeven type)
;

; - list_devices_com_adress_spectype:   argumenten: list_w_stewards (listof steward-object ...),  type (string?) (duidt op het type van toestellen dat je wil)
;                                       Output: (listof string?) (lijst met de communicatieadressen van alle toestellen van het gegeven type)
;

; - list_devices_status_spectype:       argumenten: list_w_stewards (listof steward-object ...),  type (string?) (duidt op het type van toestellen dat je wil)
;                                       Output: (listof string?) (lijst met de statussen van alle toestellen van het gegeven type)
;

; Commentaar: geen


#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))

(provide (all-defined-out))
;beslissen welke keuze je moet teruggeven bij een keuzeobject

;algmene functie 
(define (determine-choice lst selection)
  (define (loop lst counter)
    (let ((choice (car lst)))
      (if (= counter selection)
          choice
          (loop (cdr lst) (+ counter 1)))))
  (if (< (util:list_size lst) selection)
      (error 'determine-choice-function "Selection is out of range of the list!")
      (loop lst 0)))



;listbox updater waarbij met de listbox helemaal update volgens de kolommen
;: name - type - status - serial - comadress

(define (update-list-box lst-box current-steward)
  (let ((namelst (util:send current-steward 'list_devices_names))
        (typelst (util:send current-steward 'list_devices_type))
        (statuslst (util:send current-steward 'list_devices_status))
        (seriallst (util:send current-steward 'list_devices_serial))
        (mesurementlst (util:send current-steward 'list_devices_mesurement_with_value)))
    (util:send current-steward 'get-location)
    (send lst-box set namelst typelst statuslst seriallst mesurementlst)))


;listbox updater waarbij alle toestel van 1 type zijn
;kolommen volgens: naam - status - locatie - serial - com-adress

(define (update-list-box-spectype lst-box type stewardlist)
     (let* ((current-steward (car stewardlist))
               (namelst (list_devices_names_spectype stewardlist type))
               (locationlst (list_devices_locations_spectype stewardlist type))
               (statuslst(list_devices_status_spectype stewardlist type))
               (seriallst (list_devices_serialnumbers_spectype stewardlist type))
               (mesurementlst (list_devices_mesurement_spectype stewardlist type)))    
          (send lst-box set namelst statuslst locationlst  seriallst mesurementlst)
      ( if (empty? namelst)
           'done
       (set-data-for-listbox lst-box namelst statuslst locationlst  seriallst mesurementlst))
  ))


(define (set-data-for-listbox listbox namelst statuslst locationlst  seriallst mesurementlst)
  (define (loop listbox namelst statuslst locationlst  seriallst mesurementlst counter)
        (let ((name (car namelst))
              (status (car statuslst))
              (location (car locationlst))
              (serial (car seriallst))
              (mesurement (car mesurementlst)))
          (send listbox set-data counter (list name status location serial mesurement))
             (if (empty? (cdr namelst))
        'ok
          (loop listbox (cdr namelst) (cdr statuslst) (cdr locationlst)  (cdr seriallst) (cdr mesurementlst) (+ counter 1)))))
  
 
      (loop listbox namelst statuslst locationlst  seriallst mesurementlst 0))

;genereren van list-boxen
(define (list-box-tab string column-list tabpanel) (new list-box%
                                                        (label string)
                                                        (parent tabpanel)
                                                        (choices '())
                                                        (style (list 'single
                                                                     'column-headers))
                                                        (columns column-list)))






;Opvragen van de namen van alle objecten van een bepaald type en geeft dit terug in een lijst

  (define (list_devices_names_spectype list_w_stewards type)
     (util:list_devices_spectype list_w_stewards type 'list_devices_names))
  
  
;opvragen van de serialnumbers van alle objecten van een bepaald type en geeft dit terug in een lijst 
  
    (define (list_devices_serialnumbers_spectype list_w_stewards type)
     (util:list_devices_spectype list_w_stewards type 'list_devices_serial))
    
;opvragen van de locations van alle objecten van een bepaald type en geeft dit terug in een lijst 
    
     (define (list_devices_locations_spectype list_w_stewards type)
     (util:list_devices_spectype list_w_stewards type 'list_devices_location))
     
;opvragen van de statussen van alle objecten van een bepaald type en geeft dit terug in een lijst 
     (define (list_devices_status_spectype list_w_stewards type)
       (util:list_devices_spectype list_w_stewards type 'list_devices_status))
     
;opvragen van de com-adressen van alle objecten van een bepaald type en geeft dit terug in een lijst 
     (define (list_devices_com_adress_spectype list_w_stewards type)
       (util:list_devices_spectype list_w_stewards type 'list_devices_com_adress))
     
;opvragen van de meetingen van alle objecten van een bepaald type en geeft dit terug in een lijst 
     (define (list_devices_mesurement_spectype list_w_stewards type)
       (util:list_devices_spectype list_w_stewards type 'list_devices_mesurement))
    
    
    