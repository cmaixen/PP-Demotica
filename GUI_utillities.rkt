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



;hulpfunctie voor tab1
;splits de verkregen string in een lijst met strings
;Bv: (split-string-in-list "POW=on\nFREQ=49.8125Hz\nVRMS=227V\nIRMS=1988mA\nLOAD=443W\nWORK=0.046kWh\n"  #\newline)
(define (split-string-in-list string split_char)
  (let ((converted-lst (string->list string)))
    (print converted-lst)
  (define (loop lst  templst resultlst)
    
    (if (empty? lst)
        resultlst
    (let ((next_char (car lst)))
      (if (equal? next_char  split_char)
         (begin (loop (cdr lst) '() (append resultlst (list (list->string templst))))
                ; (display templst)
                )   
         (loop (cdr lst) (append templst (list next_char)) resultlst)))))
    (loop converted-lst '() '())))
          
  

