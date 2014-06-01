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


#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(provide (all-defined-out))


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
          
  


(define (split-string-in-new-string string split_char)
  (let ((converted-lst (string->list string)))
  (define (loop lst  templst)

    (if (empty? lst)
       (list->string templst)
    (let ((next_char (car lst)))
      (if (equal? next_char  split_char)
          (loop (cdr lst) templst)
         (loop (cdr lst) (append templst (list next_char)))))))
    (loop converted-lst '())))