
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*               Speedcours for classes in Racket                  *-*-
;-*-*               Yannick Merckx || Arno de Witte                   *-*-
;-*-*         2013-2014 Russian Gaming/Programming Club               *-*-
;-*-*           Students from Vrije Universiteit Brussel              *-*-
;-*-*                                                                 *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

;In this course everything will be explained based on examples

;General info:

; > every classname needs to be defined with at the end "%"

; > de advantage of using classes:   - Object Oriented-programming
;                                    - Easy programmable interaction between objects
;                                    - The use of inheritance
;                                    - Encapsulation
;                                    - ...


#lang racket   ;imports racket-library


;================
; 1 THE BASICS |
;================

; 1.1 defining a superclass
;---------------------------

;to define a superclass you define (class object% ...

(define generic-data%
  (class object% 
    
    (super-new)   ;constructor superclass.   !Notice! This contructor has no variables !
   
    (init name     ;defines inintial variables
          value)   
    
    
    ;!Notice! The initial variables get no initial values by the contruction of the superclass, this means you need to initialize
    ;         them with the subclass.
    
    
    (field [name~ name] [value~ value])     ;Public fields can be accessed and mutated by subclasses using inherit-field.
                                            ;Public fields are also accessible outside the class via class-field-accessor 
                                            ;and mutable via class-field-mutator                                    
  
    (define/public (get-name) name~)        ;"define/public" defines  that function can be called from outside 
                                            ;and will be inherited by his subclasses
                                            ;shorthand for (begin (public id) (define id ...))
                                           
                                            ;!NOTICE!  when you define an function, standard the methode is private.
      
    (define/public (get-value)
      value~)
    
    (define/public (set-value! val)
      (set! value~ val))
    )
  )


;-------------------------
; 1.2 defining a subclass
;-------------------------

;to define a subclass you define (class name-superclass% ...

; because we are going to construct a subclass from the superclass from 1.1
; this subclass inherits the alle the public defines functions + variables from his superclass.

; in this example these are:   - the functions: get-name, get-value and set-value!
;                              - the variables: name and value


; !NOTICE! In this example the variables name and value are not initialized with a value


(define temperature-data%
  
  (class generic-data%                     
   
    (init value-temp)                          ;initial value of subclass
    
    (define  celcius~ #t)                     
    
    (inherit/super set-value! get-value)      ; to use a function from the superclass in the subclass

    (super-new [name 'temp] [value value-temp]) ;initialize the superclass

    (define/public (to-celcius)
      (cond [(not celcius~)
             (begin 
               (set-value! (* (- (get-value) 32) (/ 5 9)))
               (set! celcius~ #t))]
            ))
    
    (define/public (to-fahrenheit)
      (cond  [celcius~
              (begin 
                (set-value! (* (+ (get-value) 32) (/ 9 5)))
                (set! celcius~ #f))]))
    
    (define/public (which-unit)
      (if celcius~
          'celcius
          'fahrenheit))
    )
  )

;---------------------
; 1.3 making an object
;---------------------

; 2 possibilities:

(make-object temperature-data% "test")

(new temperature-data% [value-temp "test"])


;---------------------------------
; 1.4 communication with an object
;---------------------------------

(define testobject (make-object temperature-data% "test"))

(send testobject which-unit)   ;ask function defined by the subclass

(send testobject get-name)     ;ask function from subclass that is defined in superclass


;---------------------------------
; 1.5 communication with an object
;---------------------------------

;  > you can always initialize your objects directly by:

;     (init [var1 val2]
;           [var2 val2]
;            ...)



