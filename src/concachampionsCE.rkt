#lang racket

;;Constantes de Interfaz
(define height 600)
(define width 600)
;;main

;;Función que crea los jugadores.
(define (player speed strenght pos_X pos_Y ability)
    (list speed strenght pos_X pos_Y ability)
)
;;Función recursiva que crea un lista con los 2 equipos.
(define (First_Gen players)
    (cond
        ;;Largo es 11?
        ((equal? (lenght players) 11)
            players
        )
        (else
            (First_Gen (cons (player (rand) (rand) (/ width 3) (randPosY) (rand)) players))
        )
    )
)
;;Random de Posición
(define (randPosY)
    (random 0 height)
)
;;Función de random de 1 a 10
(define (rand)
    (random 1 10)
)
;;Funciones para listas
(define(lenght lista)
    (lenght_aux lista 0)
)
(define (lenght_aux lista num)
    (cond
        ((null? lista)
            num
        )
        (else
            (lenght_aux (cdr lista) (+ 1 num))
        )
    )
)

;;Juego
(CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)