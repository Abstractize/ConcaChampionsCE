#lang racket

;;Constantes de Interfaz
(define height 600)
(define width 600)
;;main
;;(CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
    (First_Gen '() (cons 1 Team1))
)
;;Función que crea los jugadores.
(define (player speed strenght pos_X pos_Y ability)
    (list speed strenght pos_X pos_Y ability)
)
;;Función recursiva que crea un lista con los 2 equipos.
(define (First_Gen players team)
    (cond
        ;;Lista vacía
        ((null? team)
            players
        )
        ;;0 al inicio de la lista?
        (else
            (cond
                ((zero? (car team))
                    (First_Gen players (cdr team))
                )
                (else
                    (cond
                        ;;Portero
                        ((equal? (lenght team) 4)
                            (First_Gen (cons (player (rand) (rand) 0 (randPosY) (rand)) players) (minus1 team))
                        )
                        ;;Defensa
                        ((equal? (lenght team) 3)                        
                            (First_Gen (cons (player (rand) (rand) (/ width 3) (randPosY) (rand)) players) (minus1 team))
                        )
                        ;;Medio
                        ((equal? (lenght team) 2)
                            (First_Gen (cons (player (rand) (rand) (/ width 2) (randPosY) (rand)) players) (minus1 team))
                        )
                        ;;Delantero
                        ((equal? (lenght team) 1)
                            (First_Gen (cons (player (rand) (rand) (* 2 (/ width 3)) (randPosY) (rand)) players) (minus1 team))
                        )
                    )
                )
            )            
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
;;Resta 1 al primero
(define (minus1 lista)
    (cons (- (car lista) 1) (cdr lista))
)
;;Funciones para listas
;;Largo
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
(CCCE2019 '(4 4 2) '(5 3 2) 20)