#lang racket

;;Constantes de Interfaz
(define height 600)
(define width 600)
;;main (CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
    (Game (First_Gen '() (cons 1 Team1)) (First_Gen '() (cons 1 Team2)) (ball (/ height 2) (/ width 2)) Generations)
)
;;Función de juego
(define (Game Team1 Team2 fball Generations)
    (cond
        ((zero? Generations)
            (print "Exit")
        )
        (else
            ;;Grafica los 2 equipos y la bola
            (Game (Fitness Team1 fball) (Fitness Team2 fball) (update fball Team1 Team2) (- Generations 1))
        )
    )
)
;;Función que crea los jugadores.
(define (player speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability)
    (list speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability)
)
;;Función recursiva que crea un lista con los 2 equipos.
(define (First_Gen players team)
    (cond
        ;;Lista vacía
        ((null? team)
            (reverse players)
            ;;Función para separar los jugadores(players team)
        )
        (else
            (cond
                ;;0 al inicio de la lista?
                ((zero? (car team))
                    (First_Gen players (cdr team))
                )
                (else
                    (cond
                        ;;Portero
                        ((equal? (lenght team) 4)
                            (First_Gen (cons (player (rand) (rand) 0 0 0 (randPosY) (rand)) players) (minus1 team))
                        )
                        ;;Defensa
                        ((equal? (lenght team) 3)                        
                            (First_Gen (cons (player (rand) (rand) 0 0 (/ width 3) (randPosY) (rand)) players) (minus1 team))
                        )
                        ;;Medio
                        ((equal? (lenght team) 2)
                            (First_Gen (cons (player (rand) (rand) 0 0 (/ width 2) (randPosY) (rand)) players) (minus1 team))
                        )
                        ;;Delantero
                        ((equal? (lenght team) 1)
                            (First_Gen (cons (player (rand) (rand) 0 0 (* 2 (/ width 3)) (randPosY) (rand)) players) (minus1 team))
                        )
                    )
                )
            )            
        )        
    )
)
;;Función de Aptitud
(define (Fitness Team1 Team2 bola)
;;Mueve los jugadores a una nueva posición y compara las estadísticas.
;;•	Porteros:
;;-Goles tapados.
;;-Alineación con el eje Y de la bola.
;;•	Defensas:
;;-Alineación con la trayectoria de los delanteros contrarios.
;;-Otra cosa que se le ocurra.
;;•	Medios:
;;-Cercanía a la bola.
;;- Alineación con la trayectoria de los delanteros contrarios.
;;•	Delanteros:
;;-Cercanía a la bola
;;-Goles Anotados
(print "Soy una función de Fitness")
;;Retorna el nuevo equipo.
;;
)
;;Función que crea la bola
(define (ball pos_X pos_Y)
    (list pos_X pos_Y)
)
;;Funcion que actualiza la bola
(define (update fball Team1 Team2)
    (cond
        ;;Comprueba si el equipo 1 y el 2 le pegaron
        ((and (zero? (Compare_force fball Team1))(zero? (Compare_force fball Team2)))
            fball
        )     
        ;;Comprueba si el equipo 1 le pegó.
        ((> (Compare_force fball Team1))
            (ball (+ (car fball) (Compare_force fball Team1)) (cdar fball))
        )
        ;;Comprueba si el equipo 2 le pegó.
        (else
            (ball (- (car fball) (Compare_force fball Team1)) (cdar fball))
        ) 
    )
)
;;Función que compara si el equipo le pegó. Retorna 0 si no le pegaron, en caso de que le pegaran, retorna la fuerza del que le pegó.
(define (Compare_force fball Team)
    (cond
        ((null? Team)
            0
        )
        (else
            (cond
                (
                    (and
                        (equal? (getComposition (car(Team)) 4) (car fball));Pos X
                        (equal? (getComposition (car(Team)) 5) (cdar fball));Pos Y
                    )
                    (getComposition (car(Team)) 1);Force
                )
                (else
                    (Compare_force fball (cdr Team))
                )
            )
        )
    )
)
;;Atributo de los jugadores
;pos = 0 para Speed.
;pos = 1 para Strength
;pos = 2 para X inicial
;pos = 3 para Y inicial
;pos = 4 para X final
;pos = 5 para Y final
;pos = 6 para Ability
(define (getComposition player pos)
    (cond
        ((zero? pos)
            (car player)
        )
        (else
            (cdr player)
        )    
    )
)
;;Función que agrupa posiciones de acuerdo a un esquema dado
(define (cluster lst group brackets)
    ;;Debe agrupar de acuerdo a las posiciones
    (print "Soy una función que agrupa")
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
;;Reversa
(define (reverse lst)
    (cond 
        ((null? lst)
        lst
        )
        (else
            (append (reverse (cdr lst)) (list (car lst)))
        )
    )
)
;;Combinar
(define (append l1 l2)
    (cond 
        ((null? l1) 
            l2
        )
        (else
            (cons (car l1) (append (cdr l1) l2))
        )
    )
)

;;Juego
(CCCE2019 '(4 4 2) '(5 3 2) 20)