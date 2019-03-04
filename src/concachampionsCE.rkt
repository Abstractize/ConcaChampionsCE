#lang racket

;;Constantes de Interfaz
(define height 600)
(define width 600)

;;main (CCCE2019 '(4 4 2) '(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
    (Game (First_Gen '() (cons 1 Team1)) (First_Gen '() (cons 1 Team2)) (ball (/ height 2) (/ width 2)) Generations)
)
;;Función de juego
(define (Game Team1 Team2 fball Generations)
    (cond
        ((zero? Generations)
         2
            (print "Exit")
        )
        (else
            ;;Grafica los 2 equipos y la bola
            (Game (Fitness Team1 '() '() fball) (Fitness Team2 '() '() fball) (update fball Team1 Team2) (- Generations 1))
        )
    )
)
;;Función que crea los jugadores.
;;Role = 0 Portero
;;Role = 1 Defensa
;;Role = 2 Medios
;;Role = 3 Delantero
(define (player speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability role)
    (list speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability role)
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
                            (First_Gen (cons (player (rand) (rand) 0 0 0 (randPosY) (rand) 0) players) (minus1 team))
                        )
                        ;;Defensa
                        ((equal? (lenght team) 3)                        
                            (First_Gen (cons (player (rand) (rand) 0 0 (/ width 3) (randPosY) (rand) 1) players) (minus1 team))
                        )
                        ;;Medio
                        ((equal? (lenght team) 2)
                            (First_Gen (cons (player (rand) (rand) 0 0 (/ width 2) (randPosY) (rand) 2) players) (minus1 team))
                        )
                        ;;Delantero
                        ((equal? (lenght team) 1)
                            (First_Gen (cons (player (rand) (rand) 0 0 (* 2 (/ width 3)) (randPosY) (rand) 3) players) (minus1 team))
                        )
                    )
                )
            )            
        )        
    )
)

;;Method that mutate players, giving them more favorable characteristics.
;;+0 o +-1 a las caracteristicas basicas
(define (mutate players newPlayers)
    (cond
        ((null? players)
            newPlayers
        )
        ((null? newPlayers)
            (mutate (cdr players) (list (car players)) )
        )
        (else
            (mutate 
                (cdr players) 
                (append newPlayers 
                    (list(player 
                        (+ (random 0 2) (getComposition (car players) 0)) 
                        (+ (random 0 2) (getComposition (car players) 1)) 
                        (getComposition (car players) 2) 
                        (getComposition (car players) 3)
                        (+ (random 0 (getComposition (car players) 0)) (getComposition (car players) 2))
                        (+ (random 0 (getComposition (car players) 0)) (getComposition (car players) 3))
                        (+ (random 0 2) (getComposition (car players) 6))
                        (getComposition (car players) 7)
                    ))
                )
            )    
        )
    )
)

;;Method that calculates the fitness of a single player
;;suma de todas las caracteristicas basicas, si la bola esta en la zona que les
;;corresponde, se considera |Xjugador-Xbola| |Yjugador-Ybola|, de lo contrario
;;solo |Yjugador-Ybola|
;;min siystem span: solo considerar Y?
(define(calcFitness player ball)
    (- (+ (getComposition player 0) (getComposition player 1) (getComposition player 6)) (abs(- (getComposition player 5) (cadr ball))))
)

;;Función de Aptitud
;;•	Porteros:
;;-Goles tapados.
;;-Alineación con el eje Y de la bola.
;;solo un portero, por lo tanto se clona y se muta

;;•	Defensas:
;;-Alineación con la trayectoria de los delanteros contrarios.
;;-Otra cosa que se le ocurra.
;;se busca el mejor defensa, se clona y se muta
  
;;•	Medios:
;;-Cercanía a la bola.
;;- Alineación con la trayectoria de los delanteros contrarios.
;;se busca el mejor medio, se clona y se muta
  
;;•	Delanteros:
;;-Cercanía a la bola
;;-Goles Anotados
;;se busca el mejor delantero, se clona y se muta.

;;Retorna el nuevo equipo.
;;
(define (Fitness team newTeam subteam bola)
    (cond
        ((null? team)
            (mutate newTeam '())
        )
        ((equal? (getComposition (car team) 7) 0)
            
            (Fitness (cdr team) (list(car team)) '() bola )
        )
        ((equal? (getComposition (car team) 7) 1)
            (cond
                ((equal? (getComposition (cadr team) 7) 1)
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola )
                )
                (else
                    (Fitness (cdr team) (append newTeam (defenseFitness (cons (car team) subteam) '() bola)) '() bola)
                )
            )
        )
        ((equal? (getComposition (car team) 7) 2)
            (cond
                ((equal? (getComposition (cadr team) 7) 2)
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola )
                )
                (else
                    (Fitness (cdr team) (append newTeam (midFitness (cons (car team) subteam) '() bola)) '() bola)
                )
            )
        )
        ((equal? (getComposition (car team) 7) 3)
            (cond
                ((null? (cdr team))
                    (Fitness (cdr team) (append newTeam (forwFitness (cons (car team) subteam) '() bola)) '() bola)
                )
                ((equal? (getComposition (cadr team) 7) 3)
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola )
                )
                (else
                    (Fitness (cdr team) (append newTeam (forwFitness (cons (car team) subteam) '() bola)) '() bola)
                )
            )
        )
    )
)

(define(defenseFitness defenses bestDefenses ball)
    (cond
        ((null? defenses)
            bestDefenses
        )
        ((null? bestDefenses)
            (defenseFitness (cdr defenses) (list (car defenses)) ball)
        )
        ((< (calcFitness (car defenses) ball) (calcFitness (car bestDefenses) ball) )
            (defenseFitness (cdr defenses) (cons (car bestDefenses) bestDefenses) ball)
        )
        ((> (calcFitness (car defenses) ball) (calcFitness (car bestDefenses) ball) )
            (defenseFitness (append (cdr defenses) bestDefenses ) (list (car defenses)) ball)
        )
    )
)

(define(midFitness mids bestMids ball)
    (cond
        ((null? mids)
            bestMids
        )
        ((null? bestMids)
            (midFitness (cdr mids) (list (car mids)) ball)
        )
        ((< (calcFitness (car mids) ball) (calcFitness (car bestMids) ball) )
            (midFitness (cdr mids) (cons (car bestMids) bestMids) ball)
        )
        ((> (calcFitness (car mids) ball) (calcFitness (car bestMids) ball) )
            (midFitness (append (cdr mids) bestMids ) (list (car mids)) ball)
        )
    )
)

(define(forwFitness forwards bestForwards ball)
    (cond
        ((null? forwards)
        bestForwards
        )
        ((null? bestForwards)
            (forwFitness (cdr forwards) (list (car forwards)) ball)
        )
        ((< (calcFitness (car forwards) ball) (calcFitness (car bestForwards) ball) )
            (forwFitness (cdr forwards) (cons (car bestForwards) bestForwards) ball)
        )
        ((> (calcFitness (car forwards) ball) (calcFitness (car bestForwards) ball) )
            (forwFitness (append (cdr forwards) bestForwards ) (list (car forwards)) ball)
        )
    )
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
        ((> (Compare_force fball Team1) 0)
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
                        (equal? (getComposition (car Team) 4) (car fball));Pos X
                        (equal? (getComposition (car Team) 5) (car (cdr fball)));Pos Y
                    )
                    (getComposition (car Team) 1);Force
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
;pos = 7 para Role
(define (getComposition player pos)
    (cond
        ((null? player)
            -1
        )
        ((zero? pos)
            (car player)
        )
        (else
            (getComposition (cdr player)(- pos 1))
        )    
    )
)
;;Random de Posición
(define (randPosY)
    (random 0 height)
)
;;Función de random de 1 a 10
(define (rand)
    (random 1 11)
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
(CCCE2019 '(4 4 2) '(5 3 2) 3)