#lang racket
(require (lib "graphics.ss" "graphics"))
(require 2htdp/universe)
;;(require htdp/draw)
(open-graphics)

;;Constantes de Interfaz
(define height 500)
(define width 1000)

;ventana principal de la cancha
(define ventanaPrincipal (open-viewport "Cancha" 1200 500))

;ventana ocultas para definir los limites de jugadores
(define CampoJugadores (open-pixmap  "PorteroG1"  1200 500))

((draw-solid-rectangle CampoJugadores) (make-posn 1000 0) 200 500 "darkgray")
;Portada
((draw-string CampoJugadores) (make-posn 1100 20) "Tarea #1 Lenguajes")
((draw-string CampoJugadores) (make-posn 1010 30) "Estudiantes:")
((draw-string CampoJugadores) (make-posn 1030 50) "Bertha Brenes (2017101642)")
((draw-string CampoJugadores) (make-posn 1030 70) "Gabriel Abarca (2017110442)")
((draw-string CampoJugadores) (make-posn 1030 90) "Maria Avila (2014089607)")
;Marcador
((draw-string CampoJugadores) (make-posn 1010 120) "Marcador:")
;Delimitadoress
((clear-solid-rectangle CampoJugadores) (make-posn 1000 0) 4 500)
((clear-solid-rectangle CampoJugadores) (make-posn 1000 100) 200 4)
((draw-pixmap CampoJugadores) "cancha.bmp" (make-posn 0 0))
((draw-pixmap CampoJugadores) "bola.png" (make-posn 500 220))
;;main (CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
(sleep 3)
    (Game (First_Gen '() (cons 1 Team1) 0) (First_Gen '() (cons 1 Team2) 1) (ball (/ height 2) (/ width 2)) Generations)
)
;;Función de juego
(define (Game Team1 Team2 fball Generations)
  (display "Generation:")
  (print Generations)
  (display "\n")
  (display "Team1\n")
  (print Team1)
  (display "Team2\n")
  (print Team2)
    (cond
        ((zero? Generations)
            ((clear-viewport CampoJugadores))
            ((draw-solid-rectangle CampoJugadores) (make-posn 1000 0) 200 500 "darkgray")
            ;Portada
            ((draw-string CampoJugadores) (make-posn 1100 20) "Tarea #1 Lenguajes")
            ((draw-string CampoJugadores) (make-posn 1010 30) "Estudiantes:")
            ((draw-string CampoJugadores) (make-posn 1030 50) "Bertha Brenes (2017101642)")
            ((draw-string CampoJugadores) (make-posn 1030 70) "Gabriel Abarca (2017110442)")
            ((draw-string CampoJugadores) (make-posn 1030 90) "Maria Avila (2014089607)")
            ;Marcador
            ((draw-string CampoJugadores) (make-posn 1010 120) "Marcador:")
            ;Delimitadoress
            ((clear-solid-rectangle CampoJugadores) (make-posn 1000 0) 4 500)
            ((clear-solid-rectangle CampoJugadores) (make-posn 1000 100) 200 4)
            ((draw-pixmap CampoJugadores) "cancha.bmp" (make-posn 0 0))
            (transformar Team1 Team2)
            (print "Exit")
        )
        (else
            ((clear-viewport CampoJugadores))
            ((draw-solid-rectangle CampoJugadores) (make-posn 1000 0) 200 500 "darkgray")
            ;Portada
            ((draw-string CampoJugadores) (make-posn 1100 20) "Tarea #1 Lenguajes")
            ((draw-string CampoJugadores) (make-posn 1010 30) "Estudiantes:")
            ((draw-string CampoJugadores) (make-posn 1030 50) "Bertha Brenes (2017101642)")
            ((draw-string CampoJugadores) (make-posn 1030 70) "Gabriel Abarca (2017110442)")
            ((draw-string CampoJugadores) (make-posn 1030 90) "Maria Avila (2014089607)")
            ;Marcador
            ((draw-string CampoJugadores) (make-posn 1010 120) "Marcador:")
            ;Delimitadoress
            ((clear-solid-rectangle CampoJugadores) (make-posn 1000 0) 4 500)
            ((clear-solid-rectangle CampoJugadores) (make-posn 1000 100) 200 4)
            ((draw-pixmap CampoJugadores) "cancha.bmp" (make-posn 0 0))
            ;;Grafica los 2 equipos y la bola
            (sleep 5)
            ((transformar Team1 Team2) (Game (Fitness Team1 '() '() fball) (Fitness Team2 '() '() fball) (update fball Team1 Team2) (- Generations 1)))            
            
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
(define (First_Gen players team num)
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
                    (First_Gen players (cdr team) num)
                )
                (else
                    
                        (cond
                            ;;Team 1 
                            ((zero? num)
                                (cond
                                    ;;Portero
                                    ((equal? (lenght team) 4)
                                        (First_Gen (cons (player (rand) (rand) 0 0 20 250 (rand) 0) players) (minus1 team) num)
                                    )
                                    ;;Defensa
                                    ((equal? (lenght team) 3)                        
                                        (First_Gen (cons (player (rand) (rand) 0 0 290 (randPosY) (rand) 1) players) (minus1 team) num)
                                    )
                                    ;;Medio
                                    ((equal? (lenght team) 2)
                                        (First_Gen (cons (player (rand) (rand) 0 0 498 (randPosY) (rand) 2) players) (minus1 team) num)
                                    )
                                    ;;Delantero
                                    ((equal? (lenght team) 1)
                                        (First_Gen (cons (player (rand) (rand) 0 0 680 (randPosY) (rand) 3) players) (minus1 team) num)
                                    )   
                                )
                            )
                            (else
                                (cond
                                    ;;Portero
                                    ((equal? (lenght team) 4)
                                        (First_Gen (cons (player (rand) (rand) 0 0 970 250 (rand) 0) players) (minus1 team) num)
                                    )
                                    ;;Defensa
                                    ((equal? (lenght team) 3)                        
                                        (First_Gen (cons (player (rand) (rand) 0 0 725 (randPosY) (rand) 1) players) (minus1 team) num)
                                    )
                                    ;;Medio
                                    ((equal? (lenght team) 2)
                                        (First_Gen (cons (player (rand) (rand) 0 0 450 (randPosY) (rand) 2) players) (minus1 team) num)
                                    )
                                    ;;Delantero
                                    ((equal? (lenght team) 1)
                                        (First_Gen (cons (player (rand) (rand) 0 0 250 (randPosY) (rand) 3) players) (minus1 team) num)
                                    )
                                )
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
                        (+ (random 0 3) (getComposition (car players) 0)) 
                        (+ (random 0 3) (getComposition (car players) 1)) 
                        (getComposition (car players) 4) 
                        (getComposition (car players) 5)
                        (+ (random 0 (+ 1 (getComposition (car players) 0))) (getComposition (car players) 4))
                        (+ (random 0 (+ 1 (getComposition (car players) 0))) (getComposition (car players) 5))
                        (+ (random 0 3) (getComposition (car players) 6))
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
    (- (+ (getComposition player 0) (getComposition player 1) (getComposition player 6)) (abs(- (getComposition player 5) (car(cdr ball)))))
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
                ((equal? (getComposition (car (cdr team)) 7) 1)
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola )
                )
                (else
                    (Fitness (cdr team) (append newTeam (defenseFitness (cons (car team) subteam) '() bola)) '() bola)
                )
            )
        )
        ((equal? (getComposition (car team) 7) 2)
            (cond
                ((equal? (getComposition (car (cdr team)) 7) 2)
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
                ((equal? (getComposition (car (cdr team)) 7) 3)
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola )
                )
                (else
                    (Fitness (cdr team) (append newTeam (forwFitness (cons (car team) subteam) '() bola)) '() bola)
                )
            )
        )
       (else -1)
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
        ((<= (calcFitness (car defenses) ball) (calcFitness (car bestDefenses) ball) )
            (defenseFitness (cdr defenses) (cons (car bestDefenses) bestDefenses) ball)
        )
        ((> (calcFitness (car defenses) ball) (calcFitness (car bestDefenses) ball) )
            (defenseFitness (append (cdr defenses) bestDefenses ) (list (car defenses)) ball)
        )
    )
)

(define (fusePlayers oldPlayer newPlayer)
  (print "Soy una funcion que conserva posiciones iniciales y finales")
)

(define(midFitness mids bestMids ball)
    (cond
        ((null? mids)
            bestMids
        )
        ((null? bestMids)
            (midFitness (cdr mids) (list (car mids)) ball)
        )
        ((<= (calcFitness (car mids) ball) (calcFitness (car bestMids) ball) )
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
        ((<= (calcFitness (car forwards) ball) (calcFitness (car bestForwards) ball) )
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
            (ball (+ (car fball) (Compare_force fball Team1)) (car (cdr fball)))
        )
        ((> (Compare_force fball Team2) 0)
            (ball (+ (car fball) (Compare_force fball Team2)) (car (cdr fball)))
        )
        ;;Comprueba si el equipo 2 le pegó.
        (else
            (print "Caso no admitido")
            fball
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
         (print "Error")
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
    (random 0 (- height 40))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Diseno Grafico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Creacion de jugadores
;posx => posicion en x
;posy => posicion en y
;lad => tecla que mueve el jugador, Preguntar al profe(aqui creo que va el algoritmo genetico)
(define (jugadoresG1 posx posy lad )
  (if (equal? lad 'u)
      ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
      ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
          ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
              ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
              (if (equal? lad 'r)
                  ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
                  ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
                  ;else
                  (void)
                  )
              )
      )
 )
  
 (copy-viewport CampoJugadores ventanaPrincipal)
  ;((clear-viewport CampoJugadores))
  )

  ;Jugadores G2
  (define (jugadoresG2 posx posy lad )
  (if (equal? lad 'u)
      ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
      ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
          ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
              ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
              (if (equal? lad 'r)
                  ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
                  ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
                  ;else
                  (void)
                  )
              )
      )
 )
  
 (copy-viewport CampoJugadores ventanaPrincipal)
  ;((clear-viewport CampoJugadores))
  )
  ;transformar
  (define (transformar AliG1 AliG2)
  (cond ((and (null? AliG1) (null? AliG2))
         void)
        (else
      (begin
        (jugadoresG1 (getComposition (car AliG1) 4) (getComposition (car AliG1) 5) 'r)
        (jugadoresG2 (getComposition (car AliG2) 4) (getComposition (car AliG2) 5)  'r)
        (transformar (cdr AliG1) (cdr AliG2)))
      )
     )
  )
;;Juego
(CCCE2019 '(4 4 2) '(5 3 2) 5)