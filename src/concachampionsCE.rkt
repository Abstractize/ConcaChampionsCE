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
(define (interfazCancha)
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
(void)
)

(interfazCancha)
((draw-pixmap CampoJugadores) "bola.png" (make-posn 500 220))



;;main (CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
(sleep 3)
    (Game (First_Gen '() (cons 1 Team1) 0) (First_Gen '() (cons 1 Team2) 1) (ball 495 245) Generations)
)
;;Función de juego
(define (Game Team1 Team2 fball Generations)
  ;;(display "Generation:")
  ;;(print Generations)
  ;;(display "\n")
  ;;(display "Team1\n")
  ;;(print Team1)
  ;;(display "\n")
  ;;(display "Team2\n")
  ;;(print Team2)
  ;;(display "\n")
    (cond
        ((zero? Generations)
            (interfazCancha)
            (transformar Team1 Team2 fball)
            (print "Exit")
        )
        (else
            (interfazCancha)
            ;;Grafica los 2 equipos y la bola
            (sleep 2)
            ((transformar Team1 Team2 fball) (Game (Fitness Team1 '() '() fball) (Fitness Team2 '() '() fball) (update fball Team1 Team2) (- Generations 1)))            
            (bolaG (car fball) (car (cdr fball)) 5) 
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
;;Retorna 0 si no hubo gol, 1 si el equipo 1 metió gol y 2 si el equipo 2 metió gol
(define (goal fball)
    (cond
        ((and (and (< 480 (car (cdr fball))) (> 20 fball)) (<= (car fball) 0))
            2
        ) 
        ((and (and (< 480 (car (cdr fball))) (> 20 fball)) (>= (car fball) 1000))
            1
        )
        (else
            0
        ) 
    )    
)

;;Función recursiva que crea un lista con un equipo.
(define (First_Gen players team num)
    (cond
        ;;Equipo vacío
        ((null? team)
            (reverse players)
        )
        (else
            (cond
                ;;0 al inicio de la lista?
                ((zero? (car team))
                    (First_Gen players (cdr team) num)
                )
                (else
                    (First_Gen (cons (characterCreator (lenght team) num) players) (minus1 team) num)                
                )
            )
        )            
    )        
)
;;Funciones que asignan el siguiente jugador
(define (characterCreator teamLenght team)
    (cond 
        ((equal? teamLenght 4)
            (goalkeeper team)
        )
        ((equal? teamLenght 3)
            (defense team)
        )
        ((equal? teamLenght 2)
            (midfields team)
        )
        ((equal? teamLenght 1)
            (forwards team)
        )
    )
)
;;Funciones que crean los jugadores de acuerdo al equipo
(define (goalkeeper team)
    (cond 
        ((zero? team)
            (player (rand) (rand) 0 0 20 250 (rand) 0)
        )
        (else
            (player (rand) (rand) 0 0 970 250 (rand) 0)
        )
    )
)
(define (defense team)
    (cond 
        ((zero? team)
            (player (rand) (rand) 0 0 290 (randPosY) (rand) 1)
        )
        (else
            (player (rand) (rand) 0 0 725 (randPosY) (rand) 1)
        )
    )
)
(define (midfields team)
    (cond 
        ((zero? team)
            (player (rand) (rand) 0 0 498 (randPosY) (rand) 2)
        )
        (else
            (player (rand) (rand) 0 0 450 (randPosY) (rand) 2)
        )
    )
)
(define (forwards team)
    (cond 
        ((zero? team)
            (player (rand) (rand) 0 0 680 (randPosY) (rand) 3)
        )
        (else
            (player (rand) (rand) 0 0 250 (randPosY) (rand) 3)
        )
    )
)

;;Method that mutate players, giving them more favorable characteristics.
;;+0 o +-1 a las caracteristicas basicas
(define (mutate players newPlayers ball)
    (cond
        ((null? players)
            newPlayers
        )
        ((null? newPlayers)
            (mutate (cdr players) (list (car players)) ball)
        )
        (else
            (mutate 
                (cdr players) 
                (append newPlayers 
                    (list(player 
                        (mutateSpeed (car players) (random -2 3)) 
                        (mutateStrength (car players) (random -2 3)) 
                        (getXf (car players)) 
                        (getYf (car players)) 
                        (newXf (car players) ball (getSpeed (car players)) (getXf (car players)))
                        (newYf (car players) ball (getSpeed (car players)) (getYf (car players)))
                        (mutateAbility (car players) (random -2 3))
                        (getComposition (car players) 7)
                    ))
                )
                ball
            )    
        )
    )
)

;;Method that mutates the speed of a player
(define (mutateSpeed member value)
    (cond
        ((<= (+ value (getSpeed member)) 1)
            1
        )
        ((>= (+ value (getSpeed member)) 10)
            10
        )
        (else (+ value (getSpeed member)))
    )
)

;;Method that mutates the strength of a player
(define (mutateStrength member value)
    (cond
        ((<= (+ value (getStrength member)) 1)
            1
        )
        ((>= (+ value (getStrength member)) 10)
            10
        )
        (else (+ value (getStrength member)))
    )
)

;;method that mutates the ability of a player
(define (mutateAbility member value)
    (cond
        ((<= (+ value (getAbility member)) 1)
            1
        )
        ((>= (+ value (getAbility member)) 10)
            10
        )
        (else (+ value (getAbility member)))
    )
)

;;Method that decides the new X position of the player
(define (newXf member ball iterations result)
    (cond
        ((zero? iterations)
            result
        )
        ((< (car ball) (getXf member))
            (newXf member ball (- iterations 1) (- result 2))
        )
        ((> (car ball) (getXf member))
            (newXf member ball (- iterations 1) (+ result 2))
        )
        ((equal? (car ball) (getXf member))
            result
        )
    )
)

(define (newYf member ball iterations result)
    (cond
        ((zero? iterations)
            result
        )
        ((< (car (cdr ball)) (getYf member))
            (newYf member ball (- iterations 1) (- result 2))
        )
        ((> (car (cdr ball)) (getYf member))
            (newYf member ball (- iterations 1) (+ result 2))
        )
        ((equal? (car (cdr ball)) (getYf member))
            result
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
(define (Fitness team newTeam subteam bola nTeam)
    (cond
        ((null? team)
            (mutate newTeam '() bola)
        )
        ((isGoalie (car team))
            (Fitness (cdr team) (list(car team)) '() bola nTeam)
        )
        ((isDefense (car team))
            (cond
                ((isDefense (car (cdr team)))
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola nTeam)
                )
                (else
                    (Fitness (cdr team) (append newTeam (defenseFitness (cons (car team) subteam) '() bola)) '() bola nTeam)
                )
            )
        )
        ((isMid (car team))
            (cond
                ((isMid (car (cdr team)))
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola nTeam)
                )
                (else
                    (Fitness (cdr team) (append newTeam (midFitness (cons (car team) subteam) '() bola)) '() bola nTeam)
                )
            )
        )
        ((isForward (car team))
            (cond
                ((null? (cdr team))
                    (Fitness (cdr team) (append newTeam (forwFitness (cons (car team) subteam) '() bola)) '() bola nTeam)
                )
                ((isForward (car (cdr team)))
                    (Fitness (cdr team) newTeam (cons (car team) subteam) bola nTeam )
                )
                (else
                    (Fitness (cdr team) (append newTeam (forwFitness (cons (car team) subteam) '() bola)) '() bola nTeam)
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
            (defenseFitness (cdr defenses) (append bestDefenses (list (fusePlayers (car defenses) (car bestDefenses)))) ball)
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
        ((<= (calcFitness (car mids) ball) (calcFitness (car bestMids) ball) )
            (midFitness (cdr mids) (append bestMids (list(fusePlayers (car mids) (car bestMids)))) ball)
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
            (forwFitness (cdr forwards) (append bestForwards (list(fusePlayers (car forwards) (car bestForwards)))) ball)
        )
        ((> (calcFitness (car forwards) ball) (calcFitness (car bestForwards) ball) )
            (forwFitness (append (cdr forwards) bestForwards ) (list (car forwards)) ball)
        )
    )
)

;;Method that implants the best qualities of the best player
;;into another player, keeping positions
(define (fusePlayers oldPlayer newPlayer)
    (list 
        (getSpeed newPlayer) 
        (getStrength newPlayer) 
        (getXi oldPlayer) 
        (getYi oldPlayer) 
        (getXf oldPlayer) 
        (getYf oldPlayer) 
        (getAbility newPlayer) 
        (getRole newPlayer) 
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

;;Method that gets the speed of a player
(define (getSpeed member)
    (getComposition member 0)
)

;;Method that gets the strength of a player
(define (getStrength member)
    (getComposition member 1)
)

;;Method that gets the initial X of a player
(define (getXi member)
    (getComposition member 2)
)

;;Method that gets the initial Y of a player
(define (getYi member)
    (getComposition member 3)
)

;;Method that gets the final X of a player
(define (getXf member)
    (getComposition member 4)
)

;;Method that gets the final Y of a player
(define (getYf member)
    (getComposition member 5)
)

;;Method that gets the ability of a player
(define (getAbility member)
    (getComposition member 6)
)

;;Method that gets the role of a player
(define (getRole member)
    (getComposition member 7)
)

;;Method that checks if a player is a goalie
(define (isGoalie member)
    (cond
        ((equal? (getRole member) 0) #t)
        (else #f)
    )
)

;;Method that checks if a player is a defense
(define (isDefense member)
    (cond
        ((equal? (getRole member) 1) #t)
        (else #f)
    )
)

;;Method that checks if a player is a midfield
(define (isMid member)
    (cond
        ((equal? (getRole member) 2) #t)
        (else #f)
    )
)

;;Method that checks if a player is a forward
(define (isForward member)
    (cond
        ((equal? (getRole member) 3) #t)
        (else #f)
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
      ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ((draw-pixmap CampoJugadores) "defensa.png" (make-posn posx posy))
              (if (equal? lad 'r)
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
;Bola
(define (teclado posx posy press)

;limite derecha
 (if (> posx 980)
    (begin
       (bolaG 980 posy 'd)
      (teclado 980 posy (key-value(get-key-press ventanaPrincipal))))
 ;limite de la izquiera TODO un parametro
  (if (< posx 10)
      (begin
        (bolaG 20 posy 'r)
        (teclado 20 posy (key-value(get-key-press ventanaPrincipal)) ))
      ;limite arriba
  (if (< posy 0)
      (begin
        (bolaG posx 0 'u)
        (teclado posx 0 (key-value(get-key-press ventanaPrincipal))))
      ;limite de abajo
  (if (> posy 490)
      (begin
        (bolaG posx  490 'd)
        (teclado posx 490 (key-value(get-key-press ventanaPrincipal))))    
  (if (equal? press 'up)
      (begin
        (bolaG posx posy 'u)
        (teclado posx (- posy 10) (key-value (get-key-press ventanaPrincipal))))
      (if (equal? press 'down)
          (begin
            (bolaG posx posy 'd)
            (teclado  posx (+ posy 10) (key-value (get-key-press ventanaPrincipal))))
      (if (equal? press 'left)
          (begin
            (bolaG posx posy 'l)
            (teclado (- posx 10) posy (key-value (get-key-press ventanaPrincipal))))
      (if (equal? press 'right)
          (begin
            (bolaG posx posy 'r)
            (teclado  (+ posx 10) posy (key-value (get-key-press ventanaPrincipal)) ))
          ;else
          (teclado  posx posy (key-value (get-key-press ventanaPrincipal)))
          )
    )
      )
      )
  )
  ))
  )
  )
  
(define (bolaG posx posy lad)
;  (cond((zero? lad)
;        ((draw-pixmap CampoJugadores) "bola.png" (make-posn posx posy))
;        )
;       (else(define (teclado limR limI posx posy press generacion)
;          (bolaG (- posx 10) posy (- lad 1))))
(if (equal? lad 'u)
((draw-pixmap CampoJugadores) "bola.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ((draw-pixmap CampoJugadores) "bola.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ((draw-pixmap CampoJugadores) "bola.png" (make-posn posx posy))
              (if (equal? lad 'r)
                  ((draw-pixmap CampoJugadores) "bola.png" (make-posn posx posy))
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
      ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ((draw-pixmap CampoJugadores) "delantero.png" (make-posn posx posy))
              (if (equal? lad 'r)
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
  (define (transformar AliG1 AliG2 bola)
  (cond ((and (null? AliG1) (null? AliG2))
         
         void)
        (else
      (begin
        (display bola)
        (bolaG (car bola) (car (cdr bola)) 5)
        ;(teclado (car bola) (car(cdr bola)) 'up)
        (jugadoresG1 (getComposition (car AliG1) 4) (getComposition (car AliG1) 5) 'r)
        (jugadoresG2 (getComposition (car AliG2) 4) (getComposition (car AliG2) 5)  'r)
        (transformar (cdr AliG1) (cdr AliG2) bola))
      )
     )
  )
;;Juego
(CCCE2019 '(4 4 2) '(5 3 2) 20)