#lang racket
(require (lib "graphics.ss" "graphics"))
(require 2htdp/universe)
(open-graphics)

;;Window constants
(define height 500)
(define width 1000)

(define ScoreTeam1 0)
(define ScoreTeam2 0)

;;Main Window
(define ventanaPrincipal (open-viewport "Cancha" 1200 500))

;;hidden windows that define the limits of players
(define CampoJugadores (open-pixmap  "PorteroG1"  1200 500))
(define (interfazCancha)
((draw-solid-rectangle CampoJugadores) (make-posn 1000 0) 200 500 "darkgray")
;;Authors
((draw-string CampoJugadores) (make-posn 1100 20) "Tarea #1 Lenguajes")
((draw-string CampoJugadores) (make-posn 1010 30) "Estudiantes:")
((draw-string CampoJugadores) (make-posn 1030 50) "Bertha Brenes (2017101642)")
((draw-string CampoJugadores) (make-posn 1030 70) "Gabriel Abarca (2017110442)")
((draw-string CampoJugadores) (make-posn 1030 90) "Maria Avila (2014089607)")
;;Score
((draw-string CampoJugadores) (make-posn 1010 120) "Marcador:")
;Limiters
((clear-solid-rectangle CampoJugadores) (make-posn 1000 0) 4 500)
((clear-solid-rectangle CampoJugadores) (make-posn 1000 100) 200 4)
((draw-pixmap CampoJugadores) "cancha.bmp" (make-posn 0 0))
(void)
)
((draw-pixmap CampoJugadores) "bola.png" (make-posn 500 220))
(interfazCancha)




;;main (CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
(sleep 3)
    (Game (First_Gen '() (cons 1 Team1) 0) (First_Gen '() (cons 1 Team2) 1) (ball 495 245) Generations)
)
;;Main game function
(define (Game Team1 Team2 fball Generations)
    (cond
        ((zero? Generations)
            (interfazCancha)
            (transformar Team1 Team2 fball)
            (print "Exit")
        )
        (else
            (cond
                ((goal fball)
                 ;;Cambiar el Fitness para las posiciones.
                    ((interfazCancha)
                    (sleep 1)
                    ((transformar Team1 Team2 (ball 495 245)) (Game (Fitness Team1 '() '() fball 1) (Fitness Team2 '() '() fball 2) (ball 495 245) (- Generations 1)))            
                    )       
                )
                (else
                    (interfazCancha)
                    (sleep 1)
                    ((transformar Team1 Team2 fball) (Game (Fitness Team1 '() '() fball 1) (Fitness Team2 '() '() fball 2) (update fball Team1 Team2) (- Generations 1)))
                )   
            )            
        )
    )
)
;;Mehtod that creates players
;;Role = 0 Goal Keeper
;;Role = 1 Defense
;;Role = 2 Midfields
;;Role = 3 Forwards
(define (player speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability role)
    (list speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability role)
)
;;Method that scores goals
;;Return 1 for goal, 0 for no goal
(define (goal fball)
    (cond
        ((<= (car fball) 0)
            (+ ScoreTeam2 1)
            #t
        ) 
        ((>= (car fball) 990)
            (+ ScoreTeam1 1)
            #t
        )
        (else
            #f
        ) 
    )    
)

;;Method that creates the initial instance of the teams
(define (First_Gen players team num)
    (cond
        ;;Empty team
        ((null? team)
            (reverse players)
        )
        (else
            (cond
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

;;Method that assigns the next player
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

;;Method that create goalkeeper according to team
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

;;Method that create defenses according to team
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

;;Method that create midfields according to team
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

;;Method that create forwards according to team
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
;;+0 o +-1 to basic characteristics (Speed, strength, and ability)
(define (mutate players newPlayers ball nTeam)
    (cond
        ((null? players)
            newPlayers
        )
        ((null? newPlayers)
            (mutate (cdr players) (mutateGoalie (car players) ball) ball nTeam)
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
                        (newXf (car players) ball (getSpeed (car players)) (getXf (car players)) nTeam)
                        (newYf (car players) ball (getSpeed (car players)) (getYf (car players)))
                        (mutateAbility (car players) (random -2 3))
                        (getComposition (car players) 7)
                    ))
                )
                ball nTeam
            )    
        )
    )
)

;;Specific method that mutates the goalkeeper
(define (mutateGoalie gk ball)
    (list(player 
        (mutateSpeed gk (random -2 3)) 
        (mutateStrength gk (random -2 3)) 
        (getXf gk) 
        (getYf gk) 
        (getXf gk)
        (newYf gk ball (getSpeed gk) (getYf gk))
        (mutateAbility gk (random -2 3))
        (getComposition gk 7)
    ))
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
(define (newXf member ball iterations result nTeam)
    (cond
        ((zero? iterations)
            (cond
                ((equal? nTeam 1)
                    (cond
                        ((isDefense member)
                            (applyZone1Limit result)
                        )
                        ((isMid member)
                            (applyZone2Limit result)
                        )
                        ((isForward member)
                            (applyZone3Limit result)
                        )
                    )
                )
                (else
                    (cond
                        ((isDefense member)
                            (applyZone3Limit result)
                        )
                        ((isMid member)
                            (applyZone2Limit result)
                        )
                        ((isForward member)
                            (applyZone1Limit result)
                        )
                    )
                )
            )
        )
        ((< (car ball) (getXf member))
            (newXf member ball (- iterations 1) (- result 1) nTeam)
        )
        ((> (car ball) (getXf member))
            (newXf member ball (- iterations 1) (+ result 1) nTeam)
        )
        ((equal? (car ball) (getXf member))
            (cond
                ((equal? nTeam 1)
                    (cond
                        ((isDefense member)
                            (applyZone1Limit result)
                        )
                        ((isMid member)
                            (applyZone2Limit result)
                        )
                        ((isForward member)
                            (applyZone3Limit result)
                        )
                    )
                )
                (else
                    (cond
                        ((isDefense member)
                            (applyZone3Limit result)
                        )
                        ((isMid member)
                            (applyZone2Limit result)
                        )
                        ((isForward member)
                            (applyZone1Limit result)
                        )
                    )
                )
            )
        )
    )
)

;;Method that determines the first third of the field
(define (applyZone1Limit number)
    (cond
        ((<= number 50)
            50
        )
        ((>= number 350)
            350
        )
        (else number)
    )
)

;;Method that determines the second third of the field
(define (applyZone2Limit number)
    (cond
        ((<= number 350)
            350
        )
        ((>= number 650)
            650
        )
        (else number)
    )
)

;;Method that determines the last third of the field
(define (applyZone3Limit number)
    (cond
        ((<= number 650)
            650
        )
        ((>= number 950)
            950
        )
        (else number)
    )
)

;;Method that decides the new Y position of the player
(define (newYf member ball iterations result)
    (cond
        ((zero? iterations)
            result
        )
        ((< (car (cdr ball)) (getYf member))
            (newYf member ball (- iterations 1) (- result 5))
        )
        ((> (car (cdr ball)) (getYf member))
            (newYf member ball (- iterations 1) (+ result 5))
        )
        ((equal? (car (cdr ball)) (getYf member))
            result
        )
    )
)

;;Method that calculates the fitness of a single player
(define(calcFitness player ball)
    (- (+ (getComposition player 0) (getComposition player 1) (getComposition player 6)) (abs(- (getComposition player 5) (car(cdr ball)))))
)

;;Fitness function
;;Output: team composition with best player cloned
(define (Fitness team newTeam subteam bola nTeam)
    (cond
        ((null? team)
            (mutate newTeam '() bola nTeam)
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

;;Specific function that determines the fitness of defenses
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

;;Specific function that determines the fitness of midfields
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

;;Specific function that determines the fitness of forwards
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

;;Method that creates the ball
(define (ball pos_X pos_Y)
    (list pos_X pos_Y)
)
;;Method that updates the ball position
(define (update fball Team1 Team2)
    (cond
        ;;Checks if any of the teams hit the ball
        ((and (zero? (car (Compare_force fball Team1)))(zero? (car (Compare_force fball Team2))))
            fball
        )     
        ;;Compares the strength of the player(s) that hit the ball
        ((> (car (Compare_force fball Team1)) (car (Compare_force fball Team2)))
            (ball (moveBallX (Compare_force fball Team1) 1 (car fball))  (moveBallY (Compare_force fball Team1) 1 (car (cdr fball))))
        )
        ((< (car (Compare_force fball Team1)) (car (Compare_force fball Team2)))
            (ball (moveBallX (Compare_force fball Team2) 2 (car fball))  (moveBallY (Compare_force fball Team2) 2 (car (cdr fball))))
        )
        ((equal? (car (Compare_force fball Team1)) (car (Compare_force fball Team2)))
            fball
        )
        (else
            fball
        ) 
    )
)

;;Method that returns a list with the strength and ability of the player that hit the ball
;;returns zero if no one hit it
(define (Compare_force fball Team)
    (cond
        ((null? Team)
            '(0 0)
        )
        (else
            (cond
                (
                    (and
                        (isInXRange fball (car Team));Pos X
                        (isInYRange fball (car Team));Pos Y
                    )
                    (list (* 3 (getStrength (car Team))) (* 3 (getAbility (car Team))));Force
                )
                (else
                    (Compare_force fball (cdr Team))
                )
            )
        )
    )
)

;;Method that checks if a player is in X range of the ball
(define (isInXRange ball member)
    (cond
        ((and (<= (getXf member) (+ 2 (car ball))) (>= (getXf member) (- 2 (car ball))) ) #t)
        (else #f)
    )
)

;;Method that checks if a player is in Y range of the ball
(define (isInYRange ball member)
    (cond
        ((and (<= (getYf member) (+ 2 (car (cdr ball)))) (>= (getYf member) (- 2 (car (cdr ball)))) ) #t)
        (else #f)
    )
)

;;Method that moves the ball in the x axis
(define (moveBallX powerAability nTeam result)
    (cond
        ((equal? nTeam 1)
            (+ result (* (car powerAability) (cos (* 4.5 (car (cdr powerAability))))))
        )
        ((equal? nTeam 2)
            (- result (* (car powerAability) (cos (* 4.5 (car (cdr powerAability))))))
        )
    )
)

;;Method that checks if the ball is in the Y boundaries of the field
(define (checkBoundaries yVal)
    (cond
        ((> yVal 500)
            (- 500 (- yVal 500))
        )
        ((< yVal 0)
            (abs yVal)
        )
        (else
            yVal
        )
    )
)

;;Method that moves the ball in the y axis
(define (moveBallY powerAability nTeam result)
    (checkBoundaries (+ result (* (* (car powerAability) (cos (* 4.5 (car (cdr powerAability))))) (random -1 2)) ))
)

;;Method that gets the attributes of the players
;pos = 0 Speed.
;pos = 1 Strength
;pos = 2 Xi
;pos = 3 Yi
;pos = 4 Xf
;pos = 5 Yf
;pos = 6 Ability
;pos = 7 Role
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

;;Random Y position
(define (randPosY)
    (random 0 (- height 40))
)
;;Random 1-10 pos for player attributes
(define (rand)
    (random 1 11)
)
;;Method that subtracts 1 from the first
;;element of the list
(define (minus1 lista)
    (cons (- (car lista) 1) (cdr lista))
)
;;List functions
;;length
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
;;Reverse
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
;;Append 2 lists
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grafic design ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;player creation
;posx => position in x
;posy => position in y
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
  )
;;Ball
(define (teclado posx posy press)

;;right limit
 (if (> posx 980)
    (begin
       (bolaG 980 posy 'd)
      (teclado 980 posy (key-value(get-key-press ventanaPrincipal))))
 ;;left limit
  (if (< posx 10)
      (begin
        (bolaG 20 posy 'r)
        (teclado 20 posy (key-value(get-key-press ventanaPrincipal)) ))
;;upper limit
  (if (< posy 0)
      (begin
        (bolaG posx 0 'u)
        (teclado posx 0 (key-value(get-key-press ventanaPrincipal))))
  ;;lower limit
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
  
(define (bolaG posx posy)
      ((draw-pixmap CampoJugadores) "bola.png" (make-posn posx posy))
     
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
         (bolaG (car bola) (car (cdr bola)))
         void)
        (else
      (begin      
        ;(teclado (car bola) (car(cdr bola)) 'up)
        (jugadoresG1 (getComposition (car AliG1) 4) (getComposition (car AliG1) 5) 'r)
        (jugadoresG2 (getComposition (car AliG2) 4) (getComposition (car AliG2) 5)  'r)
        
        (transformar (cdr AliG1) (cdr AliG2) bola))
        
      )
     )
  )
;;Juego
(CCCE2019 '(4 4 2) '(5 3 2) 100)