#lang racket
(require (lib "graphics.ss" "graphics"))
 (require racket/format)
(require 2htdp/universe)
;;(require htdp/draw)
(open-graphics)


;Interface Constants
(define height 500)
(define width 1000)

(define  ScoreTeam1 0)
(define ScoreTeam2 0)

;main window of the court
(define ventanaPrincipal (open-viewport "Cancha" 1200 500))

; hidden window to define the limits of players
(define PlayersField (open-pixmap  "PorteroG1"  1200 500))
;Funtion that define the graphics interface in the project
(define (FieldGUI)
  ((draw-solid-rectangle PlayersField) (make-posn 1000 0) 200 500 "darkgray")
  ((draw-string PlayersField) (make-posn 1100 20) "Tarea #1 Lenguajes")
  ((draw-string PlayersField) (make-posn 1010 30) "Estudiantes:")
  ((draw-string PlayersField) (make-posn 1030 50) "Bertha Brenes (2017101642)")
  ((draw-string PlayersField) (make-posn 1030 70) "Gabriel Abarca (2017110442)")
  ((draw-string PlayersField) (make-posn 1030 90) "Maria Avila (2014089607)")

  ((draw-string PlayersField) (make-posn 1010 120) "Marcador:")
  ((draw-string PlayersField) (make-posn 1010 140) "Equipo 1:")
  ((draw-string PlayersField) (make-posn 1060 140) (~r ScoreTeam1 ) )
  ((draw-string PlayersField) (make-posn 1010 160) "Equipo 2:")
  ((draw-string PlayersField) (make-posn 1060 160) (~r ScoreTeam2) )
  ((clear-solid-rectangle PlayersField) (make-posn 1000 0) 4 500)
  ((clear-solid-rectangle PlayersField) (make-posn 1000 100) 200 4)
  ((draw-pixmap PlayersField) "cancha.bmp" (make-posn 0 0))
  (void)
)
((draw-pixmap PlayersField) "bola.png" (make-posn 500 220))
(FieldGUI)




;;main (CCCE2019 ‘(4 4 2) ‘(5 3 2) 20)
(define (CCCE2019 Team1 Team2 Generations)
(sleep 3)
    (Game (First_Gen '() (cons 1 Team1) 0) (First_Gen '() (cons 1 Team2) 1) (ball 495 245) Generations)
)
;;Funtion of the game
(define (Game Team1 Team2 fball Generations)
  
    (cond
        ((zero? Generations)
            (FieldGUI)
            (Transform Team1 Team2 fball)
            (print "Exit")
        )
        (else
            (cond
                ((goal fball)
                    (cond ((equal? 3 (abs (- ScoreTeam1 ScoreTeam2)))
                        ((Transform Team1 Team2 (ball 495 245)) (Game (Fitness Team1 '() '() fball 1) (Fitness Team2 '() '() fball 2) (ball 495 245) 0))
                    )
                          (else
                           ;;Change the Fitness for the positions.
                           ;; Review Update movement
                           (+ ScoreTeam2 1)
                           (FieldGUI)
                           ;;Made the GUI and the ball
                           ;(sleep 2)
                           ((Transform Team1 Team2 (ball 495 245)) (Game (Fitness Team1 '() '() fball 1) (Fitness Team2 '() '() fball 2) (ball 495 245) (- Generations 1))) 
                           )
                    )
                
                            
                           
                )
                (else
                    (FieldGUI)
                    ;;Made the GUI with the two teams
                    ;(sleep 2)
                    ((Transform Team1 Team2 fball) (Game (Fitness Team1 '() '() fball 1) (Fitness Team2 '() '() fball 2) (update fball Team1 Team2) (- Generations 1)))
                )   
            )            
        )
    )
)

;;Function that creates the players.
;; Role = 0 Goalkeeper
;; Role = 1 Defense
;; Role = 2 Media
;; Role = 3 Front
(define (player speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability role)
    (list speed strength pos_Xi pos_Yi pos_Xf pos_Yf ability role)
)
;; Returns 0 if there was no goal, 1 if team 1 scored a goal and 2 if team 2 scored a goal
(define (goal fball)
    (cond
        ((<= (car fball) 50)
            (set! ScoreTeam2 (+ ScoreTeam2 1))
           
            #t
        ) 
        ((>= (car fball) 990)
            (set! ScoreTeam1 (+ ScoreTeam1 1))
           
            #t
        )
        (else
            #f
        ) 
    )    
)

;; Recursive function that creates a list with a team.
(define (First_Gen players team num)
    (cond
        ;;Empty Team
        ((null? team)
            (reverse players)
        )
        (else
            (cond
                ;;0 at the begging of the list?
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
;; Functions that assign the next player
;; Functions that players create according to the team
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
;sum of all the basic characteristics, if the ball is in the area that
;; corresponds, is considered | Xjugador-Xbola | Yjugador-Ybola |, otherwise
;; only | Yjugador-Ybola |
;; min siystem span: just consider Y?
(define(calcFitness player ball)
    (- (+ (getComposition player 0) (getComposition player 1) (getComposition player 6)) (abs(- (getComposition player 5) (car(cdr ball)))))
)

;;; Function of Fitness
;; • Goalkeepers:
;; - Closed goals.
;; - Alignment with the Y axis of the ball.
;; only a doorman, therefore clones and mutates

;; • Defenses:
;; - Alignment with the trajectory of the opposing strikers.
;; - Another thing that comes to mind.
;; the best defense is sought, cloned and mutated
  
;;•	Media:
;; - Closeness to the ball.
;; - Alignment with the trajectory of the opposing strikers.
;; the best medium is sought, cloned and mutated
  
;; • Front:
;; - Closeness to the ball
;; - Scores
;; the best forward is sought, cloned and mutated.

;; Return the new equipment.

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

;; Players attribute
; pos = 0 for Speed.
; pos = 1 for Strength
; pos = 2 for initial X
; pos = 3 for initial Y
; pos = 4 for final X
; pos = 5 for Y final
; pos = 6 for Ability
; pos = 7 for Role
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

;;Random of position
(define (randPosY)
    (random 0 (- height 40))
)
;;Funtion made a random from 1 to 10
(define (rand)
    (random 1 11)
)
;;Subtraction 1 of the first
(define (minus1 lista)
    (cons (- (car lista) 1) (cdr lista))
)
;;Funtions for lists
;;Lenght
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
;;Append
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
;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creation of players
; posx => position in x
; posy => position in and
(define (PlayersG1 posx posy lad )
  (if (equal? lad 'u)
      ((draw-pixmap PlayersField) "defensa.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ((draw-pixmap PlayersField) "defensa.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ((draw-pixmap PlayersField) "defensa.png" (make-posn posx posy))
              (if (equal? lad 'r)
                  ((draw-pixmap PlayersField) "defensa.png" (make-posn posx posy))
                  ;else
                  (void)
                  )
              )
      )
 )
  
 (copy-viewport PlayersField ventanaPrincipal)
  )

  
(define (ballG posx posy)
      ((draw-pixmap PlayersField) "bola.png" (make-posn posx posy))
     
 (copy-viewport PlayersField ventanaPrincipal)
)
  ;Jugadores G2
  (define (PlayersG2 posx posy lad )
  (if (equal? lad 'u)
      ((draw-pixmap PlayersField) "delantero.png" (make-posn posx posy))
      (if (equal? lad 'd)
          ((draw-pixmap PlayersField) "delantero.png" (make-posn posx posy))
          (if (equal? lad 'l)
              ((draw-pixmap PlayersField) "delantero.png" (make-posn posx posy))
              (if (equal? lad 'r)
                  ((draw-pixmap PlayersField) "delantero.png" (make-posn posx posy))
                  ;else
                  (void)
                  )
              )
      )
 )
  
 (copy-viewport PlayersField ventanaPrincipal)
  )
  ;transformar
  (define (Transform AliG1 AliG2 bola)
  (cond ((and (null? AliG1) (null? AliG2))
         (ballG (car bola) (car (cdr bola)))
         void)
        (else
      (begin      
        ;(teclado (car bola) (car(cdr bola)) 'up)
        (PlayersG1 (getComposition (car AliG1) 4) (getComposition (car AliG1) 5) 'r)
        (PlayersG2 (getComposition (car AliG2) 4) (getComposition (car AliG2) 5)  'r)
        
        (Transform (cdr AliG1) (cdr AliG2) bola))
        
      )
     )
  )
;;Juego
(CCCE2019 '(4 4 2) '(5 3 2) 1000)