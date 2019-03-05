#lang racket
;Tutorial
(require (lib "graphics.ss" "graphics"))
(open-graphics)

;ventana principal de la cancha
(define ventanaPrincipal (open-viewport "Cancha" 1200 500))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ventana ocultas para definir los limites de jugadores
(define CampoJugadores (open-pixmap  "PorteroG1"  1200 500))
;Limite del porteroG1
;((draw-solid-rectangle CampoJugadores) (make-posn 0 0) 50 500 "green")
;Limite del porteroG1
;((draw-solid-rectangle CampoJugadores) (make-posn 50 0) 150 500 "yellow")
;Limite del porteroG1
;((draw-solid-rectangle CampoJugadores) (make-posn 200 0) 150 500 "orange")
;Limite del porteroG1
;((draw-solid-rectangle CampoJugadores) (make-posn 350 0) 150 500 "blue")
;Limite del porteroG2
;((draw-solid-rectangle CampoJugadores) (make-posn 950 0) 50 500 "green")
;Limite del porteroG2
;((draw-solid-rectangle CampoJugadores) (make-posn 800 0) 150 500 "yellow")
;Limite del porteroG2
;((draw-solid-rectangle CampoJugadores) (make-posn 650 0) 150 500 "orange")
;Limite del porteroG2
;((draw-solid-rectangle CampoJugadores) (make-posn 500 0) 150 500 "blue")
;Rectangulo para colocar informacion 
((draw-solid-rectangle CampoJugadores) (make-posn 1000 0) 200 500 "darkgray")
;Portada
((draw-string CampoJugadores) (make-posn 1100 20) "Tarea #1 Lenguajes")
((draw-string CampoJugadores) (make-posn 1010 30) "Estudiantes:")
((draw-string CampoJugadores) (make-posn 1030 50) "Bertha Brenes (2017101642)")
((draw-string CampoJugadores) (make-posn 1030 70) "Gabriel (2017101642)")
((draw-string CampoJugadores) (make-posn 1030 90) "Maria (2017101642)")
;Marcador
((draw-string CampoJugadores) (make-posn 1010 120) "Marcador:")
;Delimitadores
((clear-solid-rectangle CampoJugadores) (make-posn 1000 0) 4 500)
((clear-solid-rectangle CampoJugadores) (make-posn 1000 100) 200 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Creacion de jugadores
;posx => posicion en x
;posy => posicion en y
;lad => tecla que mueve el jugador, Preguntar al profe(aqui creo que va el algoritmo genetico)
(define (jugadores posx posy lad )
  (if (equal? lad 'u)
      ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
      ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/delantero.bmp" (make-posn posx posy))
      (if (equal? lad 'd)
          ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
          ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/delantero.bmp" (make-posn posx posy))
          (if (equal? lad 'l)
              ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
              ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/delantero.bmp" (make-posn posx posy))
              (if (equal? lad 'r)
                  ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
                  ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/delantero.bmp" (make-posn posx posy))
                  ;else
                  (void)
                  )
              )
      )
      )
 (copy-viewport CampoJugadores ventanaPrincipal)
  ;((clear-viewport CampoJugadores))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion para que el jugador se mueva desde el teclada
;Ademas genera los limites para que el jugadores no se mueve mas alla de sus limite
;limR => limite derecho del jugador
;limI => limite izq del jugador
;posx => posicion en x
;posy => posicion en y
;press => la tecla que se esta presionando
(define (teclado limR limI posx posy press)
  ;limite derecha
  (if (< posx limR)
      (begin
        (jugadores limR posy 'r)
        (teclado limR limI limR posy (key-value(get-key-press ventanaPrincipal))))
      ;limite de la izquiera TODO un parametro
  (if (> posx (- limI 10))
      (begin
        (jugadores (- limI 10) posy 'r)
        (teclado limR limI (- limI 10) posy (key-value(get-key-press ventanaPrincipal))))
      ;limite arriba
  (if (< posy 0)
      (begin
        (jugadores posx 0 'u)
        (teclado limR limI posx 0 (key-value(get-key-press ventanaPrincipal))))
      ;limite de abajo
  (if (> posy 490)
      (begin
        (jugadores posx  490 'd)
        (teclado limR limI posx 490 (key-value(get-key-press ventanaPrincipal))))    
  (if (equal? press 'up)
      (begin
        (jugadores posx posy 'u)
        (teclado limR limI posx (- posy 10) (key-value (get-key-press ventanaPrincipal))))
      (if (equal? press 'down)
          (begin
            (jugadores posx posy 'd)
            (teclado limR limI posx (+ posy 10) (key-value (get-key-press ventanaPrincipal))))
      (if (equal? press 'left)
          (begin
            (jugadores posx posy 'l)
            (teclado limR limI (- posx 10) posy (key-value (get-key-press ventanaPrincipal))))
      (if (equal? press 'right)
          (begin
            (jugadores posx posy 'r)
            (teclado limR limI (+ posx 10) posy (key-value (get-key-press ventanaPrincipal))))
          ;else
          (teclado limR limI posx posy (key-value (get-key-press ventanaPrincipal)))
          )
    )
      )
      )
  )
  ))
  ))
(define porteroG1
  '(0 50 10 250)
  )
(define defensaG1 
  '(50 200 60 250)
  )
(define medioG1
  '(200 350 250 250)
  )
(define delanteroG1
  '(350 500 400 250))

(define delanteroG2
  '(500 650 550 250))

(define medioG2
  '(650 800 700 250))

(define defensaG2
  '(800 950 900 250))

(define porteroG2
  '(950 1000 1040 250))

(teclado 350 500 400 250 'up)
  ;(teclado 0 50 10 250 'up)
