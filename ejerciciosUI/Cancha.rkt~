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
((draw-solid-rectangle CampoJugadores) (make-posn 0 0) 50 500 "green")
;Limite del porteroG1
((draw-solid-rectangle CampoJugadores) (make-posn 50 0) 150 500 "yellow")
;Limite del porteroG1
((draw-solid-rectangle CampoJugadores) (make-posn 200 0) 150 500 "orange")
;Limite del porteroG1
((draw-solid-rectangle CampoJugadores) (make-posn 350 0) 150 500 "blue")
;Limite del porteroG2
((draw-solid-rectangle CampoJugadores) (make-posn 950 0) 50 500 "green")
;Limite del porteroG2
((draw-solid-rectangle CampoJugadores) (make-posn 800 0) 150 500 "yellow")
;Limite del porteroG2
((draw-solid-rectangle CampoJugadores) (make-posn 650 0) 150 500 "orange")
;Limite del porteroG2
((draw-solid-rectangle CampoJugadores) (make-posn 500 0) 150 500 "blue")
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;nueva ventana
(copy-viewport CampoJugadores ventanaPrincipal)