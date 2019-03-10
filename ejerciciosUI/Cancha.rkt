#lang racket
;Tutorial
(require (lib "graphics.ss" "graphics"))
(require htdp/draw)
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
((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/cancha.bmp" (make-posn 0 0))
((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/bola.bmp" (make-posn 500 220))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Creacion de jugadores
;posx => posicion en x
;posy => posicion en y
;lad => tecla que mueve el jugador, Preguntar al profe(aqui creo que va el algoritmo genetico)
(define (jugadoresG1 posx posy lad )
  (if (equal? lad 'u)
      ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
      ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/defensa.bmp" (make-posn posx posy))
      (if (equal? lad 'd)
          ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
          ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/defensa.bmp" (make-posn posx posy))
          (if (equal? lad 'l)
              ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
              ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/defensa.bmp" (make-posn posx posy))
              (if (equal? lad 'r)
                  ;((draw-solid-rectangle CampoJugadores) (make-posn posx posy) 10 10 "black")
                  ((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/defensa.bmp" (make-posn posx posy))
                  ;else
                  (void)
                  )
              )
      )
 )
  
 (copy-viewport CampoJugadores ventanaPrincipal)
  ;((clear-viewport CampoJugadores))
  )

(define (jugadoresG2 posx posy lad )
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
(define (teclado limR limI posx posy press generacion)

        
  ;limite derecha
  (if (< posx limR)
      (begin
        (jugadoresG1 limR posy 'r)
        (jugadoresG1 limR (+ posy 200) 'r)
        (teclado limR limI limR posy (key-value(get-key-press ventanaPrincipal)) (- generacion 1)))
      ;limite de la izquiera TODO un parametro
  (if (> posx (- limI 10))
      (begin
        (jugadoresG1 (- limI 10) posy 'r)
        (jugadoresG1 (- limI 10) (+ posy 200) 'r)
        (teclado limR limI (- limI 10) posy (key-value(get-key-press ventanaPrincipal)) (- generacion 1)))
      ;limite arriba
  (if (< posy 0)((draw-pixmap CampoJugadores) "/Users/bertha/Documents/Dr.Racket/ConcaChampionsCE/ejerciciosUI/cancha.bmp" (make-posn 0 0))
      (begin
        (jugadoresG1 posx 0 'u)
        (jugadoresG1 (+ posx 200) 0 'u)
        (teclado limR limI posx 0 (key-value(get-key-press ventanaPrincipal)) (- generacion 1)))
      ;limite de abajo
  (if (> posy 490)
      (begin
        (jugadoresG1 posx  490 'd)
        (jugadoresG1 (+ posx 200) 490 'd)
        (teclado limR limI posx 490 (key-value(get-key-press ventanaPrincipal)) (- generacion 1)))    
  (if (equal? press 'up)
      (begin
        (jugadoresG1 posx posy 'u)
        (jugadoresG1 (+ posx 200) (+ posy 200) 'u)
        (teclado limR limI posx (- posy 10) (key-value (get-key-press ventanaPrincipal)) (- generacion 1)))
      (if (equal? press 'down)
          (begin
            (jugadoresG1 posx posy 'd)
            (jugadoresG1 (+ posx 200) (+ posy 200) 'd)
            (teclado limR limI posx (+ posy 10) (key-value (get-key-press ventanaPrincipal)) (- generacion 1)))
      (if (equal? press 'left)
          (begin
            (jugadoresG1 posx posy 'l)
            (jugadoresG1 (+ posx 200) (+ posy 200) 'l)
            (teclado limR limI (- posx 10) posy (key-value (get-key-press ventanaPrincipal)) (- generacion 1)))
      (if (equal? press 'right)
          (begin
            (jugadoresG1 posx posy 'r)
            (jugadoresG1 (+ posx 200) (+ posy 200) 'r)
            (teclado limR limI (+ posx 10) posy (key-value (get-key-press ventanaPrincipal)) (- generacion 1)))
          ;else
          (teclado limR limI posx posy (key-value (get-key-press ventanaPrincipal)) (- generacion 1))
          )
    )
      )
      )
  )
  ))
  ))



(define (transformar AliG1 AliG2)
  (cond ((and (null? AliG1) (null? AliG2))
         void)
        (else
      (begin
        (jugadoresG1 (car (car AliG1)) (car (cdr (car AliG1))) 'r)
        (jugadoresG2 (car (car AliG2)) (car (cdr (car AliG2))) 'r)
        (transformar (cdr AliG1) (cdr AliG2)))
      )
     )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;defensas ;;;;;;;;;;
(define (defensaG1 num)
  (cond ((zero? num)
         '())
        (else
         (cons (list 290 (/ 500 (+ 1 num))) (defensaG1 (- num 1)) ) 
            )))
(define (defensaG2 num)
  (cond ((zero? num)
         '())
        (else
         (cons (list 725 (/ 500 (+ 1 num))) (defensaG2 (- num 1)) ) 
            )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;delanteros;;;;
(define (delanteroG1 num)
  (cond ((zero? num)
         '())
        (else
         (cons (list 680 (+ 100 (/ 500 (+ 1 num)))) (delanteroG1 (- num 1)) ) 
            )))
(define (delanteroG2 num)
  (cond ((zero? num)
         '())
        (else
         (cons (list 250 (+ 100 (/ 500 (+ 1 num)))) (delanteroG2 (- num 1)) ) 
            )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;medios
(define (mediosG1 num)
  (cond ((zero? num)
         '())
        (else
         (cons (list 495 (/ 500 (+ 1 num))) (mediosG1 (- num 1)) ) 
            )))
(define (mediosG2 num)
  (cond ((zero? num)
         '())
        (else
         (cons (list 450 (/ 500 (+ 1 num))) (mediosG2 (- num 1)) ) 
            )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Crear Grupo 1;;;;;;;
(define (G1 lista)
  (cond ((null? lista)
         porteroG1)
        ((= (largo lista) 3)
         (append  (defensaG1 (car lista)) (G1 (cdr lista))))
        ((= (largo lista) 2)
         (append  (mediosG1 (car lista)) (G1 (cdr lista))))
        ((= (largo lista) 1)
         (append  (delanteroG1 (car lista)) (G1 (cdr lista))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Crear G2;;;;;;;;;
(define (G2 lista)
  (cond ((null? lista)
         porteroG2)
        ((= (largo lista) 3)
         (append  (defensaG2 (car lista)) (G2 (cdr lista))))
        ((= (largo lista) 2)
         (append  (mediosG2 (car lista)) (G2 (cdr lista))))
        ((= (largo lista) 1)
         (append  (delanteroG2 (car lista)) (G2 (cdr lista))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;largo lista;;;;;;;;;;
(define (largo lista)
  (cond((empty? lista)
        0)
       (else
        (+ 1 (largo (cdr lista))))))
;;;;;;;;;;;;;;;;;;
;;;;;transformar
(define (CCCE2019 AlG1 AlG2 G)
  (cond ((zero? G)
        (void))
        (else
         ((transformar (G1 AlG1) (G2 AlG2)) (CCCE2019 AlG1 AlG2 (- 1 G))))))
  

(define porteroG1
  '((25 250))
  )
(define porteroG2
  '((970 250))
  )

;(teclado 800 950 900 250 'up 10)
  ;(teclado 0 50 10 250 'up)

;(CCCE20192 '((50 82) (150 164) (240 246) (200 328) (350 410) (400 125) (380 250) (500 375) (750 260) (750 320)) '((850 100) (850 200) (850 300) (850 400) (600 100) (600 200) (600 300) (600 400) (350 160) (350 320)))
(CCCE2019 '(4 4 2) '(4 2 4) 1)