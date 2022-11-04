#lang racket/gui

(define WORLD-COLOR (make-color 122 154 214))  ; background color
(define WALL-COLOR (make-color 8 47 120))      ; wall color
(define SNAKE-COLOR (make-color 240 245 255))  ; not-so-snake color
(define FOOD-COLOR (make-color 224 202 130))   ; grapes color
(define FRAME-WIDTH 300)                       ; window width
(define FRAME-HEIGHT (* FRAME-WIDTH 2))        ; window height


(define main-frame
  (new frame%
     [label "not so snake!"]
     [width FRAME-WIDTH]
     [height FRAME-HEIGHT]))


(define game-canvas%
  (class canvas%

    ; initialize variables
    (define current-direction 'right) ; initial moving direction
    (define x-pos 10)                 ; initial x position
    (define y-pos 250)                ; initial y position
    (define speed 2)                  ; initial speed
    (define DIAMETER 20)              ; default not-so-snake diameter px
    (define food-x-pos (random 12 (- FRAME-WIDTH 55)))     ; initial x position of the grapes
    (define food-y-pos (random 110 (- FRAME-HEIGHT 155)))  ; initial y position of the grapes
    (define counter 0)                ; initial score counter
    (define score (number->string counter))                ; initial score text
    (define game-state 0 )            ; initial game state set to running

    
    (define timer
      (new timer%
           [notify-callback
            (lambda()
              (case current-direction
                ['left    ; not-so-snake is moving to the left
                 (when (< x-pos 15)
                   ; when not-so-snake hits the left wall, stop the game
                   (send timer stop)
                   (set! game-state 1))
                 (when (check x-pos food-x-pos)
                   ; when not-so-snake collides with grapes, increase not-so-snake's speed
                   ; then randomly reset grapes's location
                   (set! food-x-pos (random 10 (- FRAME-WIDTH 55)))
                   (set! food-y-pos (random 110 (- FRAME-HEIGHT 175)))
                   (set! speed (+ speed 1))
                   (set! counter (+ counter 1))
                   (set! score (number->string counter)))
                 (set! x-pos (- x-pos speed))]
                
                ['right    ; not-so-snake is moving to the right
                 (when (> x-pos (- FRAME-WIDTH 50))
                   ; when not-so-snake hits the right wall, stop the game
                   (send timer stop)
                   (set! game-state 1))
                 (when (check x-pos food-x-pos)
                   (set! food-x-pos (random 10 (- FRAME-WIDTH 55)))
                   (set! food-y-pos (random 110 (- FRAME-HEIGHT 175)))
                   (set! speed (+ speed 1))
                   (set! counter (+ counter 1))
                   (set! score (number->string counter)))
                 (set! x-pos (+ x-pos speed))]
                
                ['up      ; not-so-snake is moving upward
                 (when (< y-pos 100)
                   ; when not-so-snake hits the top wall, stop the game
                   (send timer stop)
                   (set! game-state 1))
                 (when (check x-pos food-x-pos)
                   (set! food-x-pos (random 10 (- FRAME-WIDTH 55)))
                   (set! food-y-pos (random 110 (- FRAME-HEIGHT 175)))
                   (set! speed (+ speed 1))
                   (set! counter (+ counter 1))
                   (set! score (number->string counter)))
                 (set! y-pos (- y-pos speed))]
                
                ['down    ; not-so-snake is moving downward
                 (when (> y-pos (- FRAME-HEIGHT 60))
                   ; when not-so-snake hits the bottom wall, stop the game
                   (send timer stop)
                   (set! game-state 1))
                 (when (check x-pos food-x-pos)
                   (set! food-x-pos (random 10 (- FRAME-WIDTH 55)))
                   (set! food-y-pos (random 110 (- FRAME-HEIGHT 175)))
                   (set! speed (+ speed 1))
                   (set! counter (+ counter 1))
                   (set! score (number->string counter)))
                 (set! y-pos (+ y-pos speed))]
              )
              (send this refresh))
            ]
           ))
    
    
     (define/override (on-char event)
       (case (send event get-key-code)
        ['left    ; when left key is pressed, make not-so-snake move to the left
         (set! current-direction 'left)]
         
        ['right   ; when right key is pressed, make not-so-snake move to the right
         (set! current-direction 'right)]
         
        ['up      ; when up key is pressed, make not-so-snake move upward
         (set! current-direction 'up)]
         
        ['down    ; when down key is pressed, make not-so-snake move downward
         (set! current-direction 'down)]))

    
    (define (check x-pos food-x-pos)
      ; check the positions of not-so-snake and grapes
      (if (and (member food-y-pos (range (- y-pos 15) (+ y-pos 15) 1)) (member food-x-pos (range (- x-pos 15) (+ x-pos 15) 1)))
          #t      ;  if grapes comes within range of not-so-snake
          #f))

    
    (send timer start 20)
          
    
    (define/private (custom-paint-callback canvas dc)
      ; the world
      ; - a box iinside the frame
      (send dc set-brush WORLD-COLOR 'solid)
      (send dc set-pen WALL-COLOR 5 'solid)
      (send dc draw-rectangle
            10 100 
            (- FRAME-WIDTH 35) (- FRAME-HEIGHT 155))

      ; not-so-snake
      ; - just a circle
      (send dc set-brush SNAKE-COLOR 'solid)
      (send dc set-pen "white" 1 'transparent)
      (send dc draw-ellipse x-pos y-pos DIAMETER DIAMETER)
      
      ; grapes
      ; - unicode for grapes
      (send dc set-text-foreground FOOD-COLOR)
      (send dc set-font (make-font #:size 14 #:weight 'bold))
      (send dc draw-text "\U1F347" food-x-pos food-y-pos)

      ; wall
      (send dc set-text-foreground WALL-COLOR)
      (send dc draw-text (~a "\U1F347 eaten " score) 30 50)

      ; game over
      (when (= game-state 1)
        (send dc set-brush (make-color 0 0 0 0.5) 'solid)
        (send dc draw-rectangle 0 0 FRAME-WIDTH FRAME-HEIGHT)
        (send dc set-text-foreground FOOD-COLOR)
        (send dc set-font (make-font #:size 26 #:weight 'bold))
        (send dc draw-text "GAME OVER" (/ FRAME-WIDTH 6) (/ FRAME-HEIGHT 2))
        (send dc draw-text (~a score "\U1F347") (* FRAME-WIDTH 0.35) (* FRAME-HEIGHT 0.4)))
      )

    (super-new
     (paint-callback (lambda (canvas dc) (custom-paint-callback canvas dc)))
     )
    ))


(define main-canvas
  (new game-canvas% [parent main-frame]))


(send main-frame show #t)

