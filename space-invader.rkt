;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; ================================CONSTANTS==============================



(define WIDTH  300)
(define HEIGHT 500)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define HIT-RANGE 10)
(define INVADE-RATE 200)


(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer



;; ====================Data Definitions========================================



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - (TANK-HEIGHT/2) in screen coordinates
;;         the tank stops if dir is 0 
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 0))   ;center and stopped(namely represent the initial condition of the tank
(define T1 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T2 (make-tank 50 1))            ;going right
(define T3 (make-tank 1 -1))           ;will go outside of the screen,so STOP!

#;
(define (fn-for-tank t)
  (... (tank-x t)
       (tank-dir t)))




(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader movement will be left downward with (INVADER-X-SPEED , INVADER-Y-SPEED) if dir = -1
;;         the invader movement will be right downward with (INVADER-X-SPEED, INVADER-Y-SPEED) if dir = 1


(define I1 (make-invader 150 100 1))       ; not landed, moving right
(define I2 (make-invader 299 150 1))       ; just about to touch right border, will be moving left
(define I3 (make-invader 1 88 -1))         ; just about to touch right border, will be moving right
(define I4 (make-invader 33 HEIGHT -1))    ; touched the bottom -> the game will be stopped

#;
(define (fn-for-invader invader)
  (... (invader-x invader)
       (invader-y invader)
       (invader-dir invader)))




;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. as the list of invaders currently on the screen.

(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I2 LOI1))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (...
          (fn-for-invader (first loi))
          (fn-for loinvader (rest loi)))]))




(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                      
(define M2 (make-missile 150 98))  
(define M3 (make-missile 180 345)) 

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))




;;ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. as list of missiles currently on the screen.

(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M2 LOM1))

(define (fn-for-lomissile lom)
  (cond [(empty? lom) (...)]
        [else
         (...
          (fn-for-missile (first lom))
          (fn-for-lomissile (rest lom)))]))


 

(define-struct game (invaders missiles tank))
;; Game is (make-game  ListOfInvader ListOfMissile Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position on the screen

;(define G0 (make-game LOI0 LOM0 T0))
;(define G1 (make-game LOI1 LOM1 T1))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T2))
(define G3 (make-game (list I1 I2) (list M1 M2) T3))  ; I1 & M2 will collide

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



;; ===========================BIG-BANG===========================



;; Game -> Game
;; start the game by (main (make-game empty empty (make-tank (/ WIDTH 2) 0)))

(define (main g )
  (big-bang g                      ; Game
    (on-key handle-key-game)       ; Game KeyEvent -> Game
    (on-tick next-game)         ; Game -> Game
    (to-draw render-game)          ; Game -> Image
    (stop-when reached-bottom?)))  ; Game ->  Boolean



; =======HANDLE KEY EVENT===================



;; Game KeyEvent -> Game
;; make change in position of Tank is left or rigth arrow is pressed
;; create missile is space bar is pressed
;; do nothing otherwise

(check-expect (handle-key-game  G0 "left") (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect (handle-key-game  G1 "right") G1)
(check-expect (handle-key-game  G2 "left") (make-game (list I1) (list M1)
                                                      (make-tank 50 -1)))
(check-expect (handle-key-game G3 " ") (make-game (list I1 I2)
                                                     (list (make-missile 1 496) M1 M2)
                                                     T3))


; (define (handle-key-game t ke) T0) ;stub

(define (handle-key-game g ke)
  (make-game  (game-invaders g)
              (handle-key-missile (game-missiles g) (tank-x (game-tank g)) ke)
              (handle-key-tank (game-tank g) ke)))



;; ListOfMissile Integer[0, 300] KeyEvent -> ListOfMissile
;; create a missile from the current x-position(Integer[0, 300]) of the Tank if space is being pressed
;; otherwise do nothing

(check-expect (handle-key-missile  LOM0 22 " ") (cons (make-missile 22 (- HEIGHT 4)) LOM0))
(check-expect (handle-key-missile  LOM1 34 "a") LOM1)
(check-expect (handle-key-missile  LOM2 67 " ") (cons (make-missile 67 496) LOM2))

(define (handle-key-missile lom x ke)
  (cond [(key=? ke " ")
         (cons (make-missile x (- HEIGHT 4)) lom)]
        [else
         lom]))



;; Tank KeyEvent -> Tank
;; make the Tank moving direction into left if left arrow key is pressed
;; make the Tank moving diretion into right if rigth arrow key is pressed
;; keep the tank as it is otherwise
  
(check-expect (handle-key-tank T0 "left") (make-tank (tank-x T0) -1))
(check-expect (handle-key-tank T2 "right") T2)
(check-expect (handle-key-tank T3 "right") (make-tank (tank-x T3) 1))

(define (handle-key-tank t ke)
  (cond [(key=? ke "left")
         (make-tank (tank-x t) -1)]
        [(key=? ke "right")
         (make-tank (tank-x t) 1)]
        [else t]))


;; ================ON-TICK===========



;; Game -> Game
;; produce the next instance of the game

; (define (next-game g) G1) ;stub

;;<INCLUDE CHECK EXPECT >

(define (next-game g)
  (create-game (advance-game (filter-game g))))



;; Game -> Game
;; check if an Invader collide with a Missile
;; if it does, delete both of them or do nothing

(check-expect (filter-game G1) G1)
(check-expect (filter-game G3) (make-game (list I2) (list M1) T3)) ;I1 & M2 destroy each other

; (define (filter-game g) G0) ;stub

(define (filter-game g)
  (cond [(empty? (game-invaders g)) g]
        [(empty? (game-missiles g)) g]
        [else
         (if (no-collision? g)

             (add-invader (first (game-invaders g))
                          (filter-game (filter-game-input g)))
             
             (filter-game (filter-game-input g)))]))



;; Game -> Boolean
;; produce true if there is no collision happend inbetween Invader & Missile, false otherwise

;;<INCLUDE CHECKEXPECT>

; (define (no-collision g) true) ;stub

(define (no-collision? g)
  (same? (game-missiles g)
         (search-collision (first (game-invaders g))
                           (game-missiles g))))



;; ListOfMissile ListOfMissile -> Boolean
;; produce true if both lists are same, false otherwise

;(define (same? lom1 lom2) false) ;stub

(define (same? lom1 lom2)
  (cond [(empty? lom1) true]
        [(empty? lom2) false]
        [else
         (same? (rest lom1)
                (rest lom2))]))



;; Invader ListOfMissile -> ListOfMissile
;; Search if the Invader has collided with any other Missile in ListOfMissile
;; Delete the Missile from the ListOfMissile if it does, do nothing otherwise

;; <INCLUDE CHECKEXPECT >

; (define (search-collision i lom) LOM1)

(define (search-collision i lom)
  (cond [(empty? lom) empty]
        [else
         (if (close? i (first lom))
             (rest lom)
             (cons (first lom) (search-collision i (rest lom))))]))



;; Invader Missile -> Boolean
;; produce true if the Missile is close enough to hit the Invader, false otherwise

;;<INLCUDE CEHCKEWXPEW>

; (define (close? i m) true) ;stub

(define (close? i m)
  (< (sqrt (+ (sqr (- (invader-x i) (missile-x m)))
              (sqr (- (invader-y i) (missile-y m)))))
     10))
           


;; Invader Game -> Game
;; the Invader into the ListOfInvaders inside the Game

;;<INCLUDE CHECKEXPECT >

; (define (add-invader i g) G1) ;stub

(define (add-invader i g)
  (make-game
   (cons i (game-invaders g))
   (game-missiles g)
   (game-tank g)))



;; Game -> Game
;; process the input for "filter-game" function

;; <INCLUDE CHECKEXPECT >

; (define (filter-game-input g) G1)

(define (filter-game-input g)
  (make-game (rest (game-invaders g))
             (search-collision (first (game-invaders g))
                               (game-missiles g))
             (game-tank g)))


 
;; Game -> Game
;; Advance everything in the screen to their respective direction,advance the whole game

(check-random (advance-game G0) (make-game (cons (make-invader (random WIDTH) -1.5 1) empty) empty (make-tank 150 0)))
(check-random (advance-game G1) (make-game (cons  (make-invader 151.5 101.5 1) empty)
                                        (cons (make-missile 150 290) empty)
                                        (make-tank 52 1)))

; (define (advance-game g) G1) ;stub

(define (advance-game g)
  (make-game (advance-invaders (game-invaders g))
             (advance-missiles (game-missiles g))
             (advance-tank (game-tank g))))



;; ListOfInvader -> ListOfInvader
;; advance the Invaders position in their respective direction (left/right)

(check-expect (advance-invaders LOI0) empty)
(check-expect (advance-invaders LOI1) (cons (make-invader 151.5 101.5 1) empty))
(check-expect (advance-invaders LOI2) (cons (make-invader WIDTH 151.5 -1) (cons (make-invader 151.5 101.5 1) empty)))

; (define (advance-invaders loi) LOI2) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader(first loi))
               (advance-invaders (rest loi)))]))



;; Invader -> Invader
;; advance an Invader with respective x & y coordinate speed.
;; also check if they will hit left or right border of the screen

(check-expect (advance-invader I1) (make-invader 151.5 101.5 1))
(check-expect (advance-invader I2) (make-invader WIDTH 151.5 -1))

; (define (advance-invader i) I2) ;stub

(define (advance-invader i)
  (cond [(hitting-right? i)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) -1)]
        
        [(hitting-left? i)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) 1)]
        
        [else
         (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i)))
                       (+ (invader-y i) INVADER-Y-SPEED )
                       (invader-dir i))]))
      


;; Invader -> Boolean
;; produce true if the Invader will hit the right border of the screen

;; <CHECK EXPECT>

; (define (hitting-right? i) true) ;stub

(define (hitting-right? i)
  (> (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i))) WIDTH))



;; Invader -> Boolean
;; produce true if the Invader will hit the left border of the screen

;; <CHECK EXPECT>

; (define (hitting-left? i) true) ;stub

(define (hitting-left? i)
  (> 0 (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i)))))



;; ListOfMissile -> ListOfMissile
;; advance all the Missile in the ListOfMissile upward at a constant rate
;; also check if any Missile is going outside the screen.delete it if it does.

(check-expect (advance-missiles LOM0) empty)
(check-expect (advance-missiles LOM1) (cons (make-missile 150 290) empty))
(check-expect (advance-missiles LOM2) (cons (make-missile 15 49) (cons (make-missile 150 290) empty)))

; (define (advance-missiles lom) LOM2) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (out-of-screen? (first lom))
             (advance-missiles (rest lom))
             (cons (advance-missile (first lom))
                   (advance-missiles (rest lom))))]))



;; Missile -> Boolean
;; produce true if the Missile is going to be out of the screen, false otherwise

;; <INCLUDE CHECKEXPECT>

; (define (out-of-screen? m) true) ;stub

(define (out-of-screen? m)
  (< (- (missile-y m) MISSILE-SPEED) 0))



;; Missile -> Missile
;; Advance the missile upward from its current position

;; <INCLUDE CHECKEXPECT>

; (define (advance-missile m) M1) ;stub

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; Advance the tank in its respective direction (Left/Right)

(check-expect (advance-tank T0) (make-tank (+ (tank-x T0) (* TANK-SPEED (tank-dir T0))) (tank-dir T0)))
(check-expect (advance-tank T1) (make-tank 52 1)) 

; (define (advance-tank t) T1) ; stub

;; <Tank template used>

(define (advance-tank t)
  (cond [(toomuch-left? t)
         (make-tank 0 0)]
        [(toomuch-right? t)
         (make-tank WIDTH 0)]
        [else 
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))



;; Tank -> Boolean
;; produce true if the tank is about to get out of the screen through left side

;; <CHECK EXPCET >

; (define (toomuch-left? t) true) ;stub

(define (toomuch-left? t)
  (< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0))



;; Tank -> Boolean
;; produce true if the tank is about to get out of the screen through right side

;; <CHECK EXPCET >

; (define (toomuch-right? t) true) ;stub

(define (toomuch-right? t)
  (> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH))



;; ===============STOP-WHEN=============


;; Game -> Boolean
;; check if any Invader touch the bottom border of the screen

;; <INCLUDE CHECKEXPECT

; (define (reached-bottom? g) true) ;stub

(define (reached-bottom? g)
  (invader-reached-bottom? (game-invaders g)))



;; ListOfInvader -> Boolean
;; produce true if an Invader from ListOfInvader touch the bottom border of the screen, false otherwise

;; <INCLUDE CHECKEXPECT

; (define (reached-bottom? g) true) ;stub

(define (invader-reached-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (or (> (invader-y (first loi)) (- HEIGHT 3))
             (invader-reached-bottom? (rest loi)))]))




;;Game -> Game
;; produce new Game with adding(or not adding) Invader into ListOfInvader randomly



; (define (create-game g) G1) ;stub
 
(define (create-game g)
  (make-game (create-invader (game-invaders g))
             (game-missiles g)
             (game-tank g)))

;; ListOfInvader -> ListOfInvader
;; create a new Invader in randomize way

;(check-random (create-invader LOI0) (cons (make-invader (random WIDTH) -3 1) LOI0))
;(check-random (create-invader LOI1) (cons (make-invader (random WIDTH) -3 1) LOI1))
;(check-random (create-invader LOI2) (cons (make-invader (random WIDTH) -3 1) LOI2))

; (define (create-invader loi) LOI1) ;stub

(define (create-invader loi)
  (cond [(empty? loi)
         (cons (make-invader (random WIDTH) -3 1) loi)]
        [else 
         (if (< (random INVADE-RATE) 3)
             (cons (make-invader (random WIDTH) -3 1)
                   loi)
             loi)]))
;; ===================TO-DRAW=========================

     

;; Game -> Image
;; produce the rendered image of the current instance of the Game

(check-expect (render-game G0) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-game G1) (place-image TANK 50 (- 500 TANK-HEIGHT/2)
                                            (place-image INVADER 150 100 
                                                         (place-image MISSILE 150 300 BACKGROUND))))

; (define (render-game g) BACKGROUND) ;stub

(define (render-game g)
  (render-tank (game-tank g)
               (render-invaders (game-invaders g)
                                (render-missiles (game-missiles g)))))



;; Tank Image -> Image
;; place the Tank image on top of the given Image

;; < check expect >

; (define (render-tank t img) BACKGROUND) ;stub

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))



;; ListOfInvader Image -> Image
;; place the Image of Invaders in the ListOfInvader on top of the given image

;; <check expect>

; (define (render-invaders loi img) BACKGROUND) ;

(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (cons (make-invader 150 100 1) empty) BACKGROUND) (place-image INVADER 150 100 BACKGROUND))


(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi) img))]))



;; ListOfMissile -> Image
;; place Image of Missile in the ListOfMissile on top of the BACKGROUND

(check-expect (render-missiles LOM0) BACKGROUND)
(check-expect (render-missiles LOM1) (place-image MISSILE 150 100 BACKGROUND))

(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom)))]))



;; ============================THE END========================