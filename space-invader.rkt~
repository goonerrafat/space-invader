;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaderv3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Constatns:

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

;; Data Definitions:

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))







(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 299 150 1))       ;exactly landed, moving left
; (define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dir invader)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. as the list of invaders..........

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
(define M2 (make-missile 15 59))  
(define M3 (make-missile 180 345)) 

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



;;ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. as list of missile


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
;; Game is (make-game ListOfMissile Tank)
;;interp. as game situation at a moment

(define G0 (make-game LOI0 LOM0 T0))
(define G1 (make-game LOI1 LOM1 T1))







;; Big-Bang

;; Game -> Game
;; start with (main (make-game empty empty (make-tank (/ WIDTH 2) 0)))

(define (main g )
  (big-bang g 
    (on-key handle-key)
    (on-tick advance-game)
    (to-draw render-game)))


;; function pieces

;; Game -> Game

(check-expect (handle-key G0 "left") (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect (handle-key G1 "right") G1)
(check-expect (handle-key G1 "left") (make-game (game-invaders G1)
                                                (game-missiles G1)
                                                (make-tank (tank-x T2) -1)))
;; << NOT SUFFICIENT TEST CASES>ADD SPACE TEST CASES

; (define (handle-key t ke) T0) ;stub

(define (handle-key g ke)
  (make-game  (game-invaders g)
              (handle-missiles (game-missiles g) (game-tank g) ke)
              (handle-tank (game-tank g) ke)))






;; ListOfMissile -> ListOfMissile
;; projdfdsfj

(check-expect (handle-missiles LOM0 T0 " ") (cons (make-missile (tank-x T0 ) (- HEIGHT 4)) LOM0))
(check-expect (handle-missiles LOM1 T1 " ") (cons (make-missile (tank-x T1) (- HEIGHT 4)) LOM1))
(check-expect (handle-missiles LOM1 T1 "a") LOM1)

(define (handle-missiles lom t ke)
  (cond [(key=? ke " ")
         (cons (make-missile (tank-x t) (- HEIGHT 4)) lom)]
        [else
         lom]))


;; Tank -> Tank
  
(check-expect (handle-tank T0 "left") (make-tank (tank-x T0) -1))
(check-expect (handle-tank T1 "right") T1)
(check-expect (handle-tank T2 "right") (make-tank (tank-x T2) 1))

  
(define (handle-tank t ke)
  (cond [(key=? ke "left")
         (make-tank (tank-x t) -1)]
        [(key=? ke "right")
         (make-tank (tank-x t) 1)]
        [else t]))











;; Game -> Game
;;lkdfjdj



(define (advance-game g)
  (next-game (collision-detection g)))



(define (collision-detection g)
  (cond [(empty? (game-invaders g)) g]
        [(empty? (game-missiles g)) g]
        [else
         (if (same? (game-missiles g)
                    (search-collision (first (game-invaders g))
                                      (game-missiles g)))
             (make-game (cons (first (game-invaders g))
                              (game-invaders (collision-detection
                                              (make-game
                                               (rest (game-invaders g))
                                               (game-missiles g)
                                               (game-tank g)))))
                        (game-missiles (collision-detection
                                        (make-game 
                                         (rest (game-invaders g))
                                         (game-missiles g)
                                         (game-tank g))))
                        (game-tank g))

             (collision-detection (make-game
                                   (rest (game-invaders g))
                                   (game-missiles g)
                                   (game-tank g))))]))



(define (same? lom1 lom2)
  (cond [(empty? lom1) true]
        [(empty? lom2) false]
        [else
         (same? (rest lom1)
                (rest lom2))]))

(define (search-collision i lom)
  (cond [(empty? lom) empty]
        [else
         (if (close? i (first lom))
             (rest lom)
             (cons (first lom) (search-collision i (rest lom))))]))

(define (close? i m)
  (< (sqrt (+ (sqr (- (invader-x i) (missile-x m)))
                      (sqr (- (invader-y i) (missile-y m)))))
     10))
           





(check-random (next-game G0) (make-game (cons (make-invader 41.5 -1.5 1) empty) empty (make-tank 152 1)))
(check-random (next-game G1) (make-game (cons  (make-invader 151.5 101.5 1) empty)
                                        (cons (make-missile 150 290) empty)
                                        (make-tank 52 1)))

(define (next-game g)
  (make-game (add-advance-invaders (game-invaders g))
             (advance-missiles (game-missiles g))
             (advance-tank (game-tank g))))




;; Invaders -> Invaders

(check-random (add-advance-invaders LOI0) (cons (make-invader 41.5 -1.5 1) empty))
(check-random (add-advance-invaders LOI1)(cons (make-invader 151.5 101.5 1) empty))
(check-random (add-advance-invaders LOI2) (cons (make-invader 56.5 -1.5 1)
                                                (cons (make-invader WIDTH 151.5 -1)
                                                      (cons (make-invader 151.5 101.5 1) empty))))

(define (add-advance-invaders loi)
  (advance-invaders(add-invaders loi)))



;; Invaders -> Invaders
;(check-random (add-invaders LOI0) (cons (make-invader (random WIDTH) -3 1) LOI0))
;(check-random (add-invaders LOI1) (cons (make-invader (random WIDTH) -3 1) LOI1))
;(check-random (add-invaders LOI2) (cons (make-invader (random WIDTH) -3 1) LOI2))


(define (add-invaders loi)
  (cond [(empty? loi)
         (cons (make-invader (random WIDTH) -3 1) loi)]
        [else 
         (if (< (random INVADE-RATE) 3)
             (cons (make-invader (random WIDTH) -3 1)
                   loi)
             loi)]))









;; Invaders -> INvaders
;; lfldfkj

(check-expect (advance-invaders LOI0) empty)
(check-expect (advance-invaders LOI1) (cons (make-invader 151.5 101.5 1) empty))
(check-expect (advance-invaders LOI2) (cons (make-invader WIDTH 151.5 -1) (cons (make-invader 151.5 101.5 1) empty)))

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader(first loi)) (advance-invaders (rest loi)))]))


(check-expect (advance-invader I1) (make-invader 151.5 101.5 1))
(check-expect (advance-invader I2) (make-invader WIDTH 151.5 -1))

(define (advance-invader i)
  (cond [(right? i)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) -1)]
        [(left? i)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) 1)]
        
        [else
         (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i)))
                       (+ (invader-y i) INVADER-Y-SPEED )
                       (invader-dir i))]))
      
  
(define (right? i)
  (> (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i))) WIDTH))


(define (left? i)
  (> 0 (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i)))))

;; Missiles -> Missiles
;; lfkdjlfjdlfjdlfjd

(check-expect (advance-missiles LOM0) empty)
(check-expect (advance-missiles LOM1) (cons (make-missile 150 290) empty))
(check-expect (advance-missiles LOM2) (cons (make-missile 15 49) (cons (make-missile 150 290) empty)))

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
          (if (out-of-screen? (first lom))
              (advance-missiles (rest lom))
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (advance-missiles (rest lom))))]))


(define (out-of-screen? m)
  (< (- (missile-y m) MISSILE-SPEED) 0))


;; Tank -> Tank
;; change the position of the tank to the direction d

(check-expect (advance-tank T0) (make-tank (+ (tank-x T0) (* TANK-SPEED (tank-dir T0))) (tank-dir T0)))
(check-expect (advance-tank T1) (make-tank 52 1)) 

;(define (advance-tank T0) T1) ; stub

;; <Tank template used>

(define (advance-tank t)
  (cond [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0)
         (make-tank 0 0)]
        [(> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH)
         (make-tank WIDTH 0)]
        [else 
  (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

     




;; Game -> Image
;; produce la jdlfjdlkfj

(check-expect (render-game G0) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-game G1) (place-image TANK 50 (- 500 TANK-HEIGHT/2)
                                            (place-image INVADER 150 100 
                                                         (place-image MISSILE 150 300 BACKGROUND))))

(define (render-game g)
  (place-image TANK (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)
               (render-invaders (game-invaders g) (render-missiles (game-missiles g)))))





;; ListOfInvaders -> Image
;; fkljdlfjs

(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (cons (make-invader 150 100 1) empty) BACKGROUND) (place-image INVADER 150 100 BACKGROUND))


(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi) img))]))

;; ListOfMissile -> Image
;; produce of Tank in its current position

(check-expect (render-missiles LOM0) BACKGROUND)
(check-expect (render-missiles LOM1) (place-image MISSILE 150 100 BACKGROUND))

(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom)))]))

