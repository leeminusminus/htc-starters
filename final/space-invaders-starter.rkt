;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT (image-height TANK))

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-RADIUS (/ (image-height MISSILE) 2))

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



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Invaders is one of:
;;  - empty
;;  - (cons Invader Invaders)
;; interp. a list of invaders

(define LOINVADER1 empty)
(define LOINVADER2 (cons I1 empty))
(define LOINVADER3 (cons I2 (cons I3 empty)))

#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else
         (... (first loinvader)
              (fn-for-loinvader (rest loinvader)))]))

;; Missiles is one of:
;;  - empty
;;  - (cons Missile Missiles)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M2 (cons M3 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (first lom)
              (fn-for-lom (rest lom)))]))


(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define START (make-game empty empty (make-tank (/ WIDTH 2) 1)))

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



;; Functions:

;; Game -> Game
;; start the world with START
;; 
(define (main s)
  (big-bang s                          ; Game
    (on-tick   advance-game)   ; Game -> Game
    (to-draw   render-game)    ; Game -> Image
    (stop-when game-over?)     ; Game -> Boolean
    (on-key    ctrl-tank)))    ; Game KeyEvent -> Game

;; Integer -> Boolean
;; Checks if a given integer is less than or equal to the left bound of the scene
(check-expect (left-bound? 1) false)
(check-expect (left-bound? 0) false)
(check-expect (left-bound? -1) true)

;(define (left-bound? i) false) ;stub

(define (left-bound? i)
  (< i 0))

;; Integer -> Boolean
;; Checks if a given integer is greater than or equal to the right bound of the scene
(check-expect (right-bound? (- WIDTH 1)) false)
(check-expect (right-bound? WIDTH) false)
(check-expect (right-bound? (+ WIDTH 1)) true)

;(define (right-bound? i) false) ;stub

(define (right-bound? i)
  (> i WIDTH))

;; Game -> Game
;; produce the next Game
(check-expect (advance-game (make-game (list (make-invader 236 148 1.5)) (list (make-missile 100 106)) (make-tank 103 1)))
              (make-game
               (list (make-invader (+ 236 INVADER-X-SPEED) (+ 148 INVADER-Y-SPEED) 1.5))
               (list (make-missile 100 (- 106 MISSILE-SPEED)))
               (make-tank (+ 103 TANK-SPEED) 1)))

;(define (advance-game s) s) ;stub

(define (advance-game s)
  (uncollided (move-game s)))

;; Game -> Game
;; Moves all of the elements of a given game
(check-expect (move-game (make-game (list (make-invader 236 148 1.5)) (list (make-missile 100 106)) (make-tank 103 1)))
              (make-game
               (list (make-invader (+ 236 INVADER-X-SPEED) (+ 148 INVADER-Y-SPEED) 1.5))
               (list (make-missile 100 (- 106 MISSILE-SPEED)))
               (make-tank (+ 103 TANK-SPEED) 1)))

;(define (move-game s) s)

(define (move-game s)
  (make-game (next-invaders (game-invaders s))
             (next-missiles (game-missiles s))
             (next-tank (game-tank s))))

;; Invaders -> Invaders
;; Produces the next invaders

(check-random (next-invaders (list (make-invader 24 25 1.5) (make-invader 22 106 -1.5)))
              (append (rand-invader (random INVADE-RATE))
                      (next-invaders (list (make-invader 24 25 1.5) (make-invader 22 106 -1.5)))))

;(define (next-invaders loinvaders) loinvaders) ;stub

(define (next-invaders loinvaders)
  (append (rand-invader (random INVADE-RATE)) (move-invaders loinvaders)))

;; Natural -> Invaders
;; Picks a number between 0 and 99, if 0, produces a list of one new invader at a random place at the top,
;; otherwise produces empty
(check-random (rand-invader 1) empty)
(check-random (rand-invader 0) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)))

;(define (rand-invader n) empty) ;stub

(define (rand-invader n)
  (if (= n 0)
      (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))
      empty))

;; Invaders -> Invaders
;; Moves the current set of invaders
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list (make-invader 56 106 1.5)))
              (list (make-invader (+ 56 1.5) (+ 106 INVADER-Y-SPEED) 1.5)))
(check-expect (move-invaders (list (make-invader 56 106 -1.5)))
              (list (make-invader (- 56 1.5) (+ 106 INVADER-Y-SPEED) -1.5)))

;(define (move-invaders loinvader) loinvader) ;stub

(define (move-invaders loinvader)
  (cond [(empty? loinvader) loinvader]
        [else
         (cons (move-invader (first loinvader))
               (move-invaders (rest loinvader)))]))

;; Invader -> Invader
;; Moves the given invader's x and y coords.
(check-expect (move-invader (make-invader 32 41 1.5)) (make-invader (+ 32 1.5) (+ 41 INVADER-Y-SPEED) 1.5))
(check-expect (move-invader (make-invader 32 41 -1.5)) (make-invader (- 32 1.5) (+ 41 INVADER-Y-SPEED) -1.5))
(check-expect (move-invader (make-invader (+ 1 WIDTH) 41 1.5)) (make-invader WIDTH (+ 41 INVADER-Y-SPEED) -1.5))
(check-expect (move-invader (make-invader -1 41 -1.5)) (make-invader 0 (+ 41 INVADER-Y-SPEED) 1.5))

;(define (move-invader invader) invader) ;stub

(define (move-invader invader)
  (cond [(left-bound? (invader-x invader))
         (make-invader 0 (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
        [(right-bound? (invader-x invader))
         (make-invader WIDTH (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
        [else
         (make-invader
          (+ (invader-x invader) (invader-dx invader))
          (+ (invader-y invader) INVADER-Y-SPEED)
          (invader-dx invader))]))

;; Missiles -> Missiles
;; Produces the next missiles
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (list (make-missile 200 108)))
              (list (make-missile 200 (- 108 MISSILE-SPEED))))
(check-expect (next-missiles (list (make-missile 200 108) (make-missile 104 MISSILE-SPEED)))
              (list (make-missile 200 (- 108 MISSILE-SPEED)) (make-missile 104 0)))
(check-expect (next-missiles (list (make-missile 200 108) (make-missile 104 (- -1 MISSILE-RADIUS))))
              (list (make-missile 200 (- 108 MISSILE-SPEED))))

;(define (next-missiles lom) lom) ;stub

(define (next-missiles lom)
  (missiles-in-bounds (move-missiles lom)))

;; Missiles -> Missiles
;; Advances all missiles in ListOfMissile by MISSILE-SPEED
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list (make-missile 200 108)))
              (list (make-missile 200 (- 108 MISSILE-SPEED))))
(check-expect (move-missiles (list (make-missile 200 108) (make-missile 104 MISSILE-SPEED)))
              (list (make-missile 200 (- 108 MISSILE-SPEED)) (make-missile 104 0)))

;(define (move-missiles lom) lom) ;stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (move-missiles (rest lom)))]))

;; Missile -> Missile
;; Moves a missile by MISSILE-SPEED
(check-expect (move-missile (make-missile 103 97)) (make-missile 103 (- 97 MISSILE-SPEED)))

;(define (move-missile m) m) ;stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Missiles -> Missiles
;; Removes all missiles from ListOfMissile out of bounds
(check-expect (missiles-in-bounds empty) empty)
(check-expect (missiles-in-bounds (list (make-missile 200 108)))
              (list (make-missile 200 108)))
(check-expect (missiles-in-bounds (list (make-missile 200 108) (make-missile 104 (- MISSILE-RADIUS))))
              (list (make-missile 200 108) (make-missile 104 (- MISSILE-RADIUS))))
(check-expect (missiles-in-bounds (list (make-missile 200 108) (make-missile 104 (- -1 MISSILE-RADIUS))))
              (list (make-missile 200 108)))
(check-expect (missiles-in-bounds (list (make-missile 104 (- -1 MISSILE-RADIUS)) (make-missile 200 108)))
              (list (make-missile 200 108)))

;(define (missiles-in-bounds lom) lom) ;stub

(define (missiles-in-bounds lom)
  (cond [(empty? lom) empty]
        [else
         (if (in-bounds? (first lom))
             (cons (first lom) (missiles-in-bounds (rest lom)))
             (missiles-in-bounds (rest lom)))]))

;; Missile -> Boolean
;; Checks if the Missile is within bounds
(check-expect (in-bounds? (make-missile 106 203)) true)
(check-expect (in-bounds? (make-missile 106 0)) true)
(check-expect (in-bounds? (make-missile 106 (- MISSILE-RADIUS))) true)
(check-expect (in-bounds? (make-missile 106 (- -1 MISSILE-RADIUS))) false)

;(define (in-bounds? m) true) ;stub

(define (in-bounds? m)
  (>= (missile-y m) (- MISSILE-RADIUS)))

;; Tank -> Tank
;; Produces the next tank
(check-expect (next-tank (make-tank 23 1)) (make-tank (+ 23 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 23 -1)) (make-tank (- 23 TANK-SPEED) -1))
(check-expect (next-tank (make-tank (+ WIDTH 1) 1)) (make-tank WIDTH -1))
(check-expect (next-tank (make-tank -1 -1)) (make-tank 0 1))

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [(left-bound? (tank-x t)) (make-tank 0 1)]
        [(right-bound? (tank-x t)) (make-tank WIDTH -1)]
        [else (move-tank t)]))

;; Tank -> Tank
;; Move tank by TANK-SPEED pixels
(check-expect (move-tank (make-tank 45 1)) (make-tank (+ 45 TANK-SPEED) 1))
(check-expect (move-tank (make-tank 45 -1)) (make-tank (- 45 TANK-SPEED) -1))

;(define (move-tank t) t) ;stub

(define (move-tank t)
  (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t)))

;; Game -> Game
;; Removes all missiles and invaders that collide with one another from a given game
(check-expect (uncollided (make-game
                           (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5))
                           (list (make-missile 62 47) (make-missile 108 206))
                           (make-tank 102 1)))
              (make-game
               (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5))
               (list (make-missile 62 47) (make-missile 108 206))
               (make-tank 102 1)))
(check-expect (uncollided (make-game
                           (list (make-invader 62 28 1.5) (make-invader 55 92 1.5))
                           (list (make-missile 62 28) (make-missile 111 112))
                           (make-tank 108 -1)))
              (make-game
               (list (make-invader 55 92 1.5))
               (list (make-missile 111 112))
               (make-tank 108 -1)))

;(define (uncollided s) s) ;stub

(define (uncollided s)
  (make-game (uncollided-invaders (game-invaders s) (game-missiles s))
             (uncollided-missiles (game-missiles s) (game-invaders s))
             (game-tank s)))

;; Invaders Missiles -> Invaders
;; Produces a list of invaders that aren't in range of HIT-RANGE of any missiles in the given list of missiles
(check-expect (uncollided-invaders
               (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5))
               (list (make-missile 62 47) (make-missile 108 206)))
              (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5)))
(check-expect (uncollided-invaders
               (list (make-invader 62 28 1.5) (make-invader 55 92 1.5))
               (list (make-missile 62 28) (make-missile 111 112)))
              (list (make-invader 55 92 1.5)))
               
;(define (uncollided-invaders loinvaders lom) loinvaders) ;stub

(define (uncollided-invaders loinvader lom)
  (cond [(empty? loinvader) empty]
        [else
         (if (hits-missile? lom (first loinvader))
             (uncollided-invaders (rest loinvader) lom)
             (cons (first loinvader) (uncollided-invaders (rest loinvader) lom)))]))

;; Missiles Invader -> Boolean
;; Produces true if the invader is in range of HIT-RANGE of a missile, otherwise produces false
(check-expect (hits-missile?
               (list (make-missile 62 47) (make-missile 108 206))
               (make-invader 23 92 1.5))
              false)
(check-expect (hits-missile?
               (list (make-missile 62 47) (make-missile 108 206))
               (make-invader 62 47 -1.5))
              true)

;(define (hits-missile? lom invader) false) ;stub

(define (hits-missile? lom invader)
  (cond [(empty? lom) false]
        [else
         (if (in-hit-range? (first lom) invader)
             true
             (hits-missile? (rest lom) invader))]))

;; Missiles Invaders -> Missiles
;; Produces a list of missiles that aren't in range of HIT-RANGE of any invaders in the given list of invaders
(check-expect (uncollided-missiles
               (list (make-missile 62 47) (make-missile 108 206))
               (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5)))
              (list (make-missile 62 47) (make-missile 108 206)))
(check-expect (uncollided-missiles
               (list (make-missile 62 28) (make-missile 111 112))
               (list (make-invader 62 28 1.5) (make-invader 55 92 1.5)))
              (list (make-missile 111 112)))

;(define (uncollided-missiles lom loinvaders) lom) ;stub

(define (uncollided-missiles lom loinvader)
  (cond [(empty? lom) empty]
        [else
         (if (hits-invader? loinvader (first lom))
             (uncollided-missiles (rest lom) loinvader)
             (cons (first lom) (uncollided-missiles (rest lom) loinvader)))]))

;; Invaders Missile -> Boolean
;; Produces true if the missile is in range of HIT-RANGE of an invader, otherwise produces false
(check-expect (hits-invader?
               (list (make-invader 62 47 1.5) (make-invader 108 206 -1.5))
               (make-missile 23 92))
              false)
(check-expect (hits-invader?
               (list (make-invader 62 47 -1.5) (make-invader 108 206 -1.5))
               (make-missile 62 47))
              true)

;(define (hits-invader? loinvader missile) false) ;stub

(define (hits-invader? loinvader missile)
  (cond [(empty? loinvader) false]
        [else
         (if (in-hit-range? missile (first loinvader))
             true
             (hits-invader? (rest loinvader) missile))]))

;; Missile Invader -> Boolean
;; Checks if a missile and an invader are within HIT-RANGE pixels of each other
(check-expect (in-hit-range? (make-missile 22 31) (make-invader 25 34 -1.5)) true)
(check-expect (in-hit-range? (make-missile 18 12) (make-invader 24 16 -1.5)) true)
(check-expect (in-hit-range? (make-missile 30 45) (make-invader 25 34 -1.5)) false)

;(define (in-hit-range? missile invader) false) ;stub

(define (in-hit-range? missile invader)
  (<= (distance (missile-x missile) (invader-x invader) (missile-y missile) (invader-y invader))
      HIT-RANGE))

;; Integer Integer Integer Integer -> Natural
;; Finds the distance of two points using the distance formula
(check-expect (distance 22 25 38 34) 5)

;(define (distance x1 x2 y1 y2) 0) ;stub

(define (distance x1 x2 y1 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

;; Game -> Boolean
;; Checks if an invader has passed the bottom, ending the game
(check-expect (game-over? G0) false)
(check-expect (game-over? G1) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? (make-game (list I2 I3) empty T0)) true)

;(define (game-over? s) false) ;stub

(define (game-over? s)
  (invaders-bottom? (game-invaders s)))

;; Invaders -> Boolean
;; Checks if at least one invader has passed the bottom, given a list of invaders
(check-expect (invaders-bottom? LOINVADER1) false)
(check-expect (invaders-bottom? LOINVADER2) false)
(check-expect (invaders-bottom? LOINVADER3) true)

;(define (invaders-bottom? loinvader) false) ;stub

(define (invaders-bottom? loinvader)
  (cond [(empty? loinvader) false]
        [else
         (if (invader-bottom? (first loinvader))
             true
             (invaders-bottom? (rest loinvader)))]))

;; Invader -> Boolean
;; Checks if the given invader has passed the bottom
(check-expect (invader-bottom? I1) false)
(check-expect (invader-bottom? I2) false)
(check-expect (invader-bottom? I3) true)

;(define (invader-bottom? invader) false) ;stub

(define (invader-bottom? invader)
  (> (invader-y invader) HEIGHT))

;; Game KeyEvent -> Game
;; controls the tank if the space bar, left, or right arrow keys were pressed
(check-expect (ctrl-tank (make-game empty empty (make-tank 100 1)) "a")
              (make-game empty empty (make-tank 100 1)))
(check-expect (ctrl-tank (make-game empty empty (make-tank 100 1)) "right")
              (make-game empty empty (make-tank 100 1)))
(check-expect (ctrl-tank (make-game empty empty (make-tank 100 1)) "left")
              (make-game empty empty (make-tank 100 -1)))
(check-expect (ctrl-tank (make-game empty empty (make-tank 100 1)) " ")
              (make-game empty (list (make-missile 100 (- HEIGHT TANK-HEIGHT MISSILE-RADIUS))) (make-tank 100 1)))
(check-expect (ctrl-tank (make-game empty (list (make-missile 100 28)) (make-tank 126 1)) "a")
              (make-game empty (list (make-missile 100 28)) (make-tank 126 1)))
(check-expect (ctrl-tank (make-game empty (list (make-missile 100 28)) (make-tank 126 1)) " ")
              (make-game empty (list (make-missile 126 (- HEIGHT TANK-HEIGHT MISSILE-RADIUS)) (make-missile 100 28)) (make-tank 126 1)))

;(define (ctrl-tank s ke) s) ;stub


(define (ctrl-tank s ke)
  (cond [(string=? ke "right") (direct-tank s 1)]
        [(string=? ke "left") (direct-tank s -1)]
        [(string=? ke " ") (spawn-missile s)]
        [else s]))

;; Game Integer -> Game
;; Changes the direction of the tank, given a Game
(check-expect (direct-tank (make-game empty empty (make-tank 100 1)) 1)
              (make-game empty empty (make-tank 100 1)))
(check-expect (direct-tank (make-game empty empty (make-tank 100 1)) -1)
              (make-game empty empty (make-tank 100 -1)))
(check-expect (direct-tank (make-game empty empty (make-tank 100 -1)) 1)
              (make-game empty empty (make-tank 100 1)))
(check-expect (direct-tank (make-game empty empty (make-tank 100 -1)) -1)
              (make-game empty empty (make-tank 100 -1)))

;(define (direct-tank s dir) s) ;stub

(define (direct-tank s dir)
  (make-game (game-invaders s)
             (game-missiles s)
             (make-tank (tank-x (game-tank s))
                        dir)))

;; Game -> Game
;; Spawns a missile at the x and y position of tank
(check-expect (spawn-missile (make-game empty empty (make-tank 100 1)))
              (make-game empty (list (make-missile 100 (- HEIGHT TANK-HEIGHT MISSILE-RADIUS))) (make-tank 100 1)))
(check-expect (spawn-missile (make-game empty (list (make-missile 100 28)) (make-tank 126 1)))
              (make-game empty (list (make-missile 126 (- HEIGHT TANK-HEIGHT MISSILE-RADIUS)) (make-missile 100 28)) (make-tank 126 1)))

;(define (spawn-missile s) s) ;stub

(define (spawn-missile s)
  (make-game (game-invaders s)
             (add-missile (game-missiles s)
                          (tank-x (game-tank s)))
             (game-tank s)))

;; Missiles Natural -> Missiles
;; Appends a new missile at the bottom of the screen given list of missiles and an x-coordinate
(check-expect (add-missile empty 100) (list (make-missile 100 (- HEIGHT TANK-HEIGHT MISSILE-RADIUS))))
(check-expect (add-missile (list (make-missile 100 128)) 236)
              (list (make-missile 236 (- HEIGHT TANK-HEIGHT MISSILE-RADIUS)) (make-missile 100 128)))

;(define (add-missile s x) s) ;stub

(define (add-missile s x)
  (cons (make-missile x (- HEIGHT TANK-HEIGHT MISSILE-RADIUS)) s))

;; Game -> Image
;; render the current game
(check-expect (render-game (make-game
                            (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5))
                            (list (make-missile 62 47) (make-missile 108 206))
                            (make-tank 102 1)))
              (place-image INVADER 23 44
                           (place-image INVADER 42 24
                                        (place-image MISSILE 62 47
                                                     (place-image MISSILE 108 206
                                                                  (place-image TANK 102 (- HEIGHT TANK-HEIGHT/2)
                                                                               BACKGROUND))))))

;(define (render-game s) BACKGROUND) ;stub


(define (render-game s)
  (render-invaders (game-invaders s)
                   (game-missiles s)
                   (game-tank s)))

;; Invaders -> Image
;; Renders all invaders in a given list to the screen
(check-expect (render-invaders
               (list (make-invader 23 44 1.5) (make-invader 42 24 -1.5))
               (list (make-missile 62 47) (make-missile 108 206))
               (make-tank 102 1))
              (place-image INVADER 23 44
                           (place-image INVADER 42 24
                                        (place-image MISSILE 62 47
                                                     (place-image MISSILE 108 206
                                                                  (place-image TANK 102 (- HEIGHT TANK-HEIGHT/2)
                                                                               BACKGROUND))))))

;(define (render-invaders loinvaders lom t) empty-image) ;stub

(define (render-invaders loinvader lom t)
  (cond [(empty? loinvader) (render-missiles lom t)]
        [else
         (place-image INVADER
                      (invader-x (first loinvader))
                      (invader-y (first loinvader))
                      (render-invaders (rest loinvader) lom t))]))

;; Missiles -> Image
;; Renders all missiles in a given list to the screen
(check-expect (render-missiles
               (list (make-missile 62 47) (make-missile 108 206))
               (make-tank 102 1))
              (place-image MISSILE 62 47
                           (place-image MISSILE 108 206
                                        (place-image TANK 102 (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))

;(define (render-missiles lom t) empty-image) ;stub

(define (render-missiles lom t)
  (cond [(empty? lom) (render-tank t)]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) t))]))

;; Tank -> Image
;; Renders a tank to the screen
(check-expect (render-tank (make-tank 102 1))
              (place-image TANK 102 (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

;(define (render-tank t) empty-image) ;stub

(define (render-tank t)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))
