;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ball-bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ball-bounce.rkt

;; Program of a ball bouncing around the screen

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define BALL-RADIUS 20)
(define BALL-COLOR "red")

(define BALL-IMG (circle BALL-RADIUS "solid" BALL-COLOR))

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct ball (x y dx dy))
;; Ball is (make-ball Natural[0, WIDTH] Natural[0, HEIGHT] Integer Integer)
;; interp. a ball's x-coordinate (x), y-coordinate (y), change in x per tick (dx), and change in y per tick (dy)

(make-ball 15 30 3 2)
(make-ball 14 99 -3 -2)

#;
(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-dx b)
       (ball-dy b)))
;; Template rules used:
;;  - compound: 4 fields

;; =================
;; Functions:

;; Ball -> Ball
;; start the world with (main (make-ball 100 50 6 8))
;; 
(define (main b)
  (big-bang b                          ; Ball
            (on-tick   advance-ball)   ; Ball -> Ball
            (to-draw   render-ball)    ; Ball -> Image
            (on-mouse  handle-mouse))) ; Ball Integer Integer MouseEvent -> Ball

;; Ball -> Ball
;; produce the next Ball

(check-expect (advance-ball (make-ball WIDTH HEIGHT 4 8)) (make-ball WIDTH HEIGHT -4 -8)) ; corners
(check-expect (advance-ball (make-ball 0 0 -4 -8)) (make-ball 0 0 4 8))
(check-expect (advance-ball (make-ball WIDTH 0 4 -8)) (make-ball WIDTH 0 -4 8))
(check-expect (advance-ball (make-ball 0 HEIGHT -4 8)) (make-ball 0 HEIGHT 4 -8))

(check-expect (advance-ball (make-ball WIDTH 100 4 8)) (make-ball WIDTH 108 -4 8)) ;edges
(check-expect (advance-ball (make-ball 0 100 -4 8)) (make-ball 0 108 4 8))
(check-expect (advance-ball (make-ball 100 HEIGHT -4 8)) (make-ball 96 HEIGHT -4 -8))
(check-expect (advance-ball (make-ball 100 0 -4 -8)) (make-ball 96 0 -4 8))

(check-expect (advance-ball (make-ball 100 100 -4 8)) (make-ball 96 108 -4 8)) ;middle

;(define (advance-ball b) b)

;<using Ball function template>


(define (advance-ball b)
  (cond [(and (> (+ (ball-x b) (ball-dx b)) WIDTH) (> (+ (ball-y b) (ball-dy b)) HEIGHT))
                   (make-ball WIDTH HEIGHT (- (ball-dx b)) (- (ball-dy b)))]
                  [(and (< (+ (ball-x b) (ball-dx b)) 0) (< (+ (ball-y b) (ball-dy b)) 0))
                   (make-ball 0 0 (- (ball-dx b)) (- (ball-dy b)))]
                  [(and (> (+ (ball-x b) (ball-dx b)) WIDTH) (< (+ (ball-y b) (ball-dy b)) 0))
                   (make-ball WIDTH 0 (- (ball-dx b)) (- (ball-dy b)))]
                  [(and (< (+ (ball-x b) (ball-dx b)) 0) (> (+ (ball-y b) (ball-dy b)) HEIGHT))
                   (make-ball 0 HEIGHT (- (ball-dx b)) (- (ball-dy b)))]
                  [(> (+ (ball-x b) (ball-dx b)) WIDTH)
                   (make-ball WIDTH (+ (ball-y b) (ball-dy b)) (- (ball-dx b)) (ball-dy b))]
                  [(< (+ (ball-x b) (ball-dx b)) 0)
                   (make-ball 0 (+ (ball-y b) (ball-dy b)) (- (ball-dx b)) (ball-dy b))]
                  [(> (+ (ball-y b) (ball-dy b)) HEIGHT)
                   (make-ball (+ (ball-x b) (ball-dx b)) HEIGHT (ball-dx b) (- (ball-dy b)))]
                  [(< (+ (ball-y b) (ball-dy b)) 0)
                   (make-ball (+ (ball-x b) (ball-dx b)) 0 (ball-dx b) (- (ball-dy b)))]
                  [else
                   (make-ball (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy b)) (ball-dx b) (ball-dy b))]))
                  

;; Ball -> Image
;; renders Ball at the given x and y coordinates, and moving at the correct dx and dy velocities.

(check-expect (render-ball (make-ball 30 40 2 5)) (place-image BALL-IMG 30 40 MTS))

;(define (render-ball b) MTS)

;<using Ball function template>
(define (render-ball b)
  (place-image BALL-IMG (ball-x b) (ball-y b) MTS))

;; Ball Integer Integer MouseEvent -> Ball
;; Moves ball to the mouse cursor's position when mouse is pressed

(check-expect (handle-mouse (make-ball 54 76 4 -3) 106 207 "button-down") (make-ball 106 207 4 -3))
(check-expect (handle-mouse (make-ball 54 76 4 -3) 106 207 "button-up") (make-ball 54 76 4 -3))

;(define (handle-mouse b x y me) b)

;<using MouseEvent template>

(define (handle-mouse b x y me)
  (cond [(string=? me "button-down") (make-ball x y (ball-dx b) (ball-dy b))]
        [else b]))