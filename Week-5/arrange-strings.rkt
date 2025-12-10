;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname arrange-strings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; interp. a list of strings

;; Constants:

(define BLANK (square 0 "solid" "white"))
(define S1 "Apple")
(define S2 "Sally")
(define S3 "Systematic Program Design")
(define TEXT-SIZE 12)
(define TEXT-COLOR "black")

;; Data definitions

(define LOS1 empty)
(define LOS2 (cons "world" LOS1))
(define LOS3 (cons "hello" LOS2))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]  ;base case
        [else
         (... (first los)     ;string
              (fn-for-los (rest los)))]))  ;natural recursion
;; Template rules used:
;; One of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons String ListOfString)
;;  - reference: (first los) is String
;;  - self-reference: (rest los) is ListOfString

;; Functions:

;; ListOfString -> Image
;; layout strings vertically in alphabetical order
(check-expect (arrange-strings (cons "Apple" (cons "Sally" empty)))
              (above/align "left"
                           (text "Apple" TEXT-SIZE TEXT-COLOR)
                           (text "Sally" TEXT-SIZE TEXT-COLOR)
                           BLANK))
(check-expect (arrange-strings (cons "Sally" (cons "Apple" empty)))
              (above/align "left"
                           (text "Apple" TEXT-SIZE TEXT-COLOR)
                           (text "Sally" TEXT-SIZE TEXT-COLOR)
                           BLANK))

;(define (arrange-strings los) BLANK) ;stub

(define (arrange-strings los)
  (layout-strings (sort-strings los)))

;; ListofString -> Image
;; place images above each other in order of list
(check-expect (layout-strings empty) BLANK)
(check-expect (layout-strings (cons S1 (cons S2 empty)))
              (above/align "left"
                          (text S1 TEXT-SIZE TEXT-COLOR)
                          (text S2 TEXT-SIZE TEXT-COLOR)
                          BLANK))
                          
;(define (layout-strings los) BLANK)  ;stub

(define (layout-strings los)
  (cond [(empty? los) BLANK]
        [else
         (above/align "left" (text (first los) TEXT-SIZE TEXT-COLOR)
                      (layout-strings (rest los)))]))

;; ListOfString -> ListOfString
;; Sorts a list of strings alphabetically
(check-expect (sort-strings empty) empty)
(check-expect (sort-strings (cons S1 (cons S2 empty))) (cons S1 (cons S2 empty)))
(check-expect (sort-strings (cons S3 (cons S1 empty))) (cons S1 (cons S3 empty)))

;(define (sort-strings los) los) ;stub

(define (sort-strings los)
  (cond [(empty? los) empty]
        [else
         (insert-string (first los)
                        (sort-strings (rest los)))]))

;; String ListOfString -> ListOfString
;; Insert string in list of strings in alphabetical order
;; ASSUME: ListOfString is sorted
(check-expect (insert-string "Apple" empty) (cons "Apple" empty))
(check-expect (insert-string "Sally" (cons "Apple" (cons "Systematic" empty)))
              (cons "Apple" (cons "Sally" (cons "Systematic" empty))))

;(define (insert-string s los) los) ;stub

(define (insert-string str los)
  (cond [(empty? los)(cons str empty)]
        [else 
         (if (string>=? str (first los))
             (cons (first los) (insert-string str (rest los)))
             (cons str los))]))