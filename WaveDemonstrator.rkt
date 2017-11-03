;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname WaveDemonstrator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Created by: James Yoo
;; Date: 2016-12-18
;; Simple wave simulation program

;; CONSTANTS
;; =========
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 1000)
(define HEIGHT 618)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define SCENE-CLR "white")

(define MTS (empty-scene WIDTH HEIGHT SCENE-CLR))

(define R-PLUS 5)
(define LOOP-CLR "blue")

;; DATA DEFINITIONS
;; ================

(define-struct loop(x y r))
;; Loop is (make-loop Natural Natural Integer)
;; interp. first field is the loop's x-posn
;;         second field is the loop's y-posn
;;         third field is the loop's radius

(define L0 (make-loop CTR-X CTR-Y 0))
(define L1 (make-loop WIDTH HEIGHT 100))
(define L2 (make-loop 343 232 44))

#;
(define (fn-for-loop lp)
  (... (loop-x lp)
       (loop-y lp)
       (loop-r lp)))

;; ListOfLoop is one of:
;; - empty
;; - (cons Loop ListOfLoop)
;; interp. an arbitrarily-sized list of loop(s)

(define LOL0 empty)
(define LOL1 (list L0))
(define LOL2 (list L0 L1))
(define LOL3 (list L0 L1 L2))

#;
(define (fn-for-lofl lol)
  (cond [(empty? lol) (...)]
        [else
         (... (fn-for-loop (first lol))
              (fn-for-lofl (rest lol)))]))

;; FUNCTIONS
;; =========

;; ListOfLoop -> ListOfLoop
;; start world with empty canvas
;;
(define (main ws)
  (big-bang ws                      ; ListOfLoop
            (on-tick update-lofl)   ; ListOfLoop -> ListOfLoop
            (to-draw draw-lofl)     ; ListOfLoop -> Image
            (on-mouse add-loop)))   ; ListOfLoop Integer Integer ME -> ListOfLoop

;; ListOfLoop -> ListOfLoop
;; consumes lofl, updates each loop with R-PLUS added to r-field
(check-expect (update-lofl LOL0) empty)
(check-expect (update-lofl LOL1)
              (list (make-loop (loop-x L0) (loop-y L0) (+ (loop-r L0) R-PLUS))))
(check-expect (update-lofl LOL2)
              (list (make-loop (loop-x L0) (loop-y L0) (+ R-PLUS (loop-r L0)))
                    (make-loop (loop-x L1) (loop-y L1) (+ R-PLUS (loop-r L1)))))

; (define (update-lofl lofl) empty) ; stub

(define (update-lofl lol)
  (map update-one lol))
           

;; Loop -> Loop
;; consumes single loop, adds R-PLUS to r-field
(check-expect (update-one L0) (make-loop (loop-x L0) (loop-y L0) (+ R-PLUS (loop-r L0))))
(check-expect (update-one L1) (make-loop (loop-x L1) (loop-y L1) (+ R-PLUS (loop-r L1))))

; (define (update-one lp) 0) ; stub

(define (update-one lp)
  (make-loop (loop-x lp) (loop-y lp) (+ R-PLUS (loop-r lp))))
              

;; ListOfLoop -> Image
;; takes lofl, produces MTS with all lofl on it
(check-expect (draw-lofl LOL0) MTS)
(check-expect (draw-lofl LOL2) (place-image (circle (loop-r L0) "outline" LOOP-CLR) (loop-x L0) (loop-y L0)
                                            (place-image (circle (loop-r L1) "outline" LOOP-CLR)
                                                         (loop-x L1) (loop-y L1)
                                                         MTS)))

; (define (draw-lofl lofl) empty-image) ; stub

(define (draw-lofl lol)
  (foldl draw-loop MTS lol))

;; Loop Image -> Image
;; consumes a loop and image, puts the loop on the given image
(check-expect (draw-loop L1 MTS) (place-image (circle (loop-r L1) "outline" LOOP-CLR)
                                              (loop-x L1) (loop-y L1) MTS))

; (define (draw-loop lp img) empty-image) ; stub

(define (draw-loop lp img)
  (place-image (circle (loop-r lp) "outline" LOOP-CLR) (loop-x lp) (loop-y lp) img))

;; ListOfLoop Integer Integer ME -> ListOfLoop
(define (add-loop lofl x y me)
  (cond [(or (mouse=?  me "button-down") (mouse=? me "drag")) (cons (make-loop x y 0) lofl)]
        [else lofl]))

;; (main empty) ; Uncomment this line to run the world program



