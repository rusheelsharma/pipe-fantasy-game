;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pipe-fantasy-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Pipe-dream: PART 2 Official Solution v1.1
;; NOTE if using this solution for part 3:
;; - Do not delete lines that starts with ;;! when editing this solution.
;; - If this solution needs to be modified, we will make
;;   an announcement on Piazza in the post titled "Homework 11 Clarifications".
;;   So check Piazza often!
;; - Some tests are omitted from this file. You do not have to fill the tests in
;;   if you are just using these functions elsewhere in your code. However, you have to write tests for all
;;   the functions you modify, excluding the ones that returns an image.

;; Complete Task 7 here, after you have finished all other tasks.
;;

#;(define TASK-7-GAMESTATE (make-gamestate
 (make-grid
  6
  (list
   (list 1 1 (make-pipe #false #false #false #true))
   (list 1 2 (make-pipe #false #true #false #true))
   (list 1 3 (make-pipe #true #true #true #true))
   (list 1 4 (make-pipe #true #false #false #true))
   (list 2 1 (make-pipe #false #true #true #false))
   (list 2 2 (make-pipe #true #true #true #true))
   (list 2 3 (make-pipe #true #true #false #false))
   (list 2 4 (make-pipe #true #false #true #false))
   (list 3 2 (make-pipe #false #false #true #true))
   (list 4 2 (make-pipe #true #false #true #false))))
 (list #f (list 1 1 "right"))
 INCOMING-PPS
 100
 30
 5
 19))


(define TILE-SIZE 100)
(define PIPE-WIDTH 30)
(define GRID-DIM 6)

;; A pipe is a (make-pipe bool bool bool bool)
(define-struct pipe (top bottom left right))
;; representation of a pipe tile.
;; top, bot, left, right
;; represents if the corresponding direction is open for flow.
;; Any combination of trueness for top, bot, left, right are allowed except
;; when three of them are true.
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TB (make-pipe #true #true #false #false))

(define START-PIPES
  (map
   (lambda (pipe-dirs) (apply make-pipe pipe-dirs))
   (list
    (list #true #false #false #false)
    (list #false #true #false #false)
    (list #false #false #true #false)
    (list #false #false #false #true))))

;; Double checked in the original game and found out that there is no T shaped pipes.
(define PASS-THROUGH-PIPES
  (map
   (lambda (pipe-dirs) (apply make-pipe pipe-dirs))
   (list
    (list #true #true #false #false)
    (list #true #false #true #false)
    (list #true #false #false #true)
    (list #false #true #true #false)
    (list #false #true #false #true)
    (list #false #false #true #true)
    (list #true #true #true #true))))

;; Let's have all types of pipes in a list!
(define ALL-PIPES (append PASS-THROUGH-PIPES START-PIPES))

;; direction is one of:
;; - "top"
;; - "bottom"
;; - "left"
;; - "right"

;; reverse-direction: direction -> direction
;; produce the reverse direction of the given direction
(define (reverse-direction dir)
  (cond
    [(string=? dir "top") "bottom"]
    [(string=? dir "bottom") "top"]
    [(string=? dir "left") "right"]
    [(string=? dir "right") "left"]))

;; pipe-flow-into: pipe direction -> [Optional direction]
;; can a pipe allow liquid flowing into the tile, recieving the liquid from the given direction?
;; if so, produce the new direction it is going.
(define (pipe-can-flow pipe dir)
  (cond
    [(and (string=? dir "top")
          (pipe-top pipe))
     (cond
       [(pipe-bottom pipe) "bottom"]
       [(pipe-left pipe) "left"]
       [(pipe-right pipe) "right"])]
    [(and (string=? dir "bottom")
          (pipe-bottom pipe)) 
     (cond
       [(pipe-top pipe) "top"]
       [(pipe-left pipe) "left"]
       [(pipe-right pipe) "right"])]
    [(and (string=? dir "left")
          (pipe-left pipe))
     (cond
       [(pipe-right pipe) "right"]
       [(pipe-top pipe) "top"]
       [(pipe-bottom pipe) "bottom"])]
    [(and (string=? dir "right")
          (pipe-right pipe))
     (cond
       [(pipe-left pipe) "left"]
       [(pipe-top pipe) "top"]
       [(pipe-bottom pipe) "bottom"])]
    [else #false]))

(check-expect (pipe-can-flow PIPE-TL "top") "left")
(check-expect (pipe-can-flow PIPE-TL "bottom") #false)


;; pipe-image: pipe Number Number boolean [List-of Directions] -> image
;; make an image of the pipe, given size of tile and width of pipe.
;; assuming size of tile is always larger than size of pipe.
;; if the pipe is filled, the pipe is green, otherwise it is black.

(define (pipe-image pipe tile-size pipe-width filled? directions)
  (local
    [(define (pipe-image/custom-color pipe tile-size pipe-width bg-color pipe-color)
       (local
         [(define mid-left-value (quotient (- tile-size pipe-width) 2))
          (define longer-end-size (quotient (+ tile-size pipe-width) 2))
          ;; vertical-pipe: color -> image
          ;; make a vertical pipe component of the given color
          (define (vertical-pipe color)
            (rectangle pipe-width longer-end-size "solid" color))
          ;; horizontal-pipe: color -> image
          ;; make a horizontal pipe component of the given color
          (define (horizontal-pipe color)
            (rectangle longer-end-size pipe-width "solid" color))

          ;; pipe-sub-images is the list of image components of the pipe.
          ;; it will be 
          ;; (list top-image bottom-image left-image right-image)
          ;; where each image is either a colored pipe or a transparent image.
          ;; depending on if the pipe is open in that direction.

          (define pipe-sub-images
          (cond [(and (pipe-top pipe) (pipe-bottom pipe) (pipe-left pipe) (pipe-right pipe))
                 (if (and (cons? directions) (ormap (λ (dir) (or (string=? dir "top")
                                                                 (string=? (reverse-direction dir) "top"))) directions))
                     (list (vertical-pipe "black") (vertical-pipe "black") (horizontal-pipe "green") (horizontal-pipe "green"))
                     (list (vertical-pipe "green") (vertical-pipe "green") (horizontal-pipe "black") (horizontal-pipe "black")))]

                [else (map (lambda (kind pipe-fn) (kind (if (pipe-fn pipe) pipe-color "transparent")))
                 (list vertical-pipe vertical-pipe horizontal-pipe horizontal-pipe) ;; kind 
                 (list pipe-top pipe-bottom pipe-left pipe-right))]))

          (define pipe-offsets-x (list mid-left-value mid-left-value 0 mid-left-value))
          (define pipe-offsets-y (list 0 mid-left-value mid-left-value mid-left-value))]

         ;; combine the pipe sub images together.
         ;; note that how foldl, like map, can accept multiple lists.
         (foldl
          (lambda
              (x y img result-img)
            (underlay/xy result-img x y img))
          (rectangle tile-size tile-size "solid" bg-color)
          pipe-offsets-x
          pipe-offsets-y
          pipe-sub-images)))]
    (pipe-image/custom-color pipe tile-size pipe-width "grey" (if filled? "green" "black"))))



;; A grid is a (make-grid Integer (listof (list Integer Integer pipe))
(define-struct grid (dim cells))
;; dim is the dimension of the square grid.
;; cells is the representation of cells.
;; with each element is (list x-coordinate y-coordinate pipe-representation)
;; elements in the list must be sorted by the order of x-coor and y-coor
;; and (x-coor, y-coor) is unique in cells.
;; length of cells must be less than (dim * dim)

(define STARTING-GRID (make-grid GRID-DIM empty))

;; Note: The following implementation for place-pipe and pipe-at is more than what is needed:
;; it makes sure that pipes in grid-cells are in order of (row, col), with row being the primary key.

;; place-pipe: grid pipe Integer Integer -> grid
;; produce the grid after placing pipe on it at grid location (grid-x, grid-y)
(define (place-pipe grid pipe grid-x grid-y)
  (local
    [(define (insert-pipe-at-pos cells)
       (cond
         [(empty? cells) (list (list grid-x grid-y pipe))]
         [(and (= grid-x (first (first cells)))
               (= grid-y (second (first cells))))
          (cons (list grid-x grid-y pipe) (rest cells))]
         [(or (< grid-x (first (first cells)))
              (and
               (= grid-x (first (first cells)))
               (< grid-y (second (first cells)))))
          (cons (list grid-x grid-y pipe) cells)]
         [else
          (cons (first cells) (insert-pipe-at-pos (rest cells)))]))]
    (make-grid
     (grid-dim grid)
     (insert-pipe-at-pos (grid-cells grid)))))

(define SMALL-GRID1 (make-grid 2 (list (list 0 1 PIPE-TL))))
(define SMALL-GRID2 (make-grid 2 (list (list 1 0 PIPE-TB) (list 1 1 PIPE-TL))))

(check-expect (place-pipe (make-grid 2 empty) PIPE-TL 0 1) SMALL-GRID1)
(check-expect (place-pipe
               (place-pipe (make-grid 2 empty) PIPE-TL 1 1)
               PIPE-TB 1 0)
              SMALL-GRID2)
(check-expect (place-pipe
               (place-pipe (make-grid 2 empty) PIPE-TB 1 0)
               PIPE-TL 1 1)
              SMALL-GRID2)

;; pipe-at: grid Integer Integer -> grid
;; get the pipe at the grid location (x, y)
(define (pipe-at grid x y)
  (local
    [(define (find-pipe-at-pos cells)
       (cond
         [(empty? cells) #false]
         [(and (= x (first (first cells)))
               (= y (second (first cells))))
          (third (first cells))]
         [(or (< x (first (first cells)))
              (and
               (= x (first (first cells)))
               (< y (second (first cells)))))
          #false]
         [else
          (find-pipe-at-pos (rest cells))]))]
    (find-pipe-at-pos (grid-cells grid))))

;; tests
(check-expect (pipe-at SMALL-GRID1 0 1) PIPE-TL)
(check-expect (pipe-at SMALL-GRID1 1 1) #false)
(check-expect (pipe-at SMALL-GRID1 0 0) #false)

;; Goo logic

;; GooFlow is [ne-list-of [any-of #false (list Integer Integer direction)]]
;; the history of location of goo in the maps, and the direction it is flowing.
;; ordered from the most recent goo propagation location to the least recent.
;; That is, (first GooFlow) will be the most recent goo location.
;; #false can only appear in front of the list, means the goo have already spilled
;; and propagate any further.
;; (This design is to facilitate faster goo propagation ending checking for part 3)


;; my-member?: (list Integer Integer) [list-of (list Integer Integer)] -> boolean
;; produce true if maybe-elem is in lox
(define (my-member? maybe-elem lox)
  (and (not (empty? lox))
       (or (and (= (first maybe-elem)
                   (first (first lox)))
                (= (second maybe-elem)
                   (second (first lox))))
           (my-member? maybe-elem (rest lox)))))

;; grid-image: grid Number Number GooFlow -> image
(define (grid-image grid tile-size pipe-width goo-flow)
  ;; Make a tile
  ;; Note: tile size must be even number!
  (local
    [(define TILE (rectangle tile-size tile-size "outline" "blue"))
     (define dim (grid-dim grid))
     ;; create-row: num -> image
     ;; take a number n > 0 and returns a row of n tiles
     (define (create-row n)
       (if (= n 1)
           TILE
           (beside TILE (create-row (- n 1)))))
     ;; create-grid: row num-> image
     ;; takes a row image and duplicates it num times to create a grid
     ;; with rows and columns n > 0
     (define (create-grid row n_rows)
       (if (= n_rows 1)
           row
           (above row (create-grid row (- n_rows 1)))))
  
     (define grid-image
       (create-grid (create-row dim) dim))

     (define (place-pipe-on-grid grid-img x y pipe)
       (local
         [(define img-x (floor (* tile-size (+ x 1/2))))
          (define img-y (floor (* tile-size (+ y 1/2))))
          
          ]
         (place-image
          (pipe-image pipe
                      tile-size
                      pipe-width
                      (my-member? (list x y)
                                  (map (lambda (elem) (list (first elem) (second elem)))
                                       (if (and (not (empty? goo-flow))
                                                (false? (first goo-flow)))
                                           (rest goo-flow)
                                           goo-flow)))
                      (foldr (λ (g acc) (if
                                         (and (cons? g) (= (first g) img-x) (= (second g) img-y))
                                         (cons (third g) acc)
                                         acc))
                                         
                               '() goo-flow))
         
          img-x
          img-y
          grid-img)))
     

     (define (draw-pipes cells grid-img-so-far)
       (cond
         [(empty? cells) grid-img-so-far]
         [else
          (draw-pipes (rest cells)
                      (place-pipe-on-grid grid-img-so-far
                                          (first (first cells))
                                          (second (first cells))
                                          (third (first cells))))]))]
    (draw-pipes (grid-cells grid) grid-image)))

;; A test for grid-image.
(grid-image (foldl
             (lambda
                 (pipe-locs pipe base)
               (place-pipe base
                           pipe
                           (first pipe-locs)
                           (second pipe-locs)))
             (make-grid GRID-DIM empty)
             (build-list (length ALL-PIPES)
                         (lambda (x) (list
                                      (remainder x GRID-DIM)
                                      (quotient x GRID-DIM))))
             ALL-PIPES)
            TILE-SIZE
            PIPE-WIDTH
            empty)

;; grid-goo-propagate: Grid GooFlow -> GooFlow
(define (grid-goo-propagate grid goo-flow)
  (if (false? (first goo-flow))
      goo-flow
      (local
        [(define curr-goo-flow (first goo-flow))
         (define curr-dir (third curr-goo-flow))
         (define candidate-x
           (cond
             [(string=? curr-dir "left") (sub1 (first curr-goo-flow))]
             [(string=? curr-dir "right") (add1 (first curr-goo-flow))]
             [else (first curr-goo-flow)]))
         (define candidate-y
           (cond
             [(string=? curr-dir "top") (sub1 (second curr-goo-flow))]
             [(string=? curr-dir "bottom") (add1 (second curr-goo-flow))]
             [else (second curr-goo-flow)]))
         (define candidate-pipe (pipe-at grid candidate-x candidate-y))]
        (if (false? candidate-pipe)
            (cons #false goo-flow)
            (local
              ;; In the perspective of the pipe, a flow going top is comming from the bottom.
              ;; so we need to reverse the direction when checking if the goo can flow into the pipe. 
              [(define new-dir (pipe-can-flow candidate-pipe (reverse-direction curr-dir)))]
              (if (false? new-dir)
                  (cons #false goo-flow)
                  (cons (list candidate-x candidate-y new-dir) goo-flow)))))))

(define-struct gamestate [grid goo-flow incoming-pipes tile-size pipe-width num-replaced time-counter])
;; A gamestate is
;; (make-gamestate grid GooFlow (list-of pipe) Integer Integer Integer Integer Integer)
;; grid: the logical grid representation (i.e. the grid struct).
;; goo-flow: the locations of goo.
;; incoming-pipes: stack of next pipe styles to put down.
;; tile-size: size of the tile in the interface.
;; pipe-width: width of the pipe in the interface.
;; num-replaced: number of pipes user has replaced
;; time-counter: counts the time until the next step of goo propagation

; get-score: GameState -> Integer
; (* 50 (- length-path num-replaced))
; length of the gooflow minus 1 : length-path

(define EXAMPLE-GS-1 (make-gamestate
 (make-grid
  6
  (list
   (list 1 1 (make-pipe #false #false #false #true))
   (list 1 2 (make-pipe #false #true #false #true))
   (list 1 3 (make-pipe #true #false #false #true))
   (list 1 4 (make-pipe #false #false #true #true))
   (list 2 1 (make-pipe #false #true #true #false))
   (list 2 2 (make-pipe #true #false #true #false))
   (list 2 3 (make-pipe #false #true #true #false))
   (list 2 4 (make-pipe #true #true #true #true))))
 (list #false (list 2 4 "bottom") (list 2 3 "bottom") (list 1 3 "right") (list 1 2 "bottom") (list 2 2 "left") (list 2 1 "bottom") (list 1 1 "right"))
 '()
 100
 30
 0
 26))

(check-expect (get-score EXAMPLE-GS-1) 300)
(check-expect (get-score TASK-7-GAMESTATE) 0)
(check-expect (get-score (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH DOUBLE-PASS-THROUGH-PIPES)) 0)

(define (get-score gs)
  (local [(define (length-of-goo-flow gf)
            (cond
              [(boolean? (first gf)) (- (length gf) 2)]
              [(cons? (first gf)) (- (length gf) 1)]))]
    (* 50 (- (length-of-goo-flow(gamestate-goo-flow gs)) (gamestate-num-replaced gs))))) 

;; draw-grid: gamestate -> image
(define (draw-grid gs)
  (grid-image (gamestate-grid gs)
              (gamestate-tile-size gs)
              (gamestate-pipe-width gs)
              (gamestate-goo-flow gs)))

;; take-first-n-fill-blanks: (listof Any) Integer Any -> (listof Any)
;; take the first n elements of the list.
;; if list is empty, fill the blanks with the item.
(define (take-first-n-fill-blanks lst n item)
  (cond
    [(or (empty? lst) (= n 0)) (make-list n item)]
    [else (cons (first lst) (take-first-n-fill-blanks (rest lst) (sub1 n) item))]))
  
;; draw-interface: gamestate -> image
;; draw the interface of the game, including 
;; the grid, and on its right, the first (grid dim - 1) elements of pipe stack.
(define (draw-interface gs)
  (local
    [(define incoming-pipes (gamestate-incoming-pipes gs))
     (define pipe-width (gamestate-pipe-width gs))
     (define tile-size (gamestate-tile-size gs))
     
     (define incoming-pipes-img
       (above
        (text "NEXT\nPIPE" 40 "grey")
        (foldr
         (lambda (pipe img)
           (above (pipe-image pipe tile-size pipe-width #false #false)
                  img))
         empty-image
         (take-first-n-fill-blanks incoming-pipes
                                   (- (grid-dim (gamestate-grid gs)) 1)
                                   ;; a special pipe with no opening is needed to draw an empty tile
                                   ;; with just the pipe background
                                   (make-pipe #false #false #false #false)))))

     (define display-score-img
        (text (number->string(get-score gs)) 60 "indigo"))

     ]
    (above (beside (draw-grid gs) incoming-pipes-img) display-score-img)))

;HW11: TASK 5 and 6
;;hnd-on-tick: GameState -> GameState
;;Automatically propogates the goo after a given time

(check-expect (hnd-on-tick (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH DOUBLE-PASS-THROUGH-PIPES)) (make-gamestate
 (make-grid
  6
  (list
   (list
    1
    1
    (make-pipe
     #false
     #false
     #false
     #true))))
 (list (list 1 1 "right"))
 (list
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #true #false #false)
  (make-pipe #true #false #true #false)
  (make-pipe #true #false #false #true)
  (make-pipe #false #true #true #false)
  (make-pipe #false #true #false #true)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true))
 100
 30
 0
 139))
(check-expect (hnd-on-tick EXAMPLE-GS-1) (make-gamestate
 (make-grid
  6
  (list
   (list
    1
    1
    (make-pipe
     #false
     #false
     #false
     #true))
   (list
    1
    2
    (make-pipe
     #false
     #true
     #false
     #true))
   (list
    1
    3
    (make-pipe
     #true
     #false
     #false
     #true))
   (list
    1
    4
    (make-pipe
     #false
     #false
     #true
     #true))
   (list
    2
    1
    (make-pipe
     #false
     #true
     #true
     #false))
   (list
    2
    2
    (make-pipe
     #true
     #false
     #true
     #false))
   (list
    2
    3
    (make-pipe
     #false
     #true
     #true
     #false))
   (list
    2
    4
    (make-pipe
     #true
     #true
     #true
     #true))))
 (list
  #false
  (list 2 4 "bottom")
  (list 2 3 "bottom")
  (list 1 3 "right")
  (list 1 2 "bottom")
  (list 2 2 "left")
  (list 2 1 "bottom")
  (list 1 1 "right"))
 '()
 100
 30
 0
 25))
(check-expect (hnd-on-tick TASK-7-GAMESTATE) (make-gamestate
 (make-grid
  6
  (list
   (list
    1
    1
    (make-pipe
     #false
     #false
     #false
     #true))
   (list
    1
    2
    (make-pipe
     #false
     #true
     #false
     #true))
   (list
    1
    3
    (make-pipe #true #true #true #true))
   (list
    1
    4
    (make-pipe
     #true
     #false
     #false
     #true))
   (list
    2
    1
    (make-pipe
     #false
     #true
     #true
     #false))
   (list
    2
    2
    (make-pipe #true #true #true #true))
   (list
    2
    3
    (make-pipe
     #true
     #true
     #false
     #false))
   (list
    2
    4
    (make-pipe
     #true
     #false
     #true
     #false))
   (list
    3
    2
    (make-pipe
     #false
     #false
     #true
     #true))
   (list
    4
    2
    (make-pipe
     #true
     #false
     #true
     #false))))
 (list #false (list 1 1 "right"))
 (list
  (make-pipe #false #true #false #true)
  (make-pipe #true #true #false #false)
  (make-pipe #false #false #true #true)
  (make-pipe #true #true #true #true)
  (make-pipe #true #false #true #false))
 100
 30
 0
 139))

(define (hnd-on-tick gs)
  (cond
    [(> (gamestate-time-counter gs) 0)
     (make-gamestate (gamestate-grid gs)
                     (gamestate-goo-flow gs)
                     (gamestate-incoming-pipes gs)
                     (gamestate-tile-size gs)
                     (gamestate-pipe-width gs)
                     (gamestate-num-replaced gs)
                     (- (gamestate-time-counter gs) 1))]
    [(= (gamestate-time-counter gs) 0)
     (if (= (length (gamestate-goo-flow gs))
            (length (grid-goo-propagate (gamestate-grid gs)
                                        (gamestate-goo-flow gs)
                                        )))
     (make-gamestate (gamestate-grid gs)
                     (grid-goo-propagate (gamestate-grid gs) (gamestate-goo-flow gs))
                     (gamestate-incoming-pipes gs)
                     (gamestate-tile-size gs)
                     (gamestate-pipe-width gs)
                     (gamestate-num-replaced gs)
                     -1)
     (make-gamestate (gamestate-grid gs)
                     (grid-goo-propagate (gamestate-grid gs) (gamestate-goo-flow gs))
                     (gamestate-incoming-pipes gs)
                     (gamestate-tile-size gs)
                     (gamestate-pipe-width gs)
                     (gamestate-num-replaced gs)
                     28))]
    [else gs]))

;; gamestate-propagate-goo: gamestate -> gamestate
(define (gamestate-propagate-goo gs)
  (make-gamestate (gamestate-grid gs)
                  (grid-goo-propagate
                   (gamestate-grid gs)
                   (gamestate-goo-flow gs))
                  (gamestate-incoming-pipes gs)
                  (gamestate-tile-size gs)
                  (gamestate-pipe-width gs)
                  (gamestate-num-replaced gs)
                  (gamestate-time-counter gs)))

;; image-coor->grid-coor: Integer Integer -> Integer
;; produce the grid coordiate for the component, given the image coordiate and tile-size
(define (image-coor->grid-coor x-image tile-size)
  (quotient x-image tile-size))

        
;; gamestate-place-pipe-on-click: gamestate int int event -> gamestate
;; handler for mouse event. Takes gs and mouse (i.e. image) coordinates (x, y)
;; and places whatever pipe is currently on the the pipe stack correctly onto
;; grid at mouse point. Updates gamestate accordingly.
(define (gamestate-place-pipe-on-click gs x y event)
  (local
    [(define incoming-pipes (gamestate-incoming-pipes gs))
     (define griddy (gamestate-grid gs))
     (define tile-size (gamestate-tile-size gs))
     (define grid-x (image-coor->grid-coor x tile-size))
     (define grid-y (image-coor->grid-coor y tile-size))
     (define dim (grid-dim griddy))
     (define replaced-pipe? (my-member? (list grid-x grid-y)
                                  (map (lambda (elem) (list (first elem) (second elem)))
                                        (grid-cells griddy))))
     ]
    (cond
      [(and (string=? "button-down" event)
            (not (empty? incoming-pipes))
            (not (ormap (λ (g) (and (cons? g) (= grid-x (first g)) (= grid-y (second g)))) (gamestate-goo-flow gs)))
            (< grid-x dim)
            (< grid-y dim)
            (not (> 0 (gamestate-time-counter gs))))
       (make-gamestate (place-pipe griddy
                                   (first incoming-pipes)
                                   grid-x
                                   grid-y)
                       (gamestate-goo-flow gs)
                       (rest incoming-pipes)
                       (gamestate-tile-size gs)
                       (gamestate-pipe-width gs)
                       (if replaced-pipe?
                           (+ 1 (gamestate-num-replaced gs))
                           (gamestate-num-replaced gs))
                       (gamestate-time-counter gs))]
      ;; incoming-pipes is empty. one click will propagate a tile of goo.
      [(and (string=? "button-down" event)
            (empty? incoming-pipes))
       (gamestate-propagate-goo gs)]
      [else gs])))

;; gamestate-init : Integer Integer direction (any-of Integer pipes) -> gamestate
;; initialize a game state given:
;; grid-dim: the dimension of the square grid
;; start-x, start-y: the starting location for the starting pipe
;; start-dir: the starting direction for the starting pipe
;; tile-size: the size of the tile in the interface
;; pipe-width: the width of the pipe in the interface
;; incoming-pipes: the list of incoming pipes
(define (gamestate-init grid-dim start-x start-y start-dir tile-size pipe-width incoming-pipes)
  (local
    [(define start-pipe 
       (make-pipe (string=? start-dir "top")
                  (string=? start-dir "bottom")
                  (string=? start-dir "left")
                  (string=? start-dir "right")))
     (define start-grid (place-pipe (make-grid grid-dim empty)
                                    start-pipe
                                    start-x
                                    start-y))]
    (make-gamestate start-grid
                    (list (list start-x start-y start-dir))
                    incoming-pipes
                    tile-size
                    pipe-width
                    0
                    140)))

(define DOUBLE-PASS-THROUGH-PIPES
  (append PASS-THROUGH-PIPES PASS-THROUGH-PIPES))

(define (pipe-fantasy gs)
  (big-bang gs
    [to-draw draw-interface]
    [on-mouse gamestate-place-pipe-on-click]
    [on-tick hnd-on-tick]))

(define INCOMING-PPS
   (map
   (lambda (pipe-dirs) (apply make-pipe pipe-dirs))
   (list
    ; t b l r 
    (list #false #true #false #true)
    (list #true #true #false #false)
    (list #false #false #true #true)
    (list #true #true #true #true)
    (list #true #false #true #false))))

;Answer to TASK 7. It's provided at the start of the file as well but commented out. 
(define TASK-7-GAMESTATE (make-gamestate
 (make-grid
  6 (list
   (list 1 1 (make-pipe #false #false #false #true))
   (list 1 2 (make-pipe #false #true #false #true))
   (list 1 3 (make-pipe #true #true #true #true))
   (list 1 4 (make-pipe #true #false #false #true))
   (list 2 1 (make-pipe #false #true #true #false))
   (list 2 2 (make-pipe #true #true #true #true))
   (list 2 3 (make-pipe #true #true #false #false))
   (list 2 4 (make-pipe #true #false #true #false))
   (list 3 2 (make-pipe #false #false #true #true))
   (list 4 2 (make-pipe #true #false #true #false))))
 (list #f (list 1 1 "right"))
 INCOMING-PPS
 100
 30
 0
 140))


#;(pipe-fantasy (gamestate-init GRID-DIM 1 1 "right" TILE-SIZE PIPE-WIDTH DOUBLE-PASS-THROUGH-PIPES))