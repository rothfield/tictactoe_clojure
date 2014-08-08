(ns tictactoe-clojure.core)

;; Port to clojure (with a few changes) of lisp code I found at
;; http://www-users.cs.umn.edu/~gini/aiprog/rich-knight/tictactoe.lisp
;; and 
;; http://www-users.cs.umn.edu/~gini/aiprog/rich-knight/minimax.lisp
;; Reference: Artificial Intelligence, Second Edition
;; Elaine Rich and Kevin Knight
;; McGraw Hill, 1991
;;



(defn is-position?[x]
  (set? x))

(defn is-player?[z]
  (contains? #{:o :x} z))

(defn opposite-player[player]
  {:pre [(is-player? player)]}
  (get {:o :x :x :o} player))

(def starting-board  (sorted-set))


(defn moves[pos player]
  {:pre [(is-player? player)]
   :post [(seq? %)]}
 (map (fn[my-num] (vector my-num player))
 (clojure.set/difference  (set (drop 1 (range 10))) (set (map first pos))))
  )

 (defn make-move [pos mv]                     
  (conj pos mv)) 

(def win-table
  [ [1 2 3] [4 5 6] [7 8 9]
            [1 4 7] [2 5 8] [3 6 9]
            [1 5 9] [3 5 7]])

(defn wins-for[which]
  (map (fn[[x y z]] 
         (set [[x which] [y which] [ z which]]))
              win-table))
   
(def wins-for-x (wins-for :x))  
(def wins-for-o (wins-for :o))  


(defn won?[pos which]
  (some (fn[my-win] (empty? (clojure.set/difference my-win pos)) )
           (get {:x wins-for-x
                 :o wins-for-o} which)
  ))

(defn drawn?[pos]
  (and (= 9 (count pos))
       (not (won? pos :x))
       (not (won? pos :o))))


;;(println (make-move starting-board [ 1 :x)))
;;
;; (-> starting-board (make-move :x 1) (make-move :o 2) print-board)
(def x-wins 
      (reduce (fn[accum mv] (make-move accum mv)) starting-board
         [[1 :x] [7 :o]]))
(def x-wins-immediately
      (reduce (fn[accum mv] (make-move accum mv)) starting-board
         [[1 :x] [7 :o][2 :x] [4 :y]]))
;;
 (def winning-pos
      (reduce (fn[accum mv] (make-move accum mv)) starting-board
         [[1 :x] [2 :x] [4 :o][5 :o]]))
;; (println losing-pos-o-to-move)
;; (println winning-pos)
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; Function static evaluates a position from the point of view of a 
;; particular player.  It returns a number -- the higher the number, the
;; more desirable the position.  The simplest static function would be:
;;
(defn static[pos player]
      (cond (won? pos player) 
            1
            (won? pos (opposite-player player)) 
            -1
            true
            0))

(defn is-score?[x]
  (number? x))

(defn is-path?[x]
  (or (nil? x)
      (seq? x)))

(def player->string
  {:x "X" :o "O" })

(defn board->string[pos]
    (apply str (map (fn print-board-aux[i]
         (str
         (cond 
           (contains? pos [i :x])
               "X"
           (contains? pos [i :o])
               "O"
           true
           ".")
           (when (zero? (mod i 3))
            (str (get { 1 "123"
                        2 "456"
                        3 "789" 
                       } (/ i 3)) "\n")))) 
  (rest (range 0 10)))))
  
(defn print-board
  ([pos]
    (println (board->string pos))
)
  ([pos player]
  (println (str 
             (get player->string player) 
                " to move\n\n"
            (board->string pos)
             "\n\n"))))

;; (print-board starting-board :x)

  
(defn minimax[pos depth player]
  {:pre [
        (is-position? pos) 
         (number? depth)
         (is-player? player)
         ]
  :post [ (= 2 (count %))
          (is-score? (first %))] 
   }
  (cond 
         (or (zero? depth)
             (won? pos :x)
             (won? pos :o)
             (drawn? pos))
         [(static pos player) nil]
        true
          (loop [moves (moves pos player)
                 best-score -99999
                 best-path nil]
            (if (empty? moves)
              [best-score best-path]              
              (let [
                    move (first moves) 
                    successor (make-move pos move)
                    ;; destructure
                    [zz-new-value new-path] (minimax successor (dec depth)
                                         (opposite-player player))
                    new-value (- zz-new-value)
                    ]
               (cond 
                (> new-value best-score)
                (recur (rest moves)
                       new-value
                       (conj new-path move)) ;; add mood to path returned by minimax call above
                 true
                  (recur (rest moves)
                               best-score
                               best-path)
                
                ))
        ))))

(defn run-minimax[pos depth player]
  (print-board pos player)
  (println (minimax pos depth player)))

;;(println (minimax starting-board 9 :x))
;; (print-board losing-pos-o-to-move)
;;
;; (println (minimax losing-pos-o-to-move 4 :o))
;;(run-minimax x-wins 9 :x)
;;->  [1 ([2 :x] [3 :o] [5 :x] [4 :o] [6 :x] [8 :o] [9 :x])]
;;
;; (display x-wins)
;;
;;(println (minimax x-wins 3 :o))
;;(println (minimax x-wins-immediately 3 :x))
;; (print-board x-wins :x)
;; (print-board winning-pos)
 ; (use 'clojure.stacktrace)
 ; (print-stack-trace *e)

