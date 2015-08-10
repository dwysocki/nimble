(ns nimble.core
  (:require [clojure.string :as string]
            [clojure.repl :refer [set-break-handler!]])
  (:gen-class))

(defn hello
  "Prints a startup message."
  []
  (println "Welcome to Nimble!"))

(defn good-bye
  "Prints an exit message."
  [signal]
  (println "\nThanks for playing!"))

(defn game-over
  "Prints a game over message.

  Once multiple players are a thing, it will take a `player` argument, and
  produce a different message based on that."
  []
  (println "Game over!"))


(defn try-until
  "Call a (non-deterministic) function repeatedly until `pred` is true."
  [pred f]
  (first (filter pred
                 (repeatedly f))))

(defn parse-int
  "Parses a string to an int."
  [n]
  (Integer/parseInt n))

(defn read-split-line
  "Reads a line from stdin, and splits it on a regular expression."
  [re]
  (string/split (read-line) re))


(def init-board [3 5 7])


(defn remove-pieces
  "Removes `count` pieces from the given `heap` on the `board`."
  [board heap count]
  (update board
          heap
          #(- % count)))


(defn game-over?
  "Return `true` if the game is over, and `false` otherwise."
  [board]
  (every? zero? board))


(defn valid-move?
  "Returns `move` if it is valid, and `nil` otherwise, while printing an error
  message describing why."
  [board [heap cnt :as move]]
  (cond
    ;; player tries to take from a non-existant heap
    (not (<= 1 heap (count board)))
    (printf "Error: There is no heap %d.\n" heap)

    ;; player tries to take zero pieces
    (zero? cnt)
    (println "Error: Must take at least one piece.")

    ;; player tries to take negative pieces
    (neg? cnt)
    (println "Error: Cannot put pieces back.")

    ;; player tries to take more pieces than in heap
    (not (<= cnt (board (dec heap))))
    (printf "Error: Heap %d does not have %d pieces left.\n" heap cnt)

    ;; player made valid move
    :else
    move))


(defn move-index-0->1
  "Convert a move's heap from index-at-zero to index-at-one."
  [[heap count :as move]]
  [(dec heap) count])


(defn valid-move-length?
  "Print an error if `move` is not a length-two seq, and return it unchanged
  otherwise."
  [move]
  (if (= 2 (count move))
    move
    (println "Error: Must be two space-separated numbers.")))


(defn parse-move-ints
  "Parse a move into a seq of ints, and print an error if that is not possible."
  [move]
  (try
    (map parse-int move)
    (catch NumberFormatException e
      (println "Error: Must be valid integers."))))


(defn player-move
  "Prompt the player for a move. Continues prompting until a valid move is
  obtained."
  [board]
  (try-until identity
    (fn []
      ;; prompt the user
      (print "Enter 'heap' 'count': ")
      (flush)
      ;; take user input, and return [heap count] if valid, and nil if not
      (some->> (read-split-line #"\s+")
               ;; error if number of inputs is not 2
               valid-move-length?
               ;; error if inputs are not ints
               parse-move-ints
               ;; error if move is invalid for the given board
               (valid-move? board)
               ;; index heap from 1 instead of 0
               move-index-0->1))))


(defn display-board
  "Displays the board.

  Currently only prints the literal representation, but may be more creative in
  the future."
  [board]
  (println board))


(defn run-game
  "Run a game."
  [board]
  ;; displays the board
  (display-board board)
  (if (game-over? board)
    ;; display a game over message
    (game-over)
    (let [;; get the heap and number of pieces to remove from the player
          [heap count] (player-move board)]
      ;; remove the pieces and continue the loop
      (recur (remove-pieces board heap count)))))


(defn -main
  [& args]
  ;; exit gracefully on Ctrl-C
  (set-break-handler! good-bye)
  ;; greet the player
  (hello)
  ;; begin the game
  (run-game init-board))
