(ns nimble.core
  (:gen-class))

(defn keep-trying [f err]
  (let [[result success?]
        (try
          [(f) true]
          (catch Throwable t
            nil))]
    (if success?
      result
      (do (println err)
          (recur f err)))))


(defn parse-int [n]
  (Integer/parseInt n))


(defn prompt [msg parser]
  (print msg)
  (flush)
  (parser (read-line)))


(def init-board [3 5 7])

(defn remove-pieces [board i n]
  (let [heap     (board i)
        new-heap (- heap n)]
    (assoc board i new-heap)))


(defn game-over? [board]
  (every? zero? board))


(defn get-player-move [board]
  (let [i (keep-trying #(prompt "Which heap?: "
                                parse-int)
                       "Invalid number.")]
    (if (>= i (count board))
      (do (println "Index" i "out of bounds.")
          (recur board))
      (let [n (keep-trying #(prompt "Remove how many?: "
                                    parse-int)
                           "Invalid number.")]
        (if (> n (board i))
          (do (println "Heap" i "does not have" n "pieces.")
              (recur board))
          [i n])))))


(defn run-game [board]
  (println board)
  (if (game-over? board)
    (println "Game over!")
    (let [[i n] (get-player-move board)]
      (recur (remove-pieces board i n)))))


(defn -main
  [& args]
  (println "Welcome to Nimble!")
  (run-game init-board))
