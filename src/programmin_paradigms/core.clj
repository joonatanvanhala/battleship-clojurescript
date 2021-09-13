(ns programmin-paradigms.core)
; board numbers:
; 0 = empty
; 1 = hit

;define ships
(def ships [1 3])

;define boards
(def  player1Board
  [[0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]])


(def  player2Board
  [[0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]])

(defn isHitOnCell [row col shipsVector attackedPlayer]
  (def hit false)
  (doseq [x shipsVector]
    (if (and (= row (nth x 0)) (= col (nth x 1)))
      (do
        (def hit true)
        (if (= attackedPlayer 1)
          (do
            (def rowList (nth player1Board (- row 1)))
            (def newRow (assoc rowList (- col 1) 1)); Hit player 2 => Cell to 1
            (def player1Board (assoc player1Board (- row 1) newRow)))
          (do
            (def rowList (nth player2Board (- row 1)))
            (def newRow (assoc rowList (- col 1) 1)); Hit player 2 => Cell to 1
            (def player2Board (assoc player2Board (- row 1) newRow)))))))
  (if (= hit false)
    (println "Miss")
    (println "Hit")))

(defn isGameOver [playerBoard]
  (def hits 0)
  (if (= playerBoard 1)
    (do
      (doseq [rows player1Board]
        (doseq [col rows]
          (if (= col 1)
            (def hits (+ hits 1))))))
    (do
      (doseq [rows player2Board]
        (doseq [col rows]
          (if (= col 1)
            (def hits (+ hits 1)))))))
  (if (= hits 8)
    (do
      (def gameOver true)
      (println "Game over. Player " (nth [2 1] (- playerBoard 1)) " won"))))


;player ships
(def player1Ships [])
(def player2Ships [])
(def gameOver false)


(defn insertShip [size, direction, posRow, posCol, player]
  (if (= player 1)
    (if (= direction "h")
      (dotimes [i size] ; If player 1 and horizontal
        (def player1Ships (conj player1Ships [posRow (+ posCol i)])))
      (dotimes [i size] ; If player 1 and vertical
        (def player1Ships (conj player1Ships [(+ posRow i) posCol]))))
    (if (= direction "h") ; 
      (dotimes [i size]
        (def player2Ships (conj player2Ships [posRow (+ posCol i)])))
      (dotimes [i size]
        (def player2Ships (conj player2Ships [(+ posRow i) posCol]))))))

(def available-cells [1 2 3 4 5 6 7 8 9 10])

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(def ship-placed false)
(def player-turn 1)
; start application
(defn main []
  (doseq [player [1 2]]
    (doseq [s ships]
      (def ship-placed false) ; while ship not placed
      (while (= ship-placed false)
        (println "Player" player ": insert ship with size" s)
        (println  "Player" player ": insert direction for ship: h (horizontal) or v (vertical)):")
        (def direction (read-line))
        (println  "Player" player ": insert start position row (1 2 3 4 5 6 7 8 9 10) for ship with size " s)
        (def posRow (parse-int (read-line)))
        (println  "Player" player ": insert start position col (1 2 3 4 5 6 7 8 9 10) for ship with size " s)
        (def posCol (parse-int (read-line)))
      ; check if over board
        (if (or (= direction "h") (= direction "v"))
          (if (= direction "h")
            (if (and (some #{(+ posRow (- s 1))} available-cells) (some #{posCol} available-cells)); horizontal
              (do
                (def ship-placed true)
                (insertShip s direction posRow posCol player))
              (println "Cell over board, insert ship again =>"))
            (if (and (some #{posRow} available-cells) (some #{(+ posCol (- s 1))} available-cells)) ; vertical
              (do
                (def ship-placed true)
                (insertShip s direction posRow posCol player))
              (println "Cell over board, insert ship again =>")))
          (println "Not valid direction, insert ship again =>")))))

  ;print ships
  (println "Player 1 ships: " player1Ships)
  (println "Player 2 ships: " player2Ships)

  ; game loop
  (while (= gameOver false)
    (do
      (println "Player " player-turn ": attack on row (1 2 3 4 5 6 7 8 9 10)")
      (def posRow (parse-int (read-line)))
      (println "Player " player-turn ": attack on col (1 2 3 4 5 6 7 8 9 10)")
      (def posCol (parse-int (read-line)))
        ;if cell over board
      (if (and (some #{posRow} available-cells) (some #{posCol} available-cells))
        (if (= player-turn 1)
          (do ;player 1 turn 
            (isHitOnCell posRow posCol player2Ships 2)
            (isGameOver 1)
            (def player-turn 2)) ;another player's turn
          (do ;player 2 turn 
            (isHitOnCell posRow posCol player1Ships 1)
            (isGameOver 1)
            (def player-turn 1)) ;another player's turn
          )
        (println "Cell over board, choose again")))))
