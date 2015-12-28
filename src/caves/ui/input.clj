(ns caves.ui.input
  (:use [caves.world :only [random-world smooth-world]]
        [caves.ui.core :only [->UI]])
  (:require [lanterna.screen :as s]))

; Helper Functions -------------------------------------------------------
(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn reset-game [game]
  (let [fresh-world (random-world)]
    (-> game
      (assoc :world fresh-world)
      (assoc-in [:world :player] (make-player fresh-world))
      (assoc :uis [(->UI :play)]))))

; Process User Input -----------------------------------------------------
(defmulti process-input
    (fn [game input]
        (:kind (last (:uis game)))))

(defmethod process-input :start [game input]
    (reset-game game))

(defmethod process-input :win [game input]
    (if (= input :escape)
        (assoc game :uis [])
        (assoc game :uis [(->UI :start)])))

(defmethod process-input :lose [game input]
    (if (= input :escape)
        (assoc game :uis [])
        (assoc game :uis [(->UI :start)])))

(defmethod process-input :play [game input]
    (case input
        :enter     (assoc game :uis [(->UI :win)])
        :backspace (assoc game :uis [(->UI :lose)])
        \q         (assoc game :uis [])
        
        \r (update-in game [:world] smooth-world)
        
        \w (update-in game [:location] move [0 -1])
        \a (update-in game [:location] move [-1 0])
        \s (update-in game [:location] move [0 1])
        \d (update-in game [:location] move [1 0])        

        \W (update-in game [:location] move [0 -3])
        \A (update-in game [:location] move [-3 0])
        \S (update-in game [:location] move [0 3])
        \D (update-in game [:location] move [3 0])   
        
        game))

(defn get-input [game screen]
    (assoc game :input (s/get-key-blocking screen)))