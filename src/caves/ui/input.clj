(ns caves.ui.input
  (:use [caves.world :only [random-world smooth-world]]
        [caves.entities.player :only [move-player make-player]]
        [caves.ui.core :only [->UI]])
  (:require [lanterna.screen :as s]))



(defn reset-game [game]
  (let [fresh-world (random-world)]
    (-> game
      (assoc :world fresh-world)
      (assoc-in [:world :entities :player] (make-player fresh-world))
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
        \o         (assoc game :uis [])
        
        \r (update-in game [:world] smooth-world)
        
        \w (update-in game [:world] move-player :n)
        \a (update-in game [:world] move-player :w)
        \s (update-in game [:world] move-player :s)
        \d (update-in game [:world] move-player :e)        

        \q (update-in game [:world] move-player :nw)
        \e (update-in game [:world] move-player :ne)
        \z (update-in game [:world] move-player :sw)
        \x (update-in game [:world] move-player :se)   
        
        game))

(defn get-input [game screen]
    (assoc game :input (s/get-key-blocking screen)))