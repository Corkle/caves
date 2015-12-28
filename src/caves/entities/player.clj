(ns caves.entities.player
    (:use [caves.entities.core :only [Entity]]))

(defrecord Player [id glyph location])

(extend-type Player Entity
  (tick [this world]
    world))

(defn make-player [world]
  (->Player :player "@" (find-empty-tile world)))