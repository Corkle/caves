(ns caves.core-test
  (:require [caves.core])
  (:import [caves.core UI Game])
  (:use clojure.test
        caves.core))

(defn current-ui [game]
  (:kind (last (:uis game))))

(deftest test-start
  (let [game (->Game nil [(->UI :start)] nil)]
  
    (testing "Any key will continue to play screen."
      (let [results (map (partial process-input game) [\f \space :escape :enter :backspace])]
        (doseq [result results]
          (is (= (current-ui result) :play)))))))

(deftest test-play
  (let [game (->Game nil [(->UI :play)] nil)]
  
    (testing "Enter key wins at the play screen."
      (let [result (process-input game :enter)]
        (is (= (current-ui result) :win))))
    
    (testing "Backspace key lose at the play screen."
      (let [result (process-input game :backspace)]
        (is (= (current-ui result) :lose))))
    
    (testing "S key will stay at play screen after smoothing world"
      (let [result (process-input game \s)]
        (is (= (current-ui result) :play))))))