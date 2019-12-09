;==========================================================
; Rafael Moreno Cañas | A01378916
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

;==========================================================
(defn split-number
  [n]
  (map read-string (map str (seq (str n))))
  )

(defn contains-all-digits?
  [n]
  (if (< (reduce + (distinct (sort (split-number (abs n))))) 45)
    false
    true
    )
  )
;==========================================================
(deftest test-contains-all-digits?
  (is (contains-all-digits? 1023456789))
  (is (contains-all-digits? 5897230146))
  (is (contains-all-digits? 10123485679))
  (is (contains-all-digits? 1223334444555566666677777778888888889999999990))
  (is (not (contains-all-digits? 1236)))
  (is (not (contains-all-digits? 1112223334455)))
  (is (not (contains-all-digits? -587230462413578)))
  (is (not (contains-all-digits? -122333444455556666667777777888888888999999999))))

;==========================================================
(run-tests)
