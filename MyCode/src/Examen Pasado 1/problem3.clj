;==========================================================
; A01373670 Rodrigo Garcia Lopez.
;==========================================================

(use 'clojure.test)

;==========================================================
(defn binary->decimal
  "Returns the equivalent decimal value of the series
  of binary digits contained in lst."
  [lst]
  (loop [lst lst
         n 1
         res 0]
    (if (empty? lst)
      res
      (recur (take (dec (count lst)) lst) (* n 2) (+ res (* n (last lst)))))
    )
  )

;==========================================================
(deftest test-binary->decimal
  (is (= 0
         (binary->decimal ())))
  (is (= 1
         (binary->decimal '(1))))
  (is (= 2
         (binary->decimal '(1 0))))
  (is (= 5
         (binary->decimal '(1 0 1))))
  (is (= 8
         (binary->decimal '(1 0 0 0))))
  (is (= 42
         (binary->decimal '(1 0 1 0 1 0))))
  (is (= 63
         (binary->decimal '(1 1 1 1 1 1))))
  (is (= 24601
         (binary->decimal '(1 1 0 0 0 0 0 0 0 0 1 1 0 0 1)))))

;==========================================================
(run-tests)
