;==========================================================
; A01373670 Rodrigo Garcia Lopez.
;==========================================================

(use 'clojure.test)

;==========================================================
(defn suffixes
  "Returns a list with all the possible suffixes of lst."
  [lst]
  (if (empty? lst)
    (conj lst '())
    (conj (suffixes (rest lst)) lst))
  )

;==========================================================
(deftest test-suffixes
  (is (= '(())
         (suffixes ())))
  (is (= '((a) ())
         (suffixes '(a))))
  (is (= '((a b) (b) ())
         (suffixes '(a b))))
  (is (= '((a b c d) (b c d) (c d) (d) ())
         (suffixes '(a b c d))))
  (is (= '((a b c d e f g) (b c d e f g)
            (c d e f g) (d e f g)
            (e f g) (f g)
            (g) ())
         (suffixes '(a b c d e f g)))))

;==========================================================
(run-tests)
