;==========================================================
; Problem Set #3
; Date: October 04, 2019.
; Authors:
;          A01378916 Rafael Moreno
;          A01379896 Erick Bautista
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

;==========================================================
(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))
;==========================================================

;1.- The function there-exists-one takes two arguments: a one argument predicate
; function pred and a list lst. Returns true if there is exactly one element in lst
; that satisfies pred, otherwise returns false.

(defn there-exists-one
  "Returns true if there is exactly one element in lst that satisfies pred,
  otherwise returns false"
  [pred lst]
  (if(empty? lst)
    false
    (some pred lst))
  )

;Unit test 1
(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

;2.- The function my-drop-while takes two arguments: a function f and a list lst.
; It returns a list of items from lst dropping the initial items that evaluate to true when
; passed to f. Once a false value is encountered, the rest of the list is returned.
; Function f should accept one argument. Do not use the predefined drop-while function.
(defn my-drop-while
  "returns a list of items from lst dropping the initial items that evaluate to true when
  passed to f. Once a false value is encountered, the rest of the list is returned."
  [fun lst]
  (if(empty? lst)
    true
    ())
  )

;Unit test 2
(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

;==========================================================
(run-tests)