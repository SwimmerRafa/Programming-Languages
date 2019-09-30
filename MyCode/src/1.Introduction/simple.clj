; Simple Clojure exercises

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [sqrt]])

;1. Write a function called f2c that takes x degrees
;Fahrenheit and converts them to degrees Celsius.
(defn f2c [f]
    (/ (* (- f 32) 5) 9))

;Unit tests
(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

;2. Write a function called sign that takes an integer value n. It returns -1 if n is
;negative, 1 if n is positive greater than zero, or 0 if n is zero.
(defn sign [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))
;Unit tests
(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

;3. Write a function called roots that returns a vector containing the two possible roots
;that solve a quadratic equation given its three coefficients (a, b, c)
(defn roots [a, b, c]
  (let[d(- b)
       e (sqrt(-(* b b)
                (* 4 a c)))
       f (* 2 a)
       x1(/(+ d e)f)
       x2(/(- d e)f)]
  [x1,x2]))

;Unit tests
(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

;4. The BMI (body mass index) is used to determine if a person's weight and height
; proportion is adequate. The BMI may be calculated using the following formula:
; BMI = weight / height**2
;Write a function called bmi that takes two arguments: weight and height. It should
;return a symbol that represents the corresponding BMI description computed from
;its input.
(defn bmi [weight, height]
  (let[bmi(/ weight (* height height))]
    ;(if (< bmi 20)
    ;  'underweight
    ;  (if (< bmi 25)
    ;    'normal
    ;    (if (< bmi 30)
    ;      'obese1
    ;      (if(< bmi 40)
    ;        'obese2
    ;        'obese3))))))
    (cond
      (< bmi 20) 'underweight
      (< bmi 25) 'normal
      (< bmi 30) 'obese1
      (< bmi 40) 'obese2
      :else      'obese3)))

;Unit tests
(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))
(run-tests)
