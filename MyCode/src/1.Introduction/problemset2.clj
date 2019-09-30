;----------------------------------------------------------
; Problem Set #2
; Date: September 20, 2019.
; Authors:
;          A01379896 Erick Bautista Pérez
;          A01378916 Rafael Moreno Cañas
;----------------------------------------------------------
(require '[clojure.test :refer [deftest is run-tests]])
(use 'clojure.math.numeric-tower)

;1. The function replic takes two arguments: a list lst and an integer number
;n, where n ≥ 0. It returns a new list that replicates n times each element
;contained in lst
(defn replic
  " returns a new list that replicates n times each element contained in lst."
  [n lst]
   (mapcat #(repeat n %)lst)
  )

;Unit test 1
(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))

;2. The function expand takes a list lst as its argument. It returns a list where
;the first element of lst appears one time, the second elements appears two
;times, the third element appears three times, and so on.
(defn expand
  "returns a list where the rst element of lst appears one time, the second
  elements appears two times, the third element appears three times, and so on."
  [lst]
  (mapcat repeat (range 1 (+(count lst)1)) lst)
  )

;Unit test 2
(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

;3  The function insert takes two arguments: a number n and a list of numbers
;lst in ascending order. It returns a new list with the same elements as lst
;but inserting n in its corresponding place.
(defn insert
  "returns a new list with the same elements as lst
  but inserting n in its corresponding place"
  [n lst]
  (sort (cons n lst))
  )

;Unit test 3
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

;4.-The function my-sort takes an unordered list of numbers as an argument,
; and returns a new list with the same elements but in ascending order.
; You must use the insert function defined in the previous exercise to
; write my-sort.
(defn my-sort
  "returns a new list with the same elements but in ascending order."
  [lst]
  (if(empty? lst)
    lst
    (insert (first lst) (pop lst)))
  )


;Unit test 4
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

;5.- The function rotate-left takes two arguments: an integer number n and a
; list lst. It returns the list that results from rotating lst a total of n
; elements to the left. If n is negative, it rotates to the right.
(defn rotate-left
  "returns the list that results from rotating lst a total of n elements to the
  left. If n is negative, it rotates to the right."
  [n lst]
  (if (= 0 (count lst))
    lst
    (if (< n 0)
      (if (= 0 n)
        lst
        (recur (inc n) (conj (drop-last lst) (first (reverse lst)))))
      (if (= 0 n)
        lst
        (recur (dec n) (reverse (cons (first lst) (reverse (drop 1 lst))))))))
    )

;Unit test 5
(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

;6.- The function binary takes an integer n as input (assume that n ≥ 0). If n is
; equal to zero, it returns an empty list. If n is greater than zero, it returns a
; list with a sequence of ones and zeros equivalent to the binary representation of n.
(defn binary
  "If n is equal to zero, it returns an empty list. If n is greater than zero, it
  returns a list with a sequence of ones and zeros equivalent to the binary
  representation of n"
  [n]
  (loop [x n
         accum ()]
    (if (= x 0)
      accum
      (recur (quot x 2) (cons (rem x 2) accum))))
  )

;Unit test 6
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

;7.- The function prime-factors takes an integer n as input (assume that n > 0),
; and returns a list containing the prime factors of n in ascending order.
; The prime factors are the prime numbers that divide a number exactly.
; If you multiply all the prime factors you get the original number.
(defn prime-factors
  "returns a list containing the prime factors of n in ascending order."
  [n]
  (loop [n n
         elemDivisor 2
         listPrime () ]
    (if (< n 2)
      (reverse listPrime)
      (if (= 0 (rem n elemDivisor))
        (recur (/ n elemDivisor) elemDivisor (conj listPrime elemDivisor))
        (recur n (inc elemDivisor) listPrime))))
  )

;Unit test 7
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

;8.- The function gcd takes two positive integer arguments a and b as arguments,
; where a > 0 and b > 0. It returns the greatest common divisor (GCD) of a and b.
; You must NOT use the predefined gcd function from the clojure.math.numeric-tower
; namespace.
(defn gcd1
  "takes two positive integer arguments a and b as arguments, where a > 0 and b > 0.
  It returns the greatest common divisor (GCD) of a and b"
  [a b]
  (loop [a a
         b b]
    (if(= b 0)
      a
      (recur b (mod a b)))))

;Unit test 8
(deftest test-gcd
  (is (= 1 (gcd1 13 7919)))
  (is (= 4 (gcd1 20 16)))
  (is (= 6 (gcd1 54 24)))
  (is (= 7 (gcd1 6307 1995)))
  (is (= 12 (gcd1 48 180)))
  (is (= 14 (gcd1 42 56))))

;9.- The function insert-everywhere takes two arguments as input: an object x and
; a list lst. It returns a new list with all the possible ways in which x can be
; inserted into every position of lst.
(defn insert-everywhere
  "returns a new list with all the possible ways in which x can be inserted into
  every position of lst."
  [x lst]
  (loop [x x
         numb 0
         newlst ()]
    (if(= numb (+ 1(count lst)))
      (reverse newlst)
      (recur x
             (inc numb)
             (conj newlst (flatten (concat (first (split-at numb lst)) (conj (next (split-at numb lst)) x)))))
      )
    )
  )

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

;10. The function deep-reverse takes a list as its input. It returns a list with the
; same elements as its input but in reverse order. If there are any nested lists,
; these too should be reversed.
(defn deep-reverse
  "returns a list with the same elements as its input but in reverse order.
  If there are any nested lists, these too should be reversed."
  [lst]
  (if (empty? lst)
    ()
    (concat (deep-reverse (rest lst))
            (list (if (coll? (first lst))
                    (deep-reverse (first lst))
                    (first lst)))))
  )

;Unit test 10
(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

;11.- The function pack takes a list lst as its argument. If lst contains
; consecutive repeated elements they should be placed in separate sublists.
(defn pack
  "If lst contains consecutive repeated elements they should be placed in separate
  sublists."
  [lst]
  (partition-by identity lst))

;Unit test 11
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))


;12.- The function compress takes a list lst as its argument. If lst contains
; consecutive repeated elements, they should be replaced with a single copy of the
; element. The order of the elements should not be changed.
(defn compress
  "If lst contains consecutive repeated elements, they should be replaced with a
  single copy of the element."
  [lst]
  (flatten (map #(distinct %) (partition-by identity lst)))
  )

;Unit test 12
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

;13.- The function encode takes a list lst as its argument. Consecutive duplicates
; of elements in lst are encoded as vectors [n e], where n is the number of
; duplicates of the element e.
(defn encode
  "takes a list lst as its argument. Consecutive duplicates of elements in lst
  are encoded as vectors [n e], where n is the elem of duplicates of the element e."
  [lst]
  (map vector
       (map count (pack lst))
       (compress lst))
  )

;Unit test 13
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

;14.- The function encode-modified takes a list lst as its argument. It works the
; same as the previous problem, but if an element has no duplicates it is simply
; copied into the result list. Only elements with duplicates are converted to [n e]
; vectors.
(defn encode-modified
  "It works the same as the previous problem,
  but if an element has no duplicates it is simply copied into the result list."
  [lst]
  (map (fn [elem]
         (cond
           (= 1 (first elem)) (last elem)
           :else elem))
       (encode lst)))

;Unit test 14
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

;15.- The function decode takes as its argument an encoded list lst that has the same
; structure as the resulting list from the previous problem. It returns the decoded
; version of lst.
(defn decode
  " It returns the decoded version of lst."
  [lst]
  (flatten (for [i lst]
             (if (vector? i) (repeat (first i) (last i))
                             i)))
  )

;Unit test 15
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)