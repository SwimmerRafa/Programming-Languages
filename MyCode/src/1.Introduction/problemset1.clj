;Rafael Moreno | A01378916
;Erick Bautista | A01379896

(require '[clojure.test :refer [deftest is run-tests]])
(use 'clojure.math.numeric-tower)
(require '[clojure.math.numeric-tower :refer [abs]])

;1. The function ! takes a positive integer n as its argument
; and returns its corresponding factorial
;SequenceAPI
(defn !
  "Returns the factorial of n"
  [n]
  (reduce *'(range 1 (inc n))))

;Loop/recur
  ;(loop[i n
        ;result 1]
    ;(if (zero? i)
    ;result
    ;(recur(dec i)(*' result i)))))

;Recursion
  ;(if (zero? n)
    ;1
    ;(*' n (!(dec n)))))

;Unit test 1
(deftest test-!
  (is (= 1
         (! 0)))
  (is (= 120
         (! 5)))
  (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
         (map ! (range 11))))
  (is (= 15511210043330985984000000N
         (! 25)))
  (is (= 815915283247897734345611269596115894272000000000N
         (! 40))))

;2. he function duplicate takes a list lst as its argument
; and returns a new list with each element of lst duplicated.
;SequenceAPI
(defn duplicate
     "returns a new list with each element of lst duplicated."
  [lst]
  (mapcat #(list %  %)lst))

;Loop/recur
  ;(loop [lst lst
         ;accum ()]
    ;(if (empty? lst)
      ;(reverse accum)
      ;(recur (rest lst)
             ;(cons (first lst)
                   ;(cons(first lst)
                        ;accum)))))
;Recursion
     ;[lst]
     ;(if (empty? lst)
       ;()
       ;(cons (first lst)
             ;(cons (first lst)
                   ;(duplicate (rest lst)))))
;Unit test 2
(deftest test-duplicate
  (is (= '(1 1 2 2 3 3 4 4 5 5)
         (duplicate '(1 2 3 4 5))))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

;3. The function pow takes two arguments as input: a number a and
; a positive integer b. It returns the result of computing a raised to the power b.
(defn pow
  "returns the result of computing a raised to the power b"
  [a b]
  (reduce *' (repeat b a))
  )

;Unit test 3
(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))

;4. The function fib takes a positive integer n as its
; argument and returns the corresponding element of the Fibonacci sequence.
;SequenceAPI
(defn fib
  "returns the n-th element of the Fibonacci sequence"
  [n]
  (->>
    [0  1]
    (iterate (fn[[a b]] [b (+' a b)]))
    (drop n)
    first
    first))
;Loop/recur
  ;(loop [a 0
  ;       b 1
  ;       i 0]
  ;  (if (= i n)
  ;    a
  ;    (recur b
  ;           (+' a b)
  ;           (inc i)))))
;Recursion
  ;(if (<= n 1)
  ;  n
  ;  (+ (fib (- n 1))
  ;     (fib (- n 2)))))

;Unit test 4
(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
            987 1597 2584 4181 6765)
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))

;5. The function enlist surrounds in a list every upper-level element
; of the list it takes as input.
;API SEQUENCE
(defn enlist
  "surrounds in a list every upper-level element of the list it takes as input."
  [lst]
  (map list lst)
  )

;Unit test 5
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

;6. The function positives takes a list of numbers lst as its argument,
; and returns a new list that only contains the positive numbers of lst.
;Sequence API
(defn positives
  "returns a new list that only contains the positive numbers of lst."
  [lst]
  (filter pos-int? lst))

;Unit test 6
(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))

;7. The function add-list returns the sum of all the elements of its input list, or 0 if
; its empty. Assume that all the elements in the input list are numbers.
;Sequence API
(defn add-list
  "returns the sum of all the elements of its input list, or 0 if its empty. "
  [lst]
  (reduce + lst))

;Unit test 7
(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

;8. The function invert-pairs takes as an argument a list of vectors containing
; two elements each. It returns a new list with every vector pair inverted.
;Sequence API
(defn invert-pairs
  "It returns a new list with every vector pair inverted."
  [lst]
  (for [i lst] (into [] (reverse i))))

;Unit test 8
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))
      (invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

;9. The function list-of-symbols? takes a list lst as its argument.
; It returns true if all the elements (possibly zero) contained in lst are symbols,
; or false otherwise. Use the symbol? predicate to determine if something is a symbol.

(defn list-of-symbols?
  "It returns true if all the elements (possibly zero) contained in lst are symbols,
  or false otherwise."
  [lst]
  (every? true? (map symbol? lst)))

;Unit test 9
(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

;10 The function swapper takes three arguments as input: two values x and y,
; and a list lst. It returns a new list in which every occurrence of x in lst
; is swapped with y, and vice versa. Any other element of lst remains the same.
; You may assume that lst does not contain nested sequences.
(defn swapper
  "returns a new list in which every occurrence of x in lst is swapped with y,
  and vice versa."
  [x y lst]
  (map #(if(= x %) y (if(= y %) x %)) lst)
  )

;Unit test 10
(deftest test-swapper
  (is (= ()
         (swapper 1 2 ())))
  (is (= '(4 3 4 9 9 3 3 3 9 9 7 9
            3 7 8 7 8 4 5 6)
         (swapper 1 2 [4 3 4 9 9 3 3 3 9 9 7
                       9 3 7 8 7 8 4 5 6])))
  (is (= '(4 4 5 1 4 8 1 5 6 4 5 2 9 5 9 9 2 1 1 4)
         (swapper 1 2 [4 4 5 2 4 8 2 5 6 4 5
                       1 9 5 9 9 1 2 2 4])))
  (is (= '(soft purr warm purr little ball of fur
                happy purr sleepy purr kitty kitty kitty)
         (swapper 'purr
                  'kitty
                  '(soft kitty warm kitty little ball
                         of fur happy kitty sleepy kitty
                         purr purr purr)))))

;11. The function dot-product takes two arguments: the lists a and b. It returns
; the result of performing the dot product of a times b. The dot product is an
; algebraic operation that takes two equal-length sequences of numbers and returns
; a single number obtained by multiplying corresponding entries and then summing
; those products.
(defn dot-product
  "It returns the result of performing the dot product of a times b."
  [lst1 lst2]
  (reduce + (map * lst1 lst2)))


;Unit test 11
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4)
                            '(-4.5 3.0 1.5 0.9 0.0)))))

;12. The function average takes a list lst as its argument. It returns the arithmetic
; mean of the numbers contained in lst, or nil if lst is empty.
(defn average
  "eturns the arithmetic mean of the numbers contained in lst, or nil if lst is empty."
  [lst]
  (if (empty? lst)
    nil
    (/(reduce + lst)(count lst))))

;Unit Test 12
(deftest test-average
  (is (nil? (average ())))
  (is (= 4
         (average '(4))))
  (is (= 3
         (average '(5 6 1 6 0 1 2))))
  (is (= 2.5
         (average '(1.7 4.5 0 2.0 3.4 5 2.5 2.2 1.2)))))

;13 The function standard-deviation takes a list lst as its argument. It returns
; the population standard deviation of the numbers contained in lst, or nil if lst is empty.
(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(defn standard-deviation
  "returns the population standard deviation of the numbers contained in lst,
  or nil if lst is empty. "
  [lst]
  (if(empty? lst)
    nil
    (sqrt(* (/ 1 (count lst))
          (reduce + (for [i lst
                          :let [x (*(- i (average lst))(- i (average lst)))]] x)))))
  )

;Unit Test 13
(deftest test-standard-deviation
  (is (nil? (standard-deviation ())))
  (is (aprox= 0.01
              1.87
              (standard-deviation
                '(6 2 3 1))))
  (is (aprox= 0.0001
              12.3153
              (standard-deviation
                '(4 8 15 16 23 42))))
  (is (aprox= 0.00001
              7.07106
              (standard-deviation
                '(110 105 90 100 95))))
  (is (aprox= 0.001
              2.983
              (standard-deviation
                '(9 2 5 4 12 7 8 11
                   9 3 7 4 12 5 4 10
                   9 6 9 4)))))

(run-tests)