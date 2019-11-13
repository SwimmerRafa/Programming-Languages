(declare variable?
         same-variable?
         sum?
         augend
         addend
         make-sum
         product?
         multiplicand
         multiplier
         make-product
         exponentiation?
         base
         exponent
         make-exponentiation)

(defn deriv
  "Derives exp with respect to var"
  [exp var]
  (cond
    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var)
      1
      0)

    (sum? exp)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var))

    (product? exp)
    (make-sum (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var)))

    (exponentiation? exp)
    (make-product (exponent exp)
                  (make-product ((make-exponentiation (base exp) (dec (exponent exp))))
                                (deriv (base exp) var)))))



(defn variable?
  "Is exp a variable?"
  [exp]
  (symbol? exp))

(defn same-variable?
  "Are v1 and v2 a variable?"
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn sum?
  "Is exp a sum?"
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '+ (first exp))))

(defn augend
  "Return the augend"
  [exp]
  (nth exp 1))

(defn addend
  "Returns the addend"
  [exp]
  (nth exp 2))

(defn make-sum
  "Construct the sum of a1 and a2"
  [a1 a2]
  (cond
    (= a1 0) a2
    (= a2 0) a1
    (and (number? a1)
         (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))


(defn product?
  [exp]
  "Is exp a product?"
  (and (list? exp)
       (= 3 (count exp))
       (= '* (first exp))))

(defn multiplicand
  "The multiplicand of the product"
  [exp]
  (nth exp 1))

(defn multiplier
  "Returns the multiplier of the product exp"
  [exp]
  (nth exp 2))

(defn make-product
  "Construct the product of m1 and m2"
  [m1 m2]
  (cond
    (= m1 0) 0
    (= m2 0) 0
    (= m1 1) m2
    (= m2 1) m1
    (and (number? m1)
         (number? m2)) (* m1 m2)
    :else (list '*  m1 m2)))

(defn base
  "The base of exp"
  [exp]
  (nth exp 1))

(defn exponent
  "The exponent of exp"
  [exp]
  (nth exp 2))

(defn exponentiation?
  "Is exp an exponentiation?"
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '** (first exp))))

(defn make-exponentiation
  "base times exponent"
  [a1 a2]
  (cond
    (= a1 0) 0
    (= a2 0) 1
    (= a2 1) a1
    (= a1 1) 1
    (and (number? a1)
         (number? a2)) (* a1 a2)
    :else (list '** a1 a2)))
