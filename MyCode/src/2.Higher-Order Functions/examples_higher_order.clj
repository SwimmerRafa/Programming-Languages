(defn my-map
  "Returns a list resulting from applying fun to each
  element in lst"
  [fun lst]
  (if(empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))

(defn make-mul-fun
  "Makes a new multiply by x function"
  [X]
  (fn [y]
    (* X y)))

(defn fun
  [a b c d e]
  (* (+ a b)(/ c d) e))

(defn fun-curry
  [a]
  (fn [b]
    (fn [c]
      (fn [d]
        (fn [e]
          (* (+ a b)(/ c d) e))))))

(defn composite
  [f g]
  (fn [x]
    (f (g x))))

