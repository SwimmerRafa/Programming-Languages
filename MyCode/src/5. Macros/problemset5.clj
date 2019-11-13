;----------------------------------------------------------
; Problem Set #5
; Date: November 11, 2019.
; Authors:
;          A01378916 Rafael Moreno
;          A01379896 Erick Bautista
;----------------------------------------------------------
;1 My-or
(defmacro my-or
  "Evaluates its expressions one at a time, from left to right. If a form
  returns a logical true value, it returns that value and doesn't evaluate
  any of the other expressions, otherwise it returns the value of the last
  expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & args]
   `(let [t# ~x]
      (if t#
        ~x
        (my-or ~@args))))
  )

;2 Do-loop
(defn my-condition
  "Returns the condition of the args"
  [args]
  (if (= 0 (compare (first args) :while))
    (last args)
    (cons 'not (list (last args)))
    )
  )

(defmacro do-loop
  "implements a post-test loop control statement. It must combine the
  functionality of C's do-while statement and Pascal's repeat-until statement."
  [& args]
  `(while ~(my-condition (last args))
     (do ~@(butlast args))
     )
  )

;3 Def-pred
(defmacro def-pred
  "The macro should define two predicate functions: a regular one and its
  negated version."
  [name args & body]
  `(do (defn ~name ~args ~@body)
       (defn ~(symbol (str 'not- name)) ~args (not (do ~@body)))
       )
  )

;4 Defn-curry
(defn seek-curry
  "Return a list of nested functions of the args"
  [params body]
  (loop [result body
         i (reverse params)
         ]
    (if (empty? i)
      result
      (let [nested result]
        (recur
          (reverse (cons nested (reverse `(fn ~[(first i)]))))
          (rest i)))))
  )

(defmacro defn-curry
  " It takes as parameters a name, an args vector, and a body of one or more expressions.
  The macro should define a function called name that takes only the first argument from
  args and returns a function that takes the second argument from args and returns a function
  that takes the third argument from args, and so on."
  [name args & body]

  (reverse (cons
             (seek-curry (rest args) `(do ~@body))
             (reverse `(defn ~name ~(if (empty? args) [] [(first args)])))
             )
           )

  )

;5 IF
(defn seek-delimeted
  "Returns a sequence with the elements contains between start and end"
  [args start end]
  (->>
    args
    (drop-while #(not= % start))
    rest
    (take-while #(not= % end))))

(defmacro IF
  "provide a conditional statement that is syntactically a bit more similar
  to those found in languages like Pascal or Fortran. It has the following
  form:
          (IF condition :THEN exp1 exp2 ... :ELSE exp3 exp4 ...)"
  [condition & args]
  `(if ~condition (do ~@(seek-delimeted args :THEN :ELSE))
                  (do ~@(seek-delimeted args :ELSE :THEN)))
  )