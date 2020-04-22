;;propel
(ns propel.core
  (:gen-class)
  (:use clj-fuzzy.metrics))


;;added [clj-fuzzy "0.4.1"] to dependencies in project.clj

(def example-push-state
  {:exec '()
   :integer '(1 2 3 4 5 6 7)
   :string '("abc")
   :input {:in1 4}})

; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
   'in1
   ; 'integer_+
   ; 'integer_-
   ; 'integer_*
   ; 'integer_%
   ; 'integer_=
   ; 'exec_dup
   ; 'exec_if
   ; 'boolean_and
   ; 'boolean_or
   ; 'boolean_not
   ; 'boolean_=
   ; 'string_=
   'string_take
   'string_drop
   'string_reverse
   'string_concat
   ; 'string_length
   ; 'string_includes?
   'string_replacefirst
   'string_swap
   'substring_swap
   ; 'close
   0
   1
   2
   ; true
   ; false
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "abcdefghijklmnopqrstuvwxyz"
   "[]():.=-#"
   "0123456789"
   ))

(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1
   'exec_if 2})

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :input {}})

(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))

(defn integer_+
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  [state]
  (make-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             int1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (make-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))

(defn boolean_and
  [state]
  (make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))

(defn string_=
  [state]
  (make-push-instruction state = [:string :string] :boolean))

(defn string_take
  [state]
  (make-push-instruction state
                         #(apply str (take %1 %2))
                         [:integer :string]
                         :string))

(defn string_drop
  [state]
  (make-push-instruction state
                         #(apply str (drop %1 %2))
                         [:integer :string]
                         :string))

(defn string_reverse
  [state]
  (make-push-instruction state
                         #(apply str (reverse %))
                         [:string]
                         :string))

(defn string_concat
  [state]
  (make-push-instruction state
                         #(apply str (concat %1 %2))
                         [:string :string]
                         :string))

(defn string_length
  [state]
  (make-push-instruction state count [:string] :integer))

(defn string_includes?
  [state]
  (make-push-instruction state clojure.string/includes? [:string :string] :boolean))


(defn string_replacefirst
  [state]
  (make-push-instruction state
                         #(apply str (clojure.string/replace-first %1 %2 %3))
                         [:string :string :string]
                         :string))


(defn string_swap_helper [s]
  (let [sub1
        (apply str(take (rand-int (count s)) s))]
    (let [sub2 (apply str(drop (count sub1) s))]
      (apply str (concat " " sub2 sub1)))
  ))

(defn string_swap
  [state]
  (make-push-instruction state
                          #(apply str (string_swap_helper %))
                          [:string]
                          :string))

; (defn substring_swap_helper [s]
;   (let [sub0 (subs s (rand-int (/(count s) 2)) (+ (/(count s) 2) (rand-int (/(count s) 2))))]
;     (let [sub1
;           (apply str(take (rand-int (count sub0)) sub0))]
;       (let [sub2 (apply str(drop (count sub1) sub0))]
;         (apply str (concat sub2 sub1)))
;       )))

(defn substring_swap_helper [s ind1 ind2]
  (let [sub0 (subs s ind1 ind2)]
    (let [sub1
          (apply str(take (rand-int (count sub0)) sub0))]
      (let [sub2 (apply str(drop (count sub1) sub0))]
        (apply str (concat (take ind1 s) sub2 sub1 (drop ind2 s))))
      )))


(defn substring_swap
  [state]
  (make-push-instruction state
                          #(apply str (substring_swap_helper %1 %2 %3))
                          [:string :integer :integer]
                          :string))
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-raw (first (:exec state))
        first-instruction (if (symbol? first-raw)
                            (eval first-raw)
                            first-raw)]
    (cond
      (fn? first-instruction)
      (first-instruction popped-state)
      ;
      (integer? first-instruction)
      (push-to-stack popped-state :integer first-instruction)
      ;
      (string? first-instruction)
      (push-to-stack popped-state :string first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else
      (throw (Exception. (str "Unrecognized Push instruction in program: "
                              first-instruction))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opens
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))

(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))


(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))



(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))

; (defn new-individual
;   "Returns a new individual produced by selection and variation of
;   individuals in the population."
;   [pop argmap]
;   {:plushy
;    (let [prob (rand)]
;      (cond
;        (< prob 0.7) (uniform-addition (:plushy (select-parent pop argmap))
;                                        (:instructions argmap))
;        (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
;                                (:plushy (select-parent pop argmap)))       
;        :else (uniform-deletion (:plushy (select-parent pop argmap)))
;        ))})

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
      (when (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap))))
      (when (< prob 0.7) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap)))
      (when (< prob 0.7) (uniform-deletion (:plushy (select-parent pop argmap)))))})

(defn report
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println)))

(defn propel-gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size]
    :as argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                     population-size
                     #(hash-map :plushy
                                (make-random-plushy instructions
                                                    max-initial-plushy-size)))]
    (let [evaluated-pop (sort-by :total-error
                                 (map (partial error-function argmap)
                                      population))]
      (report evaluated-pop generation)
      (cond
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size
                                 #(new-individual evaluated-pop argmap)))))))


(defn compute-next-row
  "computes the next row using the prev-row current-element and the other seq"
  [prev-row current-element other-seq pred]
  (reduce
    (fn [row [diagonal above other-element]]
      (let [update-val (if (pred other-element current-element)
                         ;; if the elements are deemed equivalent according to the predicate
                         ;; pred, then no change has taken place to the string, so we are
                         ;; going to set it the same value as diagonal (which is the previous edit-distance)
                         diagonal
                         ;; in the case where the elements are not considered equivalent, then we are going
                         ;; to figure out if its a substitution (then there is a change of 1 from the previous
                         ;; edit distance) thus the value is diagonal + 1 or if its a deletion, then the value
                         ;; is present in the columns, but not in the rows, the edit distance is the edit-distance
                         ;; of last of row + 1 (since we will be using vectors, peek is more efficient)
                         ;; or it could be a case of insertion, then the value is above+1, and we chose
                         ;; the minimum of the three
                         (inc (min diagonal above (peek row))))]
                         
        (conj row update-val)))
    ;; we need to initialize the reduce function with the value of a row, since we are
    ;; constructing this row from the previous one, the row is a vector of 1 element which
    ;; consists of 1 + the first element in the previous row (edit distance between the prefix so far
    ;; and an empty string)
    [(inc (first prev-row))]
    ;; for the reduction to go over, we need to provide it with three values, the diagonal
    ;; which is the same as prev-row because it starts from 0, the above, which is the next element
    ;; from the list and finally the element from the other sequence itself.
    (map vector prev-row (next prev-row) other-seq)))

(defn levenshtein-distance
  "Levenshtein Distance - http://en.wikipedia.org/wiki/Levenshtein_distance
     In information theory and computer science, the Levenshtein distance is a
     metric for measuring the amount of difference between two sequences. This
     is a functional implementation of the levenshtein edit
     distance with as little mutability as possible.
     Still maintains the O(n*m) guarantee."
  [a b & {p :predicate  :or {p =}}]
  (cond
    (empty? a) (count b)
    (empty? b) (count a)
    :else (peek
            (reduce
              ;; we use a simple reduction to convert the previous row into the next-row  using the
              ;; compute-next-row which takes a current element, the previous-row computed so far
              ;; and the predicate to compare for equality.
              (fn [prev-row current-element]
                (compute-next-row prev-row current-element b p))
              ;; we need to initialize the prev-row with the edit distance between the various prefixes of
              ;; b and the empty string.
              (range (inc (count b)))
              a))))

(defn sequence-similarity
  [sequence1 sequence2]
  "Returns a number between 0 and 1, indicating how similar the sequences are as a normalized,
  inverted Levenshtein distance, with 1 indicating identity and 0 indicating no similarity."
  (if (and (empty? sequence1) (empty? sequence2))
    1
    (let [dist (levenshtein-distance sequence1 sequence2)
          max-dist (max (count sequence1) (count sequence2))]
      (/ (- max-dist dist) max-dist))))

(defn sequence-similarity-jw
  [sequence1 sequence2]
  "Using jaro-winkler instead of levenshtein."
  (jaro-winkler sequence1 sequence2))

(defn length-matching
  [sequence1 sequence2]
  "comparing lengths of output and correct output."
  (if (zero? (- (count sequence1) (count sequence2)))
    0
    (/ (Math/abs (- (count sequence1) (count sequence2))))))

(defn string-classification-error-function
  "Finds the behaviors and errors of an individual: Error is 0 if the value and the program's selected behavior match, or 1 if they differ, or 1000000 if no behavior is produced. The behavior is here defined as the final top item on the :boolean stack."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs [
        ; "(C(=O)O).(OCC)"
        ; "C c 1 c c 2 c ( [N+] ( = O ) [O-] ) c c c c 2 c [n+] 1 [O-] . O = P ( Cl ) ( Cl ) Cl"
        "O C 1 c 2 c c c c c 2 O c 2 n c c c c 2 1"
        ; "O = c 1 c 2 c c ( C = N O ) c c c 2 o c 2 n c c c c 1 2"
        ; "O = C ( [O-] ) c 1 c c c 2 o c 3 n c c c c 3 c ( = O ) c 2 c 1 . O = S ( Cl ) Cl"
        "N c 1 c c c c ( C ( F ) ( F ) F ) c 1 "
        ]
        correct-outputs [
        ; "(C(=O)OCC).(O)"
        ; "C c 1 c c 2 c ( [N+] ( = O ) [O-] ) c c c c 2 c ( Cl ) n 1"
        "c 1 c c c 2 c ( c 1 ) C c 1 c c c n c 1 O 2"
        ; "N # C c 1 c c c 2 o c 3 n c c c c 3 c ( = O ) c 2 c 1"
        ; "O = C ( Cl ) c 1 c c c 2 o c 3 n c c c c 3 c ( = O ) c 2 c 1"
        "N C 1 C C C C ( C ( F ) ( F ) F ) C 1"
        ]
        outputs (map (fn [input]
                       (peek-stack
                        (interpret-program
                         program
                         (assoc empty-push-state :input {:in1 input})
                         (:step-limit argmap))
                        :string))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        ; (+ (* 0.5 (float (- 1 (sequence-similarity output correct-output))))
                        ; (float (- 1 (sequence-similarity-jw output correct-output)))) ;;1-jw, so the higher this float value the worse the similarity
                        ; ))
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        (+ (float (- 1 (sequence-similarity output correct-output))) (* 0.25 (float (length-matching output correct-output )))) ))
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply +' errors))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'propel.core)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function string-classification-error-function
                                  :max-generations 500
                                  :population-size 600
                                  :max-initial-plushy-size 600
                                  :step-limit 100
                                  :parent-selection :lexicase
                                  ;;:tournament
                                  ;;:tournament-size 5
                                }
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))

