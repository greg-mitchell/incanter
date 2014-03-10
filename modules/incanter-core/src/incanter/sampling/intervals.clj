(ns ^{:doc "Functions for creating and working with intervals."
      :skip-wiki true
      :author "Greg Mitchell"}
  incanter.sampling.intervals
  (:use [clojure.math.combinatorics :only [combinations]])
  (:require clojure.set))

(derive clojure.lang.IPersistentVector ::Interval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-boundedness
  [endpoint boundedness]
  (if (or (= :open boundedness)
        (= Double/POSITIVE_INFINITY endpoint)
        (= Double/NEGATIVE_INFINITY endpoint))
    :open
    :closed))

(defmacro ^:private bound-ineq
  [ineq-fn bound-fn intervals]
  `(loop [i1# (first ~intervals)
          i2# (second ~intervals)
          irest# (nnext ~intervals)]
     (cond
       (and i1# i2# irest#)
       (if (and
             (~ineq-fn
               (~bound-fn i1#)
               (~bound-fn i2#)))
         (recur i2# (first irest#) (rest irest#))
         false)

       (and i1# i2#) (~ineq-fn
                       (~bound-fn i1#)
                       (~bound-fn i2#))
       i1# true)))

(defn- boundp?*
  [pred bound-fn i1 i2]
  (when (apply pred (bound-fn i1 i2))
    i2))

(defn- endpoints-intersect?*
  [bound-fn i1 i2]
  (when (and
          (apply = (bound-fn i1 i2))
          (apply (partial = :closed) (bound-fn (:boundedness (meta i1)) (:boundedness (meta i2)))))
    i2))

(defn- intersect?*
  [i1 i2]
  (cond
    (nil? (and i1 i2)) false
    (endpoints-intersect? :left i1 i2) true
    (endpoints-intersect? :right i1 i2) true
    (endpoints-intersect? :inner i1 i2) true
    (endpoints-intersect? :outer i1 i2) true
    (boundp? < :left i1 i2) (boundp? > :inner i1 i2)
    (boundp? > :right i1 i2) (boundp? < :outer i1 i2)))

(def ^:private bound-fn-from-bound
  {:left (fn [i1 i2] [(first i1) (first i2)])
   :right (fn [i1 i2] [(second i1) (second i2)])
   :inner (fn [i1 i2] [(second i1) (first i2)])
   :outer (fn [i1 i2] [(first i1) (second i2)])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Predicates for intervals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn left<
  [& intervals]
  (bound-ineq < first intervals))

(defn left<=
  [& intervals]
  (bound-ineq <= first intervals))

(defn right<
  [& intervals]
  (bound-ineq < second intervals))

(defn right<=
  [& intervals]
  (bound-ineq <= second intervals))

(defn left>
  [& intervals]
  (bound-ineq > first intervals))

(defn left>=
  [& intervals]
  (bound-ineq >= first intervals))

(defn right>
  [& intervals]
  (bound-ineq > second intervals))

(defn right>=
  [& intervals]
  (bound-ineq >= second intervals))

(defn boundp?
  "
  True if the predicate is true when applied to the given bounds of the intervals.

  Bound:
    :left - the lower bounds
    :right - the upper bounds
    :inner - for i1, i2, the upper bound of i1 and the lower bound of i2
    :outer - for i1, i2, the lower bound of i1 and the upper bound of i2
  "
  [predicate bound & intervals]
  (let [bound-fn (bound-fn-from-bound bound)]
    (reduce (partial boundp?* predicate bound-fn) intervals)))

(defn endpoints-intersect?
  "
  True if for the given bounds the endpoints for the given intervals are all equal and closed.

  Bound:
    :left - the lower bounds
    :right - the upper bounds
    :inner - for i1, i2, the upper bound of i1 and the lower bound of i2
    :outer - for i1, i2, the lower bound of i1 and the upper bound of i2
  "
  [bound & intervals]
  (let [bound-fn (bound-fn-from-bound bound)]
    (reduce (partial endpoints-intersect?* bound-fn) intervals)))

(defn intersect?
  "
  True if the intervals are mutually intersecting.
  "
  [& intervals]
  (let [twos  (combinations intervals 2)
        twos-intersect (map #(apply intersect?* %) twos)]
    (every? identity twos-intersect)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Operations on Intervals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create
  "
  Intervals are convex subsets of the real line, here represented as a vector.

  Intervals may be open or closed, bounded or unbounded on the left or the right, empty or non-empty,
  degenerate or proper.  An unbounded side may represented by :inf or one of the Double infinity statics and
  is assumed to be open.

  Options:
    :left   either :open or :closed.  Defaults to closed.
    :right  either :open or :closed.  Defaults to closed.

  Example:
    (3, 5] -> (create-interval [3 5] :left :open)
    [0, ∞) -> (create-interval [0 :inf])
  "
  [[lower upper] & {:keys [left right]}]
  (let [lower (if (= :inf lower)
                Double/NEGATIVE_INFINITY
                lower)
        upper (if (= :inf upper)
                Double/POSITIVE_INFINITY
                upper)
        left (get-boundedness lower left)
        right (get-boundedness upper right)]
    (with-meta [lower upper] {:boundedness [left right]
                              :tag ::Interval})))

(defn combine
  "
  Returns the combination of one or more intervals if they intersect, or nil otherwise.
  "
  ([i]
    i)
  ([i1 i2]
    (when (intersect? i1 i2)
      (let [[lower left] (if (boundp? < :left i1 i2)
                           [(first i1) (first (:boundedness (meta i1)))]
                           [(first i2) (first (:boundedness (meta i2)))])
            [upper right] (if (boundp? > :right i1 i2)
                            [(second i1) (second (:boundedness (meta i1)))]
                            [(second i2) (second (:boundedness (meta i2)))])]
        (create [lower upper] :left left :right right))))
  ([i1 i2 & more]
    (reduce combine (combine i1 i2) more)))

(declare set-union)
(defn union
  "
  Takes the union of one or more intervals, combines intersecting intervals, and returns a set of intervals.
  "
  ([i]
    (when i
        #{i}))
  ([i1 i2]
    (if-let [combined (combine i1 i2)]
      #{combined}
      #{i1 i2}))
  ([i1 i2 & more]
    (set-union (union i1 i2) (apply union more))))

(defn set-union
  "
  Takes the union each interval in one or more sets of intervals, combining intersecting intervals, and returns a set
  of intervals.
  "
  ([s]
    s)
  ([s1 s2]
    (reduce union (clojure.set/union s1 s2)))
  ([s1 s2 & more]
    (reduce union (union s1 s2) more)))