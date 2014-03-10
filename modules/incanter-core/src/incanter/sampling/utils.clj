(ns
  ^{:doc "Utility functions for working with sampling and distributions."
    :skip-wiki true
    :author "Greg Mitchell"}
  incanter.sampling.utils)

;; TODO:
;;  * Union and subtract intervals
;;  * Create lines from 2 points

(comment
  "
  The interval record defines operations on intervals - convex subsets of the real line.

  Intervals may be open or closed, bounded or unbounded on the left or the right, empty or non-empty,
  degenerate or proper.
  ")



