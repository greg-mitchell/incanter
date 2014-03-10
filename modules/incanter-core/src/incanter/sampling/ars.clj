(ns ^{:doc "Adaptive Rejection Sampling function for arbitrary distributions.
            Based on Gilks (1992).  Note that this method assumes that the
            distribution is log-concave."
      :skip-wiki true
      :author "Greg Mitchell"}
  incanter.sampling.ars
  (:use
    [incanter.distributions]
    [incanter.interp.utils]))

(defmethod sample :ars
  [f & [{:keys [domain initial-points ln-f ln-f']}]]
  )
