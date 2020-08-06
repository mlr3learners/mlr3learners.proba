# Code originally from https://github.com/lilizhaoUM/DNNSurv with minor edits.
# t - Survival time
# d - Censoring indicator
# qt - Vector of time points for dividing time interval
# Returns subject ids, time-points, and pseudo conditional probabilities
get_pseudo_conditional = function(t, d, qt) {

  s = c(0, qt)
  n = length(t)
  ns = length(s) - 1 # the number of intervals
  D = do.call(cbind, lapply(seq_len(ns), function(j) (s[j] < t) * (t <= s[j + 1]) * (d == 1)))
  R = do.call(cbind, lapply(seq_len(ns), function(j) ifelse(s[j] < t, 1, 0)))
  Delta = do.call(cbind, lapply(seq_len(ns), function(j) pmin(t, s[j + 1]) - s[j]))

  # long format
  dd_tmp = cbind.data.frame(id = rep(seq_len(n), ns),
                            s = rep(c(0, qt[-length(qt)]), each = n),
                            y = c(R * Delta),
                            d = c(D))

  dd = dd_tmp[dd_tmp$y > 0, ]
  pseudost = rep(NA, nrow(dd))
  for (j in seq_len(ns)) {
    index = (dd$s == s[j])
    dds = dd[index, ]
    if (all(dds$d) || !any(dds$d)) {
      pseudost[index] = sum(index)
    } else {
      pseudost[index] = pseudo::pseudosurv(time = dds$y, event = dds$d,
                                           tmax = s[j + 1] - s[j])$pseudo
    }
  }
  dd$pseudost = pseudost

  return(dd[, c(1, 2, 5)])
}
