## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib mlr3learners.proba, .registration = TRUE
## usethis namespace: end
NULL

#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import mlr3
#' @importFrom mlr3proba LearnerSurv LearnerDens PredictionSurv PredictionDens TaskSurv
#' @importFrom R6 R6Class
#' @importFrom survival Surv survfit
"_PACKAGE"

# nocov start
register_mlr3 <- function(libname, pkgname) {
  # get mlr_learners dictionary from the mlr3 namespace
  x <- utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("surv.akritas", LearnerSurvAkritas)
  x$add("surv.dnn", LearnerSurvDNNSurv)
}

.onLoad <- function(libname, pkgname) { # nolint
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(),
    action = "append"
  )
}

.onUnload <- function(libpath) { # nolint
  event <- packageEvent("mlr3", "onLoad")
  hooks <- getHook(event)
  pkgname <- vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3learners.proba"],
    action = "replace"
  )
}
# nocov end
