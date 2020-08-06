setcollapse <- function(x) {
  paste0("{", paste0(x, collapse = ", "), "}")
}

optimizers = c("adadelta", "adagrad", "adamax", "adam", "nadam", "rmsprop", "sgd")

#' @title Get Keras Optimizer
#' @description Utility function to construct optimiser from \CRANpkg{keras}, primarily for
#' internal use.
#' @param optimizer `(character(1))` \cr Optimizer to construct, see details for those available.
#' Default is `"adam"`.
#' @param lr `(numeric(1))` \cr Passed to all optimizers except `adadelta` and `adagrad`.
#' @param beta_1,beta_2,epsilon `(numeric(1))` \cr Passed to `adamax`, `adam`, and `nadam`.
#' @param decay `(numeric(1))` \cr Passed to `adamax`, `adam`, and `sgd`.
#' @param clipnorm,clipvalue `(numeric(1))` \cr Passed to `adamax`, `adam`, `nadam`, and `sgd`.
#' @param schedule_decay `(numeric(1))` \cr Passed to `nadam`.
#' @param momentum `(numeric(1))` \cr Passed to `sgd`.
#' @param nesterov `(logical(1))` \cr Passed to `sgd`.
#' @details Implement optimizers are
#'
#' * `"adadelta"` \cr [keras::optimizer_adadelta]
#' * `"adagrad"` \cr [keras::optimizer_adagrad]
#' * `"adamax"` \cr [keras::optimizer_adamax]
#' * `"adam"` \cr [keras::optimizer_adam]
#' * `"nadam"` \cr [keras::optimizer_nadam]
#' * `"rmsprop"` \cr [keras::optimizer_rmsprop]
#' * `"sgd"` \cr [keras::optimizer_sgd]
#'
#' @export
get_optimizer <- function(optimizer = "adam", lr = 0.02, beta_1 = 0.9, beta_2 = 0.999,
  epsilon = NULL, decay = 0, clipnorm = NULL, clipvalue = NULL,
  schedule_decay = 0.004, momentum = 0, nesterov = FALSE) {
  switch(optimizer,
    adadelta = keras::optimizer_adadelta(),
    adagrad = keras::optimizer_adagrad(),
    adamax = keras::optimizer_adamax(lr, beta_1, beta_2, epsilon, decay, clipnorm, clipvalue),
    adam = keras::optimizer_adam(lr, beta_1, beta_2, epsilon, decay, clipnorm, clipvalue),
    nadam = keras::optimizer_nadam(
      lr, beta_1, beta_2, epsilon, schedule_decay, clipnorm,
      clipvalue),
    rmsprop = keras::optimizer_rmsprop(lr),
    sgd = keras::optimizer_sgd(lr, momentum, decay, nesterov, clipnorm, clipvalue)
  )
}
