#' @title Build a Keras Multilayer Perceptron
#' @description Utility function to build a Keras MLP.
#' @details This function is a helper for R users with less Python experience. Currently it is
#' limited to simple MLPs and with identical layers.
#' More advanced networks will require manual creation with \CRANpkg{keras}.
#' @param n_in `(integer(1))`\cr Number of input features.
#' @param n_out `(integer(1))`\cr Number of targets.
#' @param nodes `(numeric())`\cr Hidden nodes in network, each element in vector represents number
#' of hidden nodes in respective layer.
#' @param layer_pars `(list())`\cr Arguments passed to [keras::layer_dense].
#' @param activation `(character(1))`\cr Activation function passed to [keras::layer_activation].
#' Default is linear.
#' @param act_pars `(list())`\cr Parameters for activation function, see
#' [keras::layer_activation].
#' @param dropout `(numeric(1))`\cr Optional dropout layer, if `NULL` then no dropout layer added
#' otherwise either same dropout will be added to all layers.
#' @param batch_norm `(logical(1))`\cr If `TRUE` (default) then batch normalisation is applied
#' to all layers.
#' @param batch_pars `(list())`\cr Parameters for batch normalisation, see
#' [keras::layer_batch_normalization].
#'
#' @examples
#' build_keras_net(10, 1)
#'
#' build_keras_net(n_in = 10, n_out = 1, nodes = c(4, 4, 4), activation = "elu")
#' @export
build_keras_net = function(n_in, n_out, nodes = c(32, 32), layer_pars = list(),
  activation = "linear", act_pars = list(),
  dropout = 0.1, batch_norm = TRUE,
  batch_pars = list()) {

  add_module = function(net, num_in, num_out) {
    mlr3misc::invoke(
      keras::layer_dense,
      object = net,
      units = num_out,
      input_shape = num_in,
      .args = layer_pars
    )

    mlr3misc::invoke(
      keras::layer_activation,
      object = net,
      activation = activation,
      .args = act_pars
    )

    if (batch_norm) {
      mlr3misc::invoke(
        keras::layer_batch_normalization,
        object = net,
        .args = batch_pars
      )
    }

    if (!is.null(dropout)) {
      keras::layer_dropout(net, dropout)
    }
  }

  net <- keras::keras_model_sequential()

  # input layer
  add_module(net, n_in, nodes[1])

  # hidden layers
  for (i in seq_along(nodes)) {
    if (i < length(nodes)) {
      add_module(net, nodes[i], nodes[i + 1])
    } else {
      # output layer
      add_module(net, nodes[i], n_out)
    }
  }

  return(net)
}
