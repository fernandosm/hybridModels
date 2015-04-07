#' It builds an object of a pre-specified class.
#' 
#' @description  \code{buildModelClass} is generic function that calls a method
#'               to create a object base on model's name.
#' 
#' @param x is an empty object of a class requested.
#' 
#' @inheritParams hybridModel
#' 
#' @return An object of the class requested.
#' 
#' @export
#' @references .
# @examples 
# data(networkSample)
# var.names <- list(from = 'originID', to = 'destinyID', Time = 'Dia',
#                   arc = 'num.animais')
# model.parms <- c(Beta = 1e-4)
# init.cond <- c(I100525 = 10, I1155 = 10, I100324 = 10)
#                   
# model2simulate <- buildModelClass(structure(list(network = network.sample,
#                                                  ssa.method = list(method = character(),
#                                                                    tau = integer()),
#                                                  pop.correc = TRUE,
#                                                  nodes.info = nodes.information),
#                                                  class = c('siWoDemogr', 'HM')))
buildModelClass <- function(x, var.names, init.cond, model.parms) UseMethod("buildModelClass")