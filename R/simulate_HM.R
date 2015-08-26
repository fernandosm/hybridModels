#' It runs the chosen hybrid model.
#' 
#' @description  \code{simHM} is generic function that calls a method to run the
#'               simulation base on object's class
#' 
#' @param x of a specific class of model.
#' 
#' @inheritParams hybridModel
#' 
#' @return A \code{\link{data.frame}} with the number of individuals through
#'         time per node, per state and per simulation.
#'
#' @references  .
#' @seealso \link{GillespieSSA}.
#' @export
#' @import foreach
# @examples
# data(networkSample)
# var.names <- list(from = 'originID', to = 'destinyID', Time = 'Dia',
#                   arc = 'num.animais')
# model.parms <- c(Beta = 1e-4)
# init.cond <- c(I100525 = 10, I1155 = 10, I100324 = 10)
#                   
# model2simulate <- buildModelClass(structure(list(network = networkSample,
#                                                  ssa.method = list(method = character(),
#                                                                    tau = integer()),
#                                                  pop.correc = TRUE,
#                                                  nodes.info = nodes.information),
#                                                  class = c('siWoDemogrMigr', 'HM')))

simHM <- function(x, network, sim.number, num.cores = 'max', fill.time) UseMethod("simHM")