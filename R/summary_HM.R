#' @name summary
#' @title summary for hybrid models
#' 
#' @description  \code{summary.HM} is a method to print a summary with basic
#'               description of nodes' states at a specific time (the time must
#'               be present in the network data). The default value is Null,
#'               that means it prints nodes' final states. 
#'               
#' @param object \code{HM} object
#' 
#' @param stateVars \code{\link{vector}} containing the state variable to
#'                  summarize. The default value is NULL, which will print a
#'                  summary with all states.
#' 
#' @param at the date (as character) that will be used to print the summary
#' 
#' @param nodes \code{\link{vector}} containing the nodes of interest. The
#'              default value is NULL, which will print a summary with all
#'              nodes.
#' 
#' @param ... arguments to be passed to methods.
#' 
#' @export
#' 
#' @examples 
#' # Parameters and initial conditions for an SIS model
#' # loading the data set 
#' data(networkSample) # help("networkSample"), for more info
#' networkSample <- networkSample[which(networkSample$Day < "2012-03-20"),]
#' 
#' var.names <- list(from = 'originID', to = 'destinationID', Time = 'Day',
#'                   arc = 'num.animals')
#'                   
#' prop.func <- c('beta * S * I / (S + I)', 'gamma * I')
#' state.var <- c('S', 'I')
#' state.change.matrix <- matrix(c(-1,  1,  # S
#'                                  1, -1), # I
#'                               nrow = 2, ncol = 2, byrow = TRUE)
#'                               
#' model.parms <- c(beta = 0.1, gamma = 0.01)
#'
#' init.cond <- rep(100, length(unique(c(networkSample$originID,
#'                                       networkSample$destinationID))))
#' names(init.cond) <- paste('S', unique(c(networkSample$originID,
#'                                         networkSample$destinationID)), sep = '')
#' init.cond <- c(init.cond, c(I36811 = 10, I36812 = 10)) # adding infection
#'                   
#' # running simulations, check num of cores available (num.cores)
#' sim.results <- hybridModel(network = networkSample, var.names = var.names,
#'                            model.parms = model.parms, state.var = state.var,
#'                            prop.func = prop.func, init.cond = init.cond,
#'                            state.change.matrix = state.change.matrix,
#'                            sim.number = 4, num.cores = 2)
#' 
#' summary(sim.results, stateVars = c('S', 'I'), nodes = c(36812, 36813))
#'
summary.HM <- function(object, at = NULL, stateVars = NULL, nodes = NULL, ...){
  
  if (is.null(at)){
    at = max(object$results$Day)
  }
  if (is.null(stateVars) & is.null(nodes)){
    summ.result <- object$results[which(object$results$Day == at),]
    return(summary(summ.result))
  } else if(is.null(nodes)){
    stateVars <- match.arg(stateVars, object$ssaObjet$state.var, several.ok = TRUE)
    summ.result <- object$results[which(object$results$Day == at),
                             grep(paste("^", stateVars, sep='',
                                        collapse="|"), colnames(object$results))]
    return(summary(summ.result))
  } else if(is.null(stateVars)){
    nodes <- match.arg(as.character(nodes),
                       unique(sub(paste("^", object$ssaObjet$state.var, sep='',
                                        collapse="|"), '',
                                  colnames(object$results)[c(-1, -2)])),
                       several.ok = TRUE)
    summ.result <- object$results[which(object$results$Day == at),
                             grep(paste("^",
                                        apply(expand.grid(object$ssaObjet$state.var,
                                                          nodes), 1, paste,
                                              collapse = ""), sep='',
                                        collapse="|"), colnames(object$results))]
    return(summary(summ.result))
  } else{
    stateVars <- match.arg(stateVars, object$ssaObjet$state.var, several.ok = TRUE)
    nodes <- match.arg(as.character(nodes),
                       unique(sub(paste("^", object$ssaObjet$state.var, sep='',
                                        collapse="|"), '',
                                  colnames(object$results)[c(-1, -2)])),
                       several.ok = TRUE)
    summ.result <- object$results[which(object$results$Day == at),
                             grep(paste("^",
                                        apply(expand.grid(stateVars, nodes), 1,
                                              paste, collapse = ""), sep='',
                                        collapse="|"), colnames(object$results))]
    return(summary(summ.result)) 
  }
}