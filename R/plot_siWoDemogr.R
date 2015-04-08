#' @name plot
#' @title Plot for SI without demographics model
#' 
#' @description  \code{plot.siWoDemogr} is a method to plot the SI without
#'               demographics model
#'               
#' 
#' @param x \code{siWoDemogr} object
#' 
#' @param sim which simulation to plot.
#' 
#' @param ... arguments to be passed to methods
#' 
#' @export plot.siWoDemogr
#' @examples 
#' # Parameters and initial conditions.
#' data(networkSample, nodesCensus)
#' networkSample <- networkSample[which(networkSample$Dia < "2012-02-01"),]
#' var.names <- list(from = 'originID', to = 'destinyID', Time = 'Dia',
#'                   arc = 'num.animais')
#' model.parms <- c(Beta = 1e-4)
#' init.cond <- c(I100525 = 10, I1155 = 10, I100324 = 10)
#'                   
#' # running simulations 
#' sim.results <- hybridModel(network = networkSample, var.names,
#'                            nodesCensus = nodesCensus,
#'                            model.parms = model.parms,
#'                            model = 'SI model without demographics',
#'                            sim.number = 1, init.cond = init.cond,
#'                            num.cores = 1)
#' 
#' plot(sim.results)

plot.siWoDemogr <- function(x, sim = 1, ...){
  
  Time <- Number <- variable <- State <- NULL
  
  sim.result <- x$results[which(x$results$sim == sim), ]
  
  sim.result.plot <- reshape2::melt(sim.result, id.vars = c('sim',
                                                            x$ssaObjet$var.names$Time))
  sim.result.plot[, 'State'] <- substring(sim.result.plot$variable,1,1)
  colnames(sim.result.plot)[c(2,4)] <- c('Time','Number')
  
  ggplot2::ggplot(sim.result.plot, ggplot2::aes(x = Time, y = Number,
                                                group = variable, color = State)) + 
    ggplot2::geom_line() + ggplot2::ggtitle(paste('Evolution of Simulation', sim)) +
    ggplot2::ylim(c(0, max(sim.result[,-which(colnames(sim.result) == x$ssaObjet$var.names$Time)])))

  #   sim.result.plot.aggr <- aggregate(sim.result.plot$Number,
  #                                     by = list(sim.result.plot$State, sim.result.plot$Time),
  #                                     sum)
  #   names(sim.result.plot.aggr) <- c('State', 'Time', 'Number')
  #   
  #   plot.aggr <- ggplot2::ggplot(sim.result.plot.aggr,
  #                                ggplot2::aes(x = Time, y = Number, color = State)) +
  #     ggplot2::geom_line() + ggplot2::ggtitle(paste('Evolution of Simulation', sim))
  #   
  #   print(plot.aggr)
}