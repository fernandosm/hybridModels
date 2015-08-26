#' @name plot
#' @title Plot for SI without demographics model
#' 
#' @description  \code{plot.HM} is a method to plot hybrid models from this
#'               package
#'               
#' 
#' @param x \code{HM} object
#' 
#' @param sim points to which simulation to plot.
#' 
#' @param should scales be fixed ("free_y", the default), free ("free"), or free
#'        in one dimension ("free_x", "free_y"). See ggplot2 package for more
#'        details.
#'        
#' @param plot.type plots the mean number of each state variable for the whole
#'        population ('pop.mean'), or the subpopulations of a particular
#'        simulation ('subpop'), or the mean of each subpopulation ('subpop.mean').
#' 
#' @param ... arguments to be passed to methods
#' 
#' @export plot.HM
#' @examples 
#' # Parameters and initial conditions.
#' data(networkSample, nodesCensus)
#' networkSample <- networkSample[which(networkSample$Dia < "2012-01-25"),]
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

plot.HM <- function(x, sim = 1, facet.scales = 'free_y', plot.type = 'subpop', ...){
  
  Time <- Number <- variable <- State <- NULL
  
  if(plot.type == 'subpop'){
    sim.result <- x$results[which(x$results$sim == sim), ]
    
    sim.result.plot <- reshape2::melt(sim.result, id.vars = c('sim',
                                                              x$ssaObjet$var.names$Time))
    sim.result.plot[, 'State'] <- substring(sim.result.plot$variable, 1, 1)
    colnames(sim.result.plot)[c(2,4)] <- c('Time','Number')
    
    sim.result.plot$State <- factor(sim.result.plot$State, levels = x$ssaObjet$state.var)
    
    ggplot2::ggplot(sim.result.plot, ggplot2::aes(x = Time, y = Number,
                                                  group = variable, color = State)) + 
      ggplot2::geom_line(alpha = 0.4, size = 0.3) + ggplot2::ggtitle(paste('Simulation', sim)) +
      ggplot2::ylab('Number Of Individuals') + ggplot2::guides(color=FALSE) +
      ggplot2::facet_wrap(~State, ncol = 1, scales = facet.scales) +      
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey", fill = NA),
                     strip.background = ggplot2::element_rect(fill = NA, colour = "grey", size = 0.1),
                     strip.text = ggplot2::element_text(face = "bold", size = 7),
                     panel.margin = grid::unit(0.6, "lines"),
                     plot.title = ggplot2::element_text(size = 10, face = "bold"),
                     axis.text = ggplot2::element_text(size = 6),
                     axis.title = ggplot2::element_text(size = 8, face = "bold"))  
  } else if(plot.type == 'pop.mean'){
    sim.result <- x$results
    sim.result.plot <- reshape2::melt(sim.result, id.vars = c('sim',
                                                              x$ssaObjet$var.names$Time))
    sim.result.plot[, 'State'] <- substring(sim.result.plot$variable, 1, 1)
    sim.result.plot <- aggregate(sim.result.plot$value,
                                 by = list(State = sim.result.plot$State,
                                           Sim = sim.result.plot$sim,
                                           Time = sim.result.plot[ , x$ssaObjet$var.names$Time]), sum)
    sim.result.plot <- aggregate(sim.result.plot$x,
                                 by = list(State = sim.result.plot$State,
                                           Time = sim.result.plot$Time), mean)
    sim.result.plot$State <- factor(sim.result.plot$State, levels = x$ssaObjet$state.var)
    
    ggplot2::ggplot(sim.result.plot, ggplot2::aes(x = Time, y = x,
                                                  group = State, color = State)) + 
      ggplot2::geom_line(size = 0.3) + ggplot2::ggtitle('Population') +
      ggplot2::ylab('Mean Number Of Individuals') + ggplot2::guides(color=FALSE) +
      ggplot2::facet_wrap(~State, ncol = 1, scales = facet.scales) +      
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey", fill = NA),
                     strip.background = ggplot2::element_rect(fill = NA, colour = "grey", size = 0.1),
                     strip.text = ggplot2::element_text(face = "bold", size = 7),
                     panel.margin = grid::unit(0.6, "lines"),
                     plot.title = ggplot2::element_text(size = 10, face = "bold"),
                     axis.text = ggplot2::element_text(size = 6),
                     axis.title = ggplot2::element_text(size = 8, face = "bold"))
  } else if(plot.type == 'subpop.mean'){
    sim.result <- x$results
    sim.result.plot <- reshape2::melt(sim.result, id.vars = c('sim',
                                                              x$ssaObjet$var.names$Time))
    sim.result.plot <- aggregate(sim.result.plot$value,
                                 by = list(Subpop = sim.result.plot$variable,
                                           Time = sim.result.plot[ , x$ssaObjet$var.names$Time]), mean)
    
    sim.result.plot[, 'State'] <- substring(sim.result.plot$Subpop, 1, 1)
    sim.result.plot$State <- factor(sim.result.plot$State, levels = x$ssaObjet$state.var)
    
    ggplot2::ggplot(sim.result.plot, ggplot2::aes(x = Time, y = x,
                                                  group = Subpop, color = State)) + 
      ggplot2::geom_line(alpha = 0.4, size = 0.3) + ggplot2::ggtitle('Subpopulations') +
      ggplot2::ylab('Mean Number Of Individuals') + ggplot2::guides(color=FALSE) +
      ggplot2::facet_wrap(~State, ncol = 1, scales = facet.scales) +      
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey", fill = NA),
                     strip.background = ggplot2::element_rect(fill = NA, colour = "grey", size = 0.1),
                     strip.text = ggplot2::element_text(face = "bold", size = 7),
                     panel.margin = grid::unit(0.6, "lines"),
                     plot.title = ggplot2::element_text(size = 10, face = "bold"),
                     axis.text = ggplot2::element_text(size = 6),
                     axis.title = ggplot2::element_text(size = 8, face = "bold"))
  }
  
}  