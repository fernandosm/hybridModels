#' @name buildModelClass
#' @export
buildModelClass.customInfl <- function(x, var.names, init.cond, model.parms,
                                       probWeights, emigrRule,
                                       prop.func, state.var, infl.var,
                                       state.change.matrix){
  
  #### args creation ####
  node.ID <- sort(unique(c(x$network[, var.names$from],
                           x$network[, var.names$to])))
  number.nodes <- length(node.ID)
  number.statVar <- length(state.var)
  number.inflVar <- length(infl.var)
  number.propFun <- length(prop.func)
  
  mov.dates <- sort(unique(x$network[, var.names$Time]), na.last = NA)
  time.diff <- c(as.numeric(mov.dates[2:length(mov.dates)] -
                              mov.dates[1:(length(mov.dates)-1)]),1)
  
    #### ssa methods ####
  if (is.null(x$ssa.method)){
    x$ssa.method <- list(method = "D", tau = 0.3, f = 10, epsilon = 0.03,
                         nc = 10, hor = NaN, dtf = 10, nd = 100)
  } else{
    userdef <- x$ssa.method
    x$ssa.method <- list(method = "D", tau = 0.3, f = 10, epsilon = 0.03,
                         nc = 10, hor = NaN, dtf = 10, nd = 100)
    
    x$ssa.method[names(userdef)] <- userdef
  }
  
  #### building propensity functions and x0 ####
  propFunc <- character()
  x0 <- integer(number.nodes * number.statVar)
  infl.parms <- integer(number.nodes * number.inflVar)
  state.var.replacements <- character(number.statVar)
  infl.var.replacements <- character(number.inflVar)
  names(state.var.replacements) <- state.var
  names(infl.var.replacements) <- infl.var
  j <- 1
  k <- 1
  
  # propensity functions and parameters (infl.var) for each node
  for(i in 1:number.nodes){
    state.var.replacements[1:number.statVar] <- paste(state.var, node.ID[i], sep = '')
    infl.var.replacements[1:number.inflVar] <- paste(infl.var, node.ID[i], sep = '')
    propFunc <- c(propFunc,
                  unlist(stringr::str_split(
                    stringr::str_replace_all(stringr::str_c(prop.func, collapse = "---"),
                                             c(infl.var.replacements,
                                               state.var.replacements)), "---")))
    
    names(x0)[j:(i * number.statVar)] <- state.var.replacements
    names(infl.parms)[k:(i * number.inflVar)] <- infl.var.replacements
    j <- j + number.statVar
    k <- k + number.inflVar
  }
  x0[names(init.cond)] <- init.cond
  model.parms <- c(model.parms, infl.parms)
  
  #### building state-change matrix ####
  scMatrix <- matrix(integer(number.statVar * number.propFun),
                     nrow = (number.nodes * number.statVar),
                     ncol = (number.nodes * number.propFun))
  
  i <- j <- 1
  while(i < (number.statVar * number.nodes)){
    scMatrix[i:(i + number.statVar - 1),
             j:(j + number.propFun - 1)] <- state.change.matrix
    
    i <- i + number.statVar
    j <- j + number.propFun
  }
  
  #### Results Data Frame ####
  results <- as.data.frame(stats::setNames(replicate(length(x0),
                                                     integer(length(mov.dates)),
                                                     simplify = FALSE), names(x0)))
  results[1, names(x0)] <- x0
  
  results <- cbind.data.frame(sim = integer(length(mov.dates)), Time = mov.dates, results)
  colnames(results)[2] <- var.names$Time
  
  # adding last day
  results <- rbind(results, results[length(mov.dates),])
  results[(length(mov.dates) + 1), var.names$Time] <- mov.dates[length(mov.dates)] + 1
  
  return(structure(list(ssaObjet = list(propFunction = propFunc, x0 = x0,
                                        sCMatrix = scMatrix, parms = model.parms,
                                        probWeights = probWeights, emigrRule = emigrRule,
                                        mov.dates = mov.dates, time.diff = time.diff,
                                        number.nodes = number.nodes,
                                        infl.var = infl.var, var.names = var.names,
                                        state.var = state.var,
                                        ssa.method =  x$ssa.method,
                                        pop.correc = x$pop.correc),
                        results = results), class = c('customInfl', 'HM')))
}
