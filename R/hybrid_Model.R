#' Hybrid model simulation.
#' 
#' @description  \code{hybridModel} function runs hybrid models simulations.
#' 
#' @param network a \code{\link{data.frame}} with variables that describe
#'        the donor node, the reciever node, the time when each connection between
#'        donor to the reciever happened and the weight of these connection.
#' 
#' @param var.names a \code{\link{list}} with variable names of the network:
#'        the donor node, the reciever node, the time when each connection between
#'        donor to the reciever happened and the weight of these connection.
#'        The variables names must be "from", "to", "Time" and "arc", respectively.
#'       
#' @param nodes.info a \code{\link{data.frame}} with the first column describing
#'        nodes' ID, the second column with the number of individuals and the third
#'        describing the day of the census (this last variable is not necessary
#'        for all models).
#'        
#' @param model a \code{\link{character}} describing model's name. 
#'
#' @param model.parms a named \code{\link{vector}} with model's parameters.
#'
#' @param sim.number Number of repetitions.The default value is 1
#'
#' @param init.cond a named \code{\link{vector}} with initial conditions.
#'
#' @param ssa.method a \code{\link{list}} with ssa method and tau leap time. The
#'        default method is the direct method.
#'
#' @param pop.correc Whether \code{hybridModel} function tries to balance the number
#'        of individuals or not. The default value is TRUE.
#'        
#' @param num.cores  number of  threads/cores that the simulation will use. the
#'        default value is num.cores = 'max', the algothim will use all
#'        threads/cores available.
#'
#' @return Object containing a \code{\link{data.frame}} (results) with the number
#'         of individuals through time per node and per state.
#'
#' @references  .
#' @seealso \link{GillespieSSA}.
#' @export
#' @examples 
#' # Parameters and initial conditions.
#' data(networkSample, nodesInfo)
#' networkSample <- networkSample[which(networkSample$Dia < "2012-02-01"),]
#' var.names <- list(from = 'originID', to = 'destinyID', Time = 'Dia',
#'                   arc = 'num.animais')
#' model.parms <- c(Beta = 1e-4)
#' init.cond <- c(I100525 = 10, I1155 = 10, I100324 = 10)
#'                   
#' # running simulations 
#' sim.results <- hybridModel(network = networkSample, var.names,
#'                            nodes.info = nodesInfo,
#'                            model.parms = model.parms,
#'                            model = 'SI model without demographics',
#'                            sim.number = 2,
#'                            init.cond = init.cond, num.cores = 2)
#' 
#' plot(sim.results)
#'
hybridModel <-   function(network, var.names, nodes.info, model.parms,
                          model = 'SI model without demographics',
                          sim.number = 1, init.cond = init.cond, pop.correc = TRUE,
                          num.cores = 'max',
                          ssa.method = list(method = character(), tau = integer())){
    
  #### Extracting, trasforming and loading the dynamic network #####
  network <- network[, c(var.names$from, var.names$to, var.names$Time, var.names$arc)]
  
  #### building classes ####
  if(model == 'SI model without demographics'){
    model1 <- 'siWoDemogr'
  }
  
  model2simulate <- buildModelClass(structure(list(network = network,
                                                   ssa.method = ssa.method,
                                                   pop.correc = pop.correc,
                                                   nodes.info = nodes.info),
                                              class = c(model1, 'HM')), var.names,
                                    init.cond, model.parms)
  
  #### running the simulation ####
  model2simulate$results <- simHM(model2simulate, network, sim.number, num.cores)

  return(model2simulate)
}