#' @title Finding elements in contact chains of a dynamic network.
#' 
#' @description Parallel function to find outgoing and ingoing contact chain
#'              elements.
#' 
#' @param Data \code{\link{data.frame}} with network information: node ID, origin
#'        node, destination node, and the time in which the link was established.
#' 
#' @param from \code{\link{character}}, variable name (column name) for origin node.
#' 
#' @param to \code{\link{character}}, variable name (column name) for destination node.
#' 
#' @param Time \code{\link{character}}, variable name (column name) for the time
#'        in which the link was established between two nodes.
#' 
#' @param selected.nodes \code{\link{vector}}, the function will find the contact
#'        chain of the nodes present in the selected.nodes vector.
#' 
#' @param type \code{\link{character}}, of returned reuslt. Type = 'size' (default),
#'        will return the size of 'outgoing' and 'ingoing' contact chains.
#'        Type = 'chain' will return also the nodes in each chain (might be slow
#'        for hugh data sets).
#' 
#' @param numberOfcores \code{\link{integer}}, number of cores used to calculate
#'        the contact chain (default is NULL, that will lead the algotrithm to
#'        use the max number of cores).
#' 
#' @details This is a function that find elements of a contact chain from a dynamic
#'          network.
#' 
#' @return a \code{\link{data.frame}} with ingoing and outgoing contact chains size.
#'         or \code{\link{list}} with the data frame and elements of ingoing and
#'         outgoing chains.
#' 
#' @references 
#' [1] K Buttner, J Krieter, and I Traulsen. Characterization of Contact Structures
#'     for the Spread of Infectious Diseases in a Pork Supply Chain in Northern
#'     Germany by Dynamic Network Analysis of Yearly and Monthly Networks. In:
#'     Transboundary and emerging diseases 2000 (May 2013), pp. 1-12.
#'     
#' [2] C Dube, C Ribble, D Kelton, et al. Comparing network analysis measures to
#'     determine potential epidemic size of highly contagious exotic diseases in
#'     fragmented monthly networks of dairy cattle movements in Ontario, Canada.
#'     In: Transboundary and emerging diseases 55.9-10 (Dec. 2008), pp. 382-392.
#'     
#' [3] C Dube, C Ribble, D Kelton, et al. A review of network analysis terminology
#'     and its application to foot-and-mouth disease modelling and policy development.
#'     In: Transboundary and emerging diseases 56.3 (Apr. 2009), pp. 73-85.
#'     
#' [4] Jenny Frossling, Anna Ohlson, Camilla Bjorkman, et al. Application of
#'     network analysis parameters in risk-based surveillance - Examples based
#'     on cattle trade data and bovine infections in Sweden. In:  Preventive
#'     veterinary medicine 105.3 (July 2012), pp. 202-208.
#'     doi: 10.1016/j.prevetmed.2011.12.011.
#'     
#' [5] Maria Noremark, Nina Ha kansson, Susanna Sternberg Lewerin, et al.
#'     Network analysis of cattle and pig movements in Sweden: measures relevant
#'     for disease control and risk based surveillance. In: Preventive veterinary
#'     medicine 99.2-4 (2011), pp. 78-90. doi: 10.1016/j.prevetmed.2010.12.009.
#' 
#' @export
#' @import foreach
#' @examples 
#' # Loading data
#' data(networkSample) # help("networkSample"), for more info.
#'  
#' # contact chain function
#' selected.nodes <- c(37501, 36811, 36812)
#' contact.chain <- findContactChain(Data = networkSample, from = 'originID',
#'                                   to = 'destinationID', Time = 'Day', selected.nodes,
#'                                   type = 'chain', numberOfcores = 2)

findContactChain <- function(Data, from, to, Time, selected.nodes,
                             type = 'size', numberOfcores = NULL){
  
  #### Making some bindins ####
  n <- NULL
  
  #### Extracting, trasforming and loading the data base #####
  Data <- Data[,c(from, to, Time)]
  
  # creating a new ID for 'to'
  ordered.ID <- sort(unique(c(Data[, from], Data[, to])))
  newID <- data.frame(newID = 1:length(ordered.ID), oldID = ordered.ID)
  
  colnames(newID)[2] <- to
  Data <- merge(Data, newID, by = to)
  Data <- Data[, -which(colnames(Data) == to)] 
  colnames(Data)[which(colnames(Data) == 'newID')] <- to
  
  # creating a new ID for 'from'
  colnames(newID)[2] <- from
  Data <- merge(Data, newID, by = from)
  Data <- Data[,-which(colnames(Data) == from)] 
  colnames(Data)[which(colnames(Data) == 'newID')] <- from
  
  # creating a new ID for 'selected.nodes'
  colnames(newID)[2] <- 'selected.nodes'
  selected.nodes <- data.frame(selected.nodes)
  selected.nodes <- merge(selected.nodes, newID, by = 'selected.nodes')
  
  #### pre-processing for parallel function  ####
  mov.time <- sort(unique(Data[,Time]))
  mov.time2 <- sort(mov.time,decreasing = T)
  tamanho <- length(mov.time)
  
  # Creating an ordered list
  Data <- apply(as.data.frame(mov.time), 1,
                function(x) Data[which(Data[, Time] == x), c(from, to)])
  names(Data) <- mov.time
  
  #### parallel function algorithm 5 ####
  DoInfectionChain5 <- function(){
    
    ingoing.chain <- selected.nodes[n, 'newID']
    outgoing.chain <- selected.nodes[n, 'newID']
    
    for(d in 1:tamanho){
      
      check.out <- length(outgoing.chain)
      outgoing.old <- 0
      while(outgoing.old != check.out){
        
        outgoing.old <- check.out
        
        # Retrive nodes that are connected to nodes in the chain
        outgoing.chain <- union(outgoing.chain, Data[[as.character(mov.time[d])]]
                                [which(Data[[as.character(mov.time[d])]][, from] %in%
                                         outgoing.chain), to])
        check.out <- length(outgoing.chain)
      }
      
      check.in <- length(ingoing.chain)
      ingoing.old <- 0
      while(ingoing.old != check.in){
        
        ingoing.old <- check.in
        # Retrive nodes that are connected to nodes in the chain
        ingoing.chain <- union(ingoing.chain, Data[[as.character(mov.time2[d])]]
                               [which(Data[[as.character(mov.time2[d])]][, to] %in%
                                        ingoing.chain), from])
        check.in <- length(ingoing.chain)
      }
      
    }
    return(c(check.out - 1, check.in - 1, selected.nodes[n, 'selected.nodes']));
  }
  
  #### parallel function algorithm 6 ####
  DoInfectionChain6 <- function(){
    
    ingoing.chain <- selected.nodes[n, 'newID']
    outgoing.chain <- selected.nodes[n, 'newID']
    
    for(d in 1:tamanho){
      
      check.out <- length(outgoing.chain)
      outgoing.old <- 0
      while(outgoing.old != check.out){
        
        outgoing.old <- check.out
        # Retrive nodes that are connected to nodes in the chain
        outgoing.chain <- union(outgoing.chain, Data[[as.character(mov.time[d])]]
                                [which(Data[[as.character(mov.time[d])]][, from] %in%
                                         outgoing.chain), to])
        check.out <- length(outgoing.chain)
      }
      
      check.in <- length(ingoing.chain)
      ingoing.old <- 0
      while(ingoing.old != check.in){
        
        ingoing.old <- check.in
        # Retrive nodes that are connected to nodes in the chain
        ingoing.chain <- union(ingoing.chain, Data[[as.character(mov.time2[d])]]
                               [which(Data[[as.character(mov.time2[d])]][, to] %in% 
                                        ingoing.chain), from])
        check.in <- length(ingoing.chain)
      }
      
    }
    
    mylist <- list()
    # storing chains
    mylist[[paste('ingoing.', selected.nodes[n, 'selected.nodes'] , sep = '')]] <-
      newID[ingoing.chain[-which(ingoing.chain == selected.nodes[n, 'newID'])],
            'selected.nodes']
    
    mylist[[paste('outgoing.', selected.nodes[n, 'selected.nodes'] , sep = '')]] <-
      newID[outgoing.chain[-which(outgoing.chain == selected.nodes[n, 'newID'])],
            'selected.nodes']
    
    mylist[['contact.chain']] <- data.frame(outgoing = check.out - 1,
                                            ingoing = check.in - 1,
                                            selected.nodes = 
                                              selected.nodes[n, 'selected.nodes'])
    return(mylist)
  }
  
  #### Parallel call ####
  if(is.null(numberOfcores))
    numberOfcores <- parallel::detectCores()
  
  cl <- parallel::makeCluster(numberOfcores, type = "PSOCK")
  doParallel::registerDoParallel(cl)
  
  if(type == 'size') {
    
    contact.chain <- foreach(n=1:length(selected.nodes[,'selected.nodes']),
                               .verbose=FALSE, .combine = 'rbind',
                               .inorder=FALSE) %dopar% (DoInfectionChain5())
    
    parallel::stopCluster(cl)
    
    contact.chain <- data.frame(outgoing = contact.chain[, 1],
                                  ingoing = contact.chain[, 2],
                                  selected.nodes = contact.chain[, 3])
    
    return(contact.chain)
    
  } else if(type == 'chain'){
    
    contact.chain2 <- foreach(n = 1:length(selected.nodes[,'selected.nodes']),
                              .verbose = FALSE, .inorder = FALSE) %dopar% (DoInfectionChain6())
    
    parallel::stopCluster(cl)
    
    contact.chain <- do.call(rbind.data.frame, lapply(contact.chain2, '[[',
                                                      'contact.chain'))
    ingoing.nodes <- lapply(contact.chain2, '[[', 1)
    names(ingoing.nodes) <- sub('ingoing.', '', sapply(lapply(contact.chain2,
                                                              names), '[[', 1))
    outgoing.nodes <- lapply(contact.chain2, '[[', 2)
    names(outgoing.nodes) <- sub('outgoing.', '', sapply(lapply(contact.chain2,
                                                               names), '[[', 2))
    
    return(list(contact.chain = contact.chain, ingoing.nodes = ingoing.nodes,
                outgoing.nodes = outgoing.nodes))
  }
}
