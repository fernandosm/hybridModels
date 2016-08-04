#' Daily record of animal's movement (from 2012 to 2013).
#'
#' One dataset containing the number of animals that were moved from one node to 
#' another.
#'
#' @format A data frame with 78 rows and 5 variables:
#' \itemize{
#'   \item Purpose: Type of premises in which animal(s) will arrive
#'   \item Day: The day when the movement occurs
#'   \item originID: The ID of the origin premises
#'   \item destinationID: The ID of the destination premises
#'   \item num.animals: The number of animals traded
#' }
#' @source ADAGRO
"networkSample"

#' Information about animal premises (from 2012 to 2013).
#'
#' A dataset containing animal premises' identification and census.
#'
#' @format A data frame with 507 rows and 2 variables:
#' \itemize{
#'   \item nodes.ID: The ID of the premises
#'   \item pop: premises's population size
#' }
"nodesCensus"