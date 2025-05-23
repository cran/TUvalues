#' @title coalitions
#'
#' @description Create all the possible coalitions given the number of players
#'
#' @param n_players Number of players
#'
#' @return A list containing a \code{data.frame} of the binary representation
#' of the coalitions and a \code{vector} of the classical representation (as
#' sets) of the coalitions
#'
#' @export

coalitions <- function(n_players){

  # create binary representation of the possible coalitions
  tryCatch(
    expr = {
      binary_coalitions <- expand.grid(rep(list(c(0, 1)), n_players))
    },
    error = function(e){
      message(e)
      stop("\nNumber of players is too big.")
    }
  )
  binary_coalitions <- binary_coalitions[order(rowSums(binary_coalitions)),]
  colnames(binary_coalitions) <- 1:n_players
  rownames(binary_coalitions) <- 1:nrow(binary_coalitions)

  # create classical representation (as sets) of the possible coalitions
  classic_coalitions <- c()
  for (i in 1:nrow(binary_coalitions)) {
    cols <- which(binary_coalitions[i, ] == 1)
    classic_coalitions[i] <- toString(cols)
  }

  # return both representations in a list
  sol<-list(binary_coalitions,classic_coalitions)
  names(sol)<-c("Binary","Classic")
  return(sol)

}

coalition_binary_to_numeric <- function(coalition_binary) {
  return(which(coalition_binary == 1))
}
