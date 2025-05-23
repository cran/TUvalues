#' @title Banzhaf-Owen value
#'
#' @description
#' Calculate the Banzhaf-Owen value
#'
#' @param characteristic_func The valued function defined on the subsets of the number
#' of players
#' @param union List of vectors indicating the a priori unions between the
#' players
#' @param method Method used to calculate the Owen value. Valid methods are:
#' \code{exact} for the exact calculation or \code{appro} for approximated polynomial
#' calculation based on sampling.
#' @param n_rep Only used if \code{method} is \code{appro}. The number of
#' iterations to perform in the approximated calculation
#' @param n_players Only used if \code{characteristic_func} is a \code{function}.
#' The number of players in the game.
#' @param replace should sampling be with replacement?
#'
#' @return The Banzhaf-Owen value for each player
#'
#' @examples
#' characteristic_func <- c(0,0,0,0,30,30,40,40,50,50,60,70,80,90,100)
#' union <- list(c(1,3),c(2),c(4))
#' banzhaf_owen(characteristic_func, union)
#' banzhaf_owen(characteristic_func, union, method = "appro", n_rep = 4000)
#'
#' @export

banzhaf_owen <- function(characteristic_func, union, method = "exact", n_rep = 10000,
                    n_players = 0, replace = TRUE){

  if (! method %in% c("exact", "appro")) {
    stop("Invalid methos specified\n Use \"exact\" for the exact value or \"appro\" for the approximation.")
  }

  if(!is.vector(characteristic_func) && !is.function(characteristic_func)) {
    stop("Invalid characteristic_func provided.")
  }

  if (method == "exact") {
    if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(banzhaf_owen_exact(characteristic_func, union, n_players))
  } else {
    if (n_rep < 1) {
      stop("Invalid number of iterations specified. m must be greater than 0.")
    } else if (is.function(characteristic_func) && n_players < 2) {
      stop("Invalid number of players specified. n_players must be greater than 1.")
    }
    return(banzhaf_owen_appro(characteristic_func, union, n_players, n_rep, replace))
  }

}
