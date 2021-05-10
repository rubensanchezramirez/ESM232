#'  Model of forest growth, where forest size is measured in units of carbon, C
#' @param time  period of growth
#' @param C initial carbon
#' @param parms - list with three values, K, thresh, r, g, and K
#' @param parms$K - carrying capacity
#' @param parms$thresh - threshold canopy closure
#' @param parms$r - base growth rate
#' @param parms$g - linear growth rate
#' @param parms$K - carrying capacity
#' @return growth rate of forest
#'
forest_growth = function(time, C, parms) {

  # Forest growth is exponential if carbon is below the canopy closure threshold
  # Forest growth is linear when carbon is greater than or equal to threshold canopy closure
  forest = ifelse(C < parms$thresh, parms$r*C, parms$g)
  # Forest growth is 0 if C is greater than or equal to carrying capacity (K)
  forest = ifelse(C >= parms$K, 0, forest)

  # Return a list of the forest size at time t
  return(list(forest))

}
