#' report_p
#'
#' @param p double required
#' @param digits significant digits
#'
#' @return p_string a string
#' @export
#'
#' @examples
#' report_p(0.02018) # returns "p = .020"
#' report_p(0.00028) # returns "p < .001"
report_p <- function(p, digits = 3) {
  if (p < 0) stop("p cannot be less than 0")
  if (p > 1) stop("p cannot be greater than 1")
  if (p < .001) return("p < .001")

  p_round <- round(p, digits)
  p_string <- paste("p =", p_round)

  return(p_string)
}

