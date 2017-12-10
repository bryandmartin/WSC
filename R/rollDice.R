#' A dice roller for DnD
#'
#' @param dice A vector of dice values.
#' @param modifier Optional modifier.
#'
#' @examples
#' # Roll 2d10 + 7
#' rollDice(rep(10,2), 7)
#'
#' @export
rollDice <- function(dice, modifier = NULL) {
  total <- 0
  for (i in 1:length(dice)) {
    total <- total + sample(1:dice[i], 1)
  }
  if (!is.null(modifier)) {
    total <- total + modifier
  }
  return(total)
}
