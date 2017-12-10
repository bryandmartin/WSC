#' Takes orders as a vector, total price after tax/delivery/tip
#'
#' @param orders Vector of orders as numerics, sums are allowed.
#' @param total Total for the order, after all fees.
#' @param names Optional vector of names for pretty output
#'
#' @examples
#' splitBill(c(3.30, 1.90, 3.80, 8.25 + 1.90, 10.25),
#' 35.36, names = c("Malcolm", "Russell", "Yandi", "Johnny", "Bryan"))
#'
#' @export
splitBill <- function(orders, total, names = NULL) {
  # Get the proportion of bill owed, based on pretotal
  bill_prop <- orders / sum(orders)
  # Proportionally split extra
  owes <- round(bill_prop*total, 2)
  # Nice output if names
  if (!is.null(names)) {
    for (i in 1:length(orders)) {
      cat(names[i], " owes $", owes[i], "\n", sep = "")
    }
  } else {
    return(owes)
  }
}
