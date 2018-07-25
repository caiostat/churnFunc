#' @title Functions to help reporting churn
#'
#' @description Enable na deletion based in a given set of columns
#'
#' @param df, c, var.
#'
#' @return Filtered data, without na on certain columns
#'
#' @examples
#'
#' @export completeFun

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
