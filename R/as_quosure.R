#' @title Helper Function

as_quosure <- function(strs) rlang::parse_quosures(paste(strs, collapse=";"))
