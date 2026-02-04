#' Using compiled C++ to create variance
#'
#' @param x numeric vector
#' @param na_rm logical for remove NA
#'
#' @returns the variance
#' @export
#'
#' @examples
#' myvar(x = rnorm(30))
myvar <- function(x, na_rm = FALSE){
  cpp_var(x, na_rm)
}
