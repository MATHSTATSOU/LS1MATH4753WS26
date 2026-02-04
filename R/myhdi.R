#' Calculate the Highest Density Interval
#'
#' @param funame the function name as found in R
#' @param credwidth the interval length
#' @param tol how accurate (use default)
#' @param ... extra parameters
#' @importFrom stats optimize
#'
#' @returns The BCI
#' @export
#'
#' @examples
#' myhdi(funame = qbeta , shape1 = 5, shape2 = 7)
myhdi <- function(funame, credwidth = .95, tol = 1e-8,... ) {

  incredwidth <- 1.0 - credwidth # by default 1-0.95 = 0.05 (error or incredible )
  intervalw <- function(lowertail, funame, credwidth, ...) {
    funame(credwidth + lowertail, ...) - funame(lowertail, ...)
  }


  LI <- optimize(f = intervalw, interval = c(0, incredwidth),
                 funame = funame, credwidth = credwidth,
                 tol = tol,
                 ...)

  hdiLTail <- LI$minimum
  BCI = list(L = funame(hdiLTail, ...),
             U = funame(credwidth + hdiLTail, ...))

  return(BCI)
}
