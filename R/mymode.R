#' Compute the statistical mode of a vector
#'
#' @description
#' Returns the most frequent value(s) (mode) of a vector. The function avoids
#' clashing with base R's \code{mode()} by using the name \code{stat_mode()}.
#'
#' @details
#' By default, if there are multiple values tied for the highest frequency,
#' all of them are returned. Use \code{ties} to control how ties are resolved.
#'
#' \itemize{
#'   \item \code{"all"}: return all modes (default; a vector of length >= 1).
#'   \item \code{"first"}: the first value to reach the maximum frequency based on original order.
#'   \item \code{"last"}: the last value to reach the maximum frequency based on original order.
#'   \item \code{"random"}: one mode chosen uniformly at random among ties.
#'   \item \code{"min"}: the smallest mode (works for types that can be ordered; e.g., numeric, Date).
#'   \item \code{"max"}: the largest mode (works for types that can be ordered; e.g., numeric, Date).
#' }
#'
#' If \code{na.rm = FALSE}, \code{NA} values are considered as a candidate mode.
#'
#' @param x A vector (numeric, character, factor, logical, Date, POSIXt, etc.).
#' @param na.rm Logical; if \code{TRUE}, remove \code{NA}s before computing the mode.
#' @param ties Character string specifying how to handle ties.
#'   One of \code{c("all","first","last","random","min","max")}.
#'
#' @return
#' If \code{ties = "all"}, returns a vector of one or more modes of the same
#' base type (e.g., numeric or character). Otherwise, returns a length-1 vector
#' of the corresponding type. When \code{na.rm = FALSE} and \code{NA} is a mode,
#' \code{NA} may be among the returned values.
#'
#' @examples
#' # Simple examples
#' stat_mode(c(1, 2, 2, 3))
#' stat_mode(c("a", "b", "a", "c", "c"), ties = "all")
#' stat_mode(c(TRUE, TRUE, FALSE))
#'
#' # Ties handling
#' x <- c(1, 2, 2, 3, 3)
#' stat_mode(x, ties = "all")     # c(2, 3)
#' stat_mode(x, ties = "first")   # 2
#' stat_mode(x, ties = "last")    # 3
#' stat_mode(x, ties = "min")     # 2
#' stat_mode(x, ties = "max")     # 3
#'
#' # NA handling
#' y <- c(NA, NA, 1, 1, 2)
#' stat_mode(y, na.rm = FALSE, ties = "all")  # c(NA, 1)
#' stat_mode(y, na.rm = TRUE)                 # 1
#'
#' # Factors
#' f <- factor(c("low", "low", "med", "high", "high"))
#' stat_mode(f, ties = "all")
#'
#' @seealso \code{\link[base]{table}}, \code{\link[base]{unique}}
#' @author Wayne S. Stewart
#' @export
stat_mode <- function(x,
                      na.rm = FALSE,
                      ties = c("all", "first", "last", "random", "min", "max")) {
  ties <- match.arg(ties)

  # Special case: length 0
  if (length(x) == 0L) {
    return(x)
  }

  # Handle NA removal or counting
  if (na.rm) {
    x2 <- x[!is.na(x)]
    # If all were NA
    if (length(x2) == 0L) {
      return(x2)  # empty of same type
    }
    ux <- unique(x2)
    idx <- match(x2, ux)
    counts <- tabulate(idx, nbins = length(ux))
  } else {
    na_count <- sum(is.na(x))
    x2 <- x[!is.na(x)]
    # Build uniques and counts for non-NA
    if (length(x2) > 0L) {
      ux <- unique(x2)
      idx <- match(x2, ux)
      counts <- tabulate(idx, nbins = length(ux))
    } else {
      ux <- x2
      counts <- integer(0L)
    }
    # If NA present, treat as its own candidate
    if (na_count > 0L) {
      ux <- c(ux, NA)     # append a single NA
      counts <- c(counts, na_count)
    }
  }

  # Identify ties
  maxc <- max(counts)
  tie_idx <- which(counts == maxc)

  # Helper: try an ordering-based choice (min/max) across comparable values
  choose_ordered <- function(which_fn) {
    candidates <- ux[tie_idx]
    # Try to order; if not possible, fall back to "first"
    ord_ok <- TRUE
    res <- tryCatch({
      o <- order(candidates)  # relies on `<` methods for the type
      candidates[which_fn(o)]
    }, error = function(e) {
      ord_ok <<- FALSE
      NULL
    })
    if (!ord_ok) {
      # Fall back deterministically: "first"
      return(ux[tie_idx[1L]])
    }
    res
  }

  out <- switch(
    ties,
    "all" = ux[tie_idx],
    "first" = ux[tie_idx[1L]],
    "last" = ux[tie_idx[length(tie_idx)]],
    "random" = ux[sample(tie_idx, size = 1L)],
    "min" = choose_ordered(function(o) o[1L]),
    "max" = choose_ordered(function(o) o[length(o)])
  )

  # Preserve factor levels if x was factor (and result is from non-NA candidates)
  if (is.factor(x)) {
    # Coerce via levels to keep class
    return(factor(out, levels = levels(x)))
  }

  out
}
