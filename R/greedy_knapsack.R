#' Greedy Knapsack Solver
#'
#' Solves the 0/1 knapsack problem using a greedy heuristic. Items are ranked
#' by value density (\code{v / w}) in descending order, and added one by one
#' until the next item would exceed the capacity \code{W}. This heuristic does
#' not guarantee an optimal solution.
#'
#' @param x A \code{data.frame} with columns \code{v} (values) and \code{w}
#'   (weights), both numeric and positive.
#'
#' @param W A single positive number giving the maximum weight capacity.
#'
#' @return A list with:
#'   \describe{
#'     \item{value}{Total value of the selected items.}
#'     \item{elements}{Integer vector of row indices (in the original \code{x})
#'       of the selected items, or \code{integer(0)} if none were selected.}
#'   }
#'
#' @references
#' https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#' @examples
#' data(knapsack_objects)
#' greedy_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#'
#' @export
greedy_knapsack <- function(x, W) {
  # Validates the inputs
  stopifnot(
    "`W` must be a positive number!" = is.numeric(W) && length(W) == 1 && W > 0,
    "`x` must be a data.frame with columns `v` and `w`!" = is.data.frame(x) &&
      all(c("v", "w") %in% names(x)),
    "`x` columns `v` and `w` must contain positive numbers only!" = is.numeric(
      x$v
    ) &&
      all(x$v > 0) &&
      is.numeric(x$w) &&
      all(x$w > 0)
  )

  # Calculates value density column
  x$v_density <- x$v / x$w

  # Sorts the data.frame x according to the v_density values, largest first
  ord <- order(x$v_density, decreasing = TRUE)
  x_sorted <- x[ord, ]

  # Sets w_cum (cummulative weight) and v_cum (cummulative value) to zero
  w_cum <- 0
  v_cum <- 0

  # Initializes iterator i to 1
  i <- 1

  # Iterates over each row in x to add the next most densely valuable element
  repeat {
    # Breaks if adding the next element exceeds W
    if (i > nrow(x) || w_cum + x_sorted[i, "w"] > W) {
      break
    }

    # Sets the new cummulative values with the current element
    w_cum <- w_cum + x_sorted[i, "w"]
    v_cum <- v_cum + x_sorted[i, "v"]
    i <- i + 1
  }

  # Returns a list with the best value and elements found
  list(
    value = v_cum,
    elements = ord[seq_len(i - 1)]
  )
}
