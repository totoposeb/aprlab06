#' Example data for the knapsack problem
#'
#' This is the sample data provided by the lab 06 instructions:
#' ```
#' suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#' v = runif(n = n, 0, 10000))
#' ```
#'
#' @format A data.frame with 2000 rows and 2 variables:
#' \describe{
#'   \item{w}{Weight of the item.}
#'   \item{v}{Value of the item.}
#' }
#' @source <https://github.com/STIMALiU/AdvRCourse/raw/master/2026_LecturesLabs/732A94_AdvancedRHT2026_Lab06.pdf>
"knapsack_objects"
