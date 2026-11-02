#' Latin hypercube sample of a parameter space
#'
#' Base-R replacement for `FME::Latinhyper()`. It makes the same random draws
#' in the same order, so a given seed gives the same sample as FME did.
#'
#' @param par_range data frame or matrix with one row per parameter and the
#'   lower and upper bounds in its two columns.
#' @param num integer; number of samples to draw.
#' @return A numeric matrix with `num` rows and one column per parameter,
#'   named by the row names of `par_range`.
#' @noRd
latin_hypercube <- function(par_range, num) {
  npar <- nrow(par_range)
  latin <- matrix(NA_real_, nrow = num, ncol = npar)
  for (i in seq_len(npar)) {
    pr <- unlist(par_range[i, ])
    dpar <- diff(pr) / num
    ii <- sort(stats::runif(num), index.return = TRUE)$ix - 1
    rr <- stats::runif(num)
    latin[, i] <- pr[1] + ii * dpar + rr * dpar
  }
  colnames(latin) <- rownames(par_range)
  latin
}
