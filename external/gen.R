#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @examples
gen = function (n = 100L) {
  dt = as.data.table(list(id = seq_len(n)))
  dt[, grp := ((id - 1) %% 26) + 1
     ][, grp := letters[grp]
       ][]
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
aggr = function (x) {
  stopifnot(
    is.data.table(x),
    "grp" %in% names(x)
  )
  x[, .N, by = grp]
}
