#' Pivot a PKNCA object to be a wide data.frame
#'
#' The defaults pivot so that all groups are kept as \code{id_cols} and then
#' nominal times are the pivoting column names
#' @inheritParams tidyr::pivot_wider
#' @export
pivot_wider.PKNCAconc <- function(data, id_cols = unname(unlist(data$columns$groups)), id_expand = FALSE, names_from = data$columns$time.nominal,
                                  names_prefix = "", names_sep = "_", names_glue = NULL, names_sort = FALSE,
                                  names_vary = "fastest", names_expand = FALSE, names_repair = "check_unique",
                                  values_from = data$columns$concentration, values_fill = NULL, values_fn = NULL,
                                  unused_fn = NULL, ...) {
  if (length(names_from) == 0) {
    stop("'names_from' must be given and not empty (Did you provide a time.nominal for your data?)")
  }
  d_pivot <-
    # if (PKNCA::is_sparse_pk(data)) {
    #   data$data_sparse
    # } else {
      data$data
    # }
  ret <-
    tidyr::pivot_wider(
      d_pivot, id_cols = id_cols, id_expand = id_expand, names_from = names_from,
      names_prefix = names_prefix, names_sep = names_sep, names_glue = names_glue, names_sort = names_sort,
      names_vary = names_vary, names_expand = names_expand, names_repair = names_repair,
      values_from = values_from, values_fill = values_fill, values_fn = values_fn,
      unused_fn = unused_fn, ...
    )
  ret <-
    dplyr::grouped_df(
      data = ret,
      vars = setdiff(unname(unlist(data$columns$groups)), data$columns$subject)
    )
  attr(ret, "columns") <- data$columns
  class(ret) <- c("PKNCAconc_pivot_wider", class(ret))
  ret
}

countN <- function(x) {
  sum(!is.na(x))
}

#' Summarize concentration by time from a PKNCAconc object that has been pivoted
#'
#' @param object the PKNCAconc_pivot_wider object
#' @param ... Ignored
#' @param .fns A named vector of functions to call to summarize the data
#' @return A data.frame of summarized results
#' @export
summary.PKNCAconc_pivot_wider <- function(object, ..., .fns=c(N=countN, Mean=mean, SD=sd)) {
  col_subject <- attr(object, "columns")$subject
  col_other_groups <- dplyr::group_vars(object)
  col_summary <- setdiff(names(object), c(col_subject, col_other_groups))
  ret <- tibble()
  for (current_fn in names(.fns)) {
    ret <-
      bind_rows(
        ret,
        bind_cols(
          data.frame(Statistic = current_fn),
          dplyr::summarize(object, dplyr::across(all_of(col_summary), .fns = .fns[[current_fn]]))
        )
      )
  }
  ret
}
