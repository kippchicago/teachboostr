#' Unnests a a data frame of list-columns
#'
#' @param x data.frame with one \code{N} columns, where one colum is the \code{id_col} and the
#' \code{N-1} are all list-columns
#' @param id_col id column which will be used to join unnested lists back together
#'
#' @return a data frame
#' @export
unnest_recursive <- function(x, id_col = "id") {

  # for first pass set x_out, for any past after the first, seperate x into x nad x_out
  if (is.data.frame(x)) {
    x_out <- x %>% dplyr::distinct_(id_col)
  } else {
    x_out <- x$x_out
    x <- x$x
  }

  #unnest id_col and second colum
  unnested <- x %>%
    dplyr::select_(id_col, 2) %>%
    tidyr::unnest()

 # join newly unnested to previoulsly unnested
  x_out <- dplyr::left_join(x_out, unnested, by = id_col)

  # drop the newly unested
  x <- x %>% dplyr::select(-2)

  #create list of output and new input
  x <- list(x = x, x_out = x_out)

  # if there's more than one column left recurse away
  if (ncol(x$x) > 1) {
    unnest_recursive(x)
  } else {
    #return the last recursed object's x_out
    return(x$x_out)
  }
}



#' Find and unnest any list-columns in a data frame.
#'
#' @param .data data.frame with some list columns
#' @param id_col the id column to rejoin unnested list-columns with non-list columns
#'
#' @return a data frame
#' @export
unnest_list_cols <- function(.data, id_col = "id"){

  # get colnames to maintian column order
  col_order <- names(.data)

  # seprate non list-columns
  non_list_cols <- .data %>%
    dplyr::select_if(function(col) !is.list(col))

  # seperate list-cols and add back id col
  list_cols <- .data %>%
    dplyr::select_if(is.list) %>%
    dplyr::bind_cols(non_list_cols %>% dplyr::select_(id_col), .)

  # un-nest list columns
  unnested_list_cols <- list_cols %>%
    unnest_recursive(id_col = id_col)

  # join unnested list columns back to non-list-columns

  out <- dplyr::inner_join(non_list_cols, unnested_list_cols, by = id_col)

  out %>% dplyr::select_(.dots = col_order)
}
