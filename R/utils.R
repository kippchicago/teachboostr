#' Extracts content of a a collection of teachboost responses returned by \link{get_tb}
#'
#' @param x the output (a list of response objects)
#'
#' @return a list of the content portions from a collection of TeachBoost response objects
extract_content <- function(x){
  x %>%
    purrr::map("content")
}


#' Translate (possibly nested) Teachboost List into a dataframe
#'
#' @description This simply takes a list constructed from \code{\link{extract_content}} and create's a
#' data frame (of the \code{tibble} variety).  It will attempt to to keep simplify the data frame as much
#' as possible, but not too much.  It tries to keep nested lists in a nested format.
#'
#' @param .data a list returned from \code{\link{extract_content}}
#'
#' @return a single row data frame
list_to_df <- function(.data) {
  out <- .data %>%
    purrr::map(~if(!is.null(.x)) .x else NA) %>%
    purrr::map(~if(length(.x)>0) .x else NA) %>%
    purrr::map(~if(!is.na(.x) && .x != "") .x else NA) %>%
    purrr::map(~if(is.list(.x) && length(.x) > 1 && !isNested(.x)) list(as.integer(.x))  else .x) %>%
    purrr::map(~if(is.list(.x) && length(.x) == 1 && is.integer(.x)) as.integer(.x)  else .x) %>%
    tibble::as_tibble()

  out
}

#' Unpack a TeachBoost response object (or a list of such objects) into a data frame
#'
#' @description Tries to create a relatively tidy data frame, though it will maintain nested
#' structures as list-columns.
#'
#'
#' @param x the TeachBoost response object (returned by \code{\link{get_tb}} to be unpacked.
#' @param unnest_cols should nested list-columns be unnested, default is \code{TRUE}
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_tb("users")
#'
#' x_df <- unpack_tb(x)
#' }
unpack_tb <- function(x, unnest_cols = TRUE){
  content <- x %>% extract_content()

  if (attr(x, "endpoint") == "forms") {
    out <- content %>%
      purrr::map(~jsonlite::fromJSON(.x, simplifyVector = FALSE)) %>%
      purrr::map_df(~.x$items %>% purrr::map_df(list_to_df))

      if (unnest_cols) out <- out %>% unnest_list_cols()

  } else {
    out <- content %>%
      purrr::map(~jsonlite::fromJSON(.x, flatten = TRUE)) %>%
      purrr::map_df("items")
  }

  out

}



isNested <- function(l) {
  if (!is.list(l)) return(FALSE)
  for (i in l) {
    if (is.list(i)) return(TRUE)
    }
  return(FALSE)
  }


get_total_records <- function(x) {
  x$content$total
}

