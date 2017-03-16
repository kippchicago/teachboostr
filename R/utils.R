#' Extracts content of a a collection of teachboost responses returned by \link{get_tb()}
#'
#' @param x the output (a list of response objects)
#'
#' @return a list of the content portions from a collection of TeachBoost response objects
extract_content <- function(x){
  x %>%
    purrr::map("content")
}

#' Parse the JSON content of a TeachBoost response into a a list
#'
#' @description Note that this function does very minimal simplificaton.
#' For example, nested JSON is returned as nested lists (rather than a flattened list).
#'
#' @param content JSON string (from Teachboost response's content) to be parsed.
#'
#' @return a (possibly nested) list

parse_content <- function(content) {
  jsonlite::fromJSON(content, simplifyVector = FALSE)
}


#' Translate (possibly nested) Teachboost List into a dataframe
#'
#' @description This simply takes a list constructed from \code{\link{parse_content}} and create's a
#' data frame (of the \code{tibble} variety).  It will attempt to to keep simplify the data frame as much
#' as possible, but not too much.  It tries to keep nested lists in a nested format.
#'
#' @param .data parsed list returned from \code{\link{parse_content}}
#'
#' @return a single row data frame
list_to_df <- function(.data) {
  out <- .data %>%
    purrr::map(~if(!is.null(.x)) .x else NA) %>%
    purrr::map(~if(length(.x)>0) .x else NA) %>%
    purrr::map(~if(!is.na(.x) && .x != "") .x else NA) %>%
    purrr::map(~if(is.list(.x) && length(.x) > 1 && !isNested(.x)) list(as.integer(.x))  else .x) %>%
    tibble::as_tibble()
}

#' Unpack a TeachBoost response object (or a list of such objects) into a data frame
#'
#' @description Tries to create a relatively tidy data frame, though it will maintain nested
#' structures as list-columns.
#'
#'
#' @param x the TeachBoost response object (returned by \code{\link{get_tb()}} to be unpacked.
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
#' x <- get_tb("users")
#'
#' x_df <- unpack_tb(x)
unpack_tb <- function(x){
  x %>% extract_content() %>%
    purrr::map(parse_content) %>%
    purrr::map_df(~.x$items %>% purrr::map_df(list_to_df))
}


get_total_records <- function(x) {
  x$content$total
}

