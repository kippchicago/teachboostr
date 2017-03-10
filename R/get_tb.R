get_total_records <- function(x) {
  x$content$total
}


get_forms <- function(key, ...){

  dots <- list(...)

  if (missing(key)) {
    key <- Sys.getenv('TB_KEY')
  }

  dots$key <- key
  dots$endpoint <- 'forms'

  resp<-teachboost_api('forms', key = key, ...)


  if(exists('parsed', where = dots)){
    if(!dots$parse){
      parsed <- jsonlite::fromJSON(resp$content)

      tot_records <- parsed$total
    }
  } else {
    tot_records <- get_total_records(resp)
  }


  if (exists('limit', where = dots)) {

    limit <- dots$limit

    if (limit <= 1000) {
      out <- list()
      out[[1]] <- resp
    }
  }

  if(!exists('out')){
    if (tot_records > 1000){

      if (exists('limit')) {
        offsets_needed <- floor(limit/1000)
        if (offsets_needed == 0) {
          remainder <- limit
        } else {
          remainder <- limit %% (offsets_needed*1000)
        }


      } else {
        offsets_needed <- floor(tot_records/1000)
        remainder = 1000
      }

      if(offsets_needed == 0) {
        offsets <- 0
        limits <- limit
      } else {
        offsets <- c(1:offsets_needed) * 1000
        limits <- c(rep(1000, times = offsets_needed - 1), remainder)
      }


      offset_resps <- purrr::map2(offsets, limits,  ~{
        dots$offset <- .x
        dots$limit <- .y

        resp_offset <- do.call(teachboost_api, args = dots)
        #resp_offset <- teachboost_api("forms", key = key, offset = .x, limit = .y, ...)
        Sys.sleep(4)
        resp_offset
      })


      if(offsets_needed == 0){
        out <- list()
        out[[1]] <- resp
      } else {
        out <- vector(mode = 'list', length = offsets_needed)

        out[[1]] <- resp

        for (i in offsets_needed) {
          out[[i + 1]] <- offset_resps[[i]]
        }
      }
    }
  }

  out

}




extract_content <- function(x){
  x %>%
    purrr::map("content")
}

parse_content <- function(content) {
  jsonlite::fromJSON(content, simplifyVector = FALSE)
}

list_to_df <- function(.data) {
  .data %>%
    purrr::map(~if(!is.null(.x)) .x else NA) %>%
    purrr::map(~if(length(.x)>0) .x else NA) %>%
    purrr::map(~if(!is.na(.x) && .x != "") .x else NA) %>%
    purrr::map(~if(is.list(.x) && length(.x) > 1) list(as.integer(.x))  else .x) %>%
    tibble::as_tibble()
}

z<- x %>% extract_content() %>%
  purrr::map(parse_content) %>%
  purrr::map_df(~.x$items %>% purrr::map_df(list_to_df))

z1<- x1 %>% extract_content() %>%
  purrr::map(parse_content) %>% #purrr::map("items")
  purrr::map_df(~.x$items %>% purrr::map_df(list_to_df))



