
#' Get and collect responses from the TeachBoost API.
#'
#' @description A wrapper around \code{\link{teachboost_api}}, which will return as many response
#' objects as needed (given any constraints passed on to \code{\link{teachboost_api}})
#'
#' @param endpoint the TeachBoost API endpoint to sent a GET request to.
#' @param key your organizations unique TeachBoost API key.  If one is not supplied the function will
#' look in the R enivronment for a \code{TB_KEY} key and retriev it's value. You can place your key in
#' an \code{.Renviron} file.
#' @param ... parameters passed on to \code{\link{teachboost_api}}
#'
#' @return a list of teachboost response objects
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_teachboost("users")
#'
#' x <- get_teachboost("forms", limit = 1200)
#' }

get_tb <- function(endpoint, key, ...){

  dots <- list(...)

  if (missing(key)) {
    key <- Sys.getenv('TB_KEY')
  }

  if(key == "") stop("You need to supply a TeachBoost API key./nYou can save on in an .Renviron file as TB_KEY='your_key_here'.")

  dots$key <- key
  dots$endpoint <- endpoint

  resp<-teachboost_api(endpoint = endpoint, key = key, ...)


  if(exists('parsed', where = dots)){
    if(dots$parsed){

      tot_records <- get_total_records(resp)

    }
  } else {
    parsed <- jsonlite::fromJSON(resp$content)

    tot_records <- parsed$total

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
        #out <- vector(mode = 'list', length = offsets_needed + 1)

        out <- list()
        out[[1]] <- resp

        for (i in 1:offsets_needed) {
          out[[i + 1]] <- offset_resps[[i]]
        }
      }
    }
  }

  if(!exists('out')) {
    out <- list()
    out[[1]] <- resp
  }

  attr(out, "endpoint") <- endpoint

  #return
  out
}

