#' R API for TeachBoost's RESTful API
#'
#' @param endpoint the endpoint from which data is requested
#' @param key your organizaitons TeachBoost API token
#' @param parsed wheterh the whether the response content should be parsed to a (possibly flattened) data.frame (`TRUE`) or
#' the whether the raw JSON us returned (`FALSE`).
#' @param ... Not used
#'
#' @details This is a workhourse function that wraps the HTTP RESTful API that TeachBoos has implemented.
#' You will need your API key, which you'll need to request from TeachBoost.
#'
#' @return a teadchboostr object, which is a list containing the response, endpoint path,
#' and returned content
#' @export
teachboost_api <- function(endpoint,
                          key,
                          parsed = TRUE,
                          ...){


  end_point <- sprintf('api/v1/%s', endpoint)

  url <- httr::modify_url(url = sprintf('https://teachboost.com/'),
                          path = end_point
  )


  ua <- httr::user_agent("http:://github.com/kippchicago/teachboostr")

  body_list = list(
    token = key,
    ...
  )


  resp <- httr::POST(url = url,
                     body = body_list,
                    # ua
  )

  # response error checking
  httr::stop_for_status(resp)



  if (parsed) {
    resp_content <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                                       simplifyVector = FALSE,
                                       simplifyDataFrame = FALSE,
                                       simplifyMatrix = FALSE
                                          )
  } else {
    resp_content <- httr::content(resp, "text", encoding = "UTF-8")
  }


  structure(
    list(
      content = resp_content,
      path = end_point,
      response = resp
    ),
    class = "teachboost_api"
  )
}


#' @export
print.teachboost_api <- function(x, ...) {
  cat("<TeachBoost ", x$path, ">\n", sep = "")
  str(x$content, max.level = 2)
  invisible(x)
}
