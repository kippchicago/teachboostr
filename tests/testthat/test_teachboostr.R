suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

context("Teachboost_api and helper functions")



api_key <- Sys.getenv("TB_KEY")

endpoints <- c("groups",
               "users",
               "forms",
               "templates",
               "tasks",
               "goals",
               "scores",
               "frameworks")

test_that("teachboost_api connects and gets data from all endpoints", {

api_reps <- endpoints %>%
  purrr::map(~teachboost_api(endpoint = .x, key = api_key))

expect_true(all(api_reps %>% map_lgl(~inherits(.x, "teachboost_api"))))
expect_true(all(!api_reps %>% map_lgl(~httr::http_error(.x$response))))
expect_error(teachboost_api(endpoint = "not_an_endpoint", key = api_key))
})

test_that("get_tb behaves well", {

  get_resps <- endpoints %>%
    map(get_tb)

  expect_warning(get_df <- get_resps %>% map(unpack_tb))
  expect_true(length(get_df) == length(endpoints))
  expect_true(all(get_df %>% map_lgl(is.data.frame)))
  expect_error(get_tb("not_an_endpoint"))
})

test_that("get_tb passes paramaters on to POST request", {

  forms_10 <-   get_tb("forms", limit = 10) %>% unpack_tb()
  forms_1001 <- get_tb("forms", limit = 1001) %>% unpack_tb()
  forms_3003 <- get_tb("forms", limit = 3003) %>% unpack_tb()
  forms_offset <- get_tb("forms", limit = 50, offset=3000) %>% unpack_tb()

  expect_true(nrow(forms_10) == 10)
  expect_true(nrow(forms_1001) == 1001)
  expect_true(nrow(forms_3003) == 3003)
  expect_true(nrow(forms_offset) == 50)
})
