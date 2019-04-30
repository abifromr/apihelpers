context("test-functions")

test_that("make_query works", {
  # handels single paramter
  q <- make_query(request = 'get_string_ids',
                  parameters = list(identifiers = 'p53'),
                  query_map = query_map_string)

  expect_true(is.list(q))
  expect_equal(length(q), 1)

  # handels multiple parameters
  q <- make_query(request = 'get_string_ids',
                  parameters = list(identifiers = 'p53',
                                    species = 9606),
                  query_map = query_map_string)

  expect_true(is.list(q))
  expect_equal(length(q), 2)

  # handels multi-input
  q <- make_query(request = 'get_string_ids',
                  parameters = list(identifiers = c('p53', 'cdkc2'),
                                    species = 9606),
                  query_map = query_map_string)

  expect_true(is.list(q))
  expect_equal(length(q), 2)
})

test_that("make_query returns a warning", {
  expect_warning(make_query(request = 'get_string_ids',
                            parameters = list(identifiers = c('p53', 'cdkc2'),
                                              notparameter = 9606),
                            query_map = query_map_string))
})

test_that("make_query stops when appropriate", {
  expect_error(make_query(request = 'not_request',
                          parameters = list(identifiers = c('p53', 'cdkc2'),
                                            species = 9606),
                          query_map = query_map_string))
  expect_error(make_query(request = 'get_string_ids',
                          parameters = list(identifiers = NULL,
                                            species = 9606),
                          query_map = query_map_string))
  expect_error(make_query(request = 'get_string_ids',
                          parameters = list(identifiers = 123,
                                            species = 9606),
                          query_map = query_map_string))
  expect_error(make_query(request = 'get_string_ids',
                          parameters = list(identifiers = c('p53', 'cdkc2'),
                                            species = '9606'),
                          query_map = query_map_string))
})

test_that("test send_request", {
  good_url <- 'https://string-db.org/api/tsv/network?identifiers=PTCH1'
  expect_true(httr::has_content(send_request(good_url)))

  bad_url <- 'https://string-db.org/api/tsv/network?identifiers=notPTCH1'
  expect_error(send_request(bad_url))
})

test_that("test format_content", {
  url <- 'https://string-db.org/api/tsv/get_string_ids?identifiers=p53'
  resp <- httr::GET(url)
  res <- format_content(resp)

  expect_true(is.data.frame(res))
  resp$content <- NULL

  expect_error(format_content(resp))
})
