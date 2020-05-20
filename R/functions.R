#' Make an API query
#'
#' Check and format the query parameters according to the query_map.
#'
#' @param request A \code{character} string. Name of a valid request for an API
#' @param parameters A named \code{list} of the parameters for the query.
#' @param query_map A \code{data.frame} of 7 columns. For an example
#' \link{query_map_string}
#'
#' @return A \code{character} string of the formated query.
#'
#' @examples
#' # query string API for a get_string_ids request
#' make_query(request = 'get_string_ids',
#'            parameters = list(identifiers = 'p53'),
#'            query_map = query_map_string)
#'
#' @export
make_query <- function(request, parameters, query_map) {
  # subset query_map by request type
  if(!request %in% query_map$request) {
    stop()
  }

  df <- query_map[query_map$request == request ,]

  # subset query_map by parameters
  nms <- names(parameters)
  df <- df[df$parameter %in% nms,]
  if(length(nms) != nrow(df)) {
    warning()
  }

  # split df by option
  ll <- split(df, df$option)

  # make parameters list
  param <- list()

  # handle required paramters
  if(nrow(ll$required) == 0) {
    stop()
  }

  n <- nrow(ll$required)
  for(i in 1:n) {
    # extract parameter
    expected <- ll$required[i,]
    input <- parameters[[expected$parameter]]

    # check not null
    if(is.null(input)) {
      stop()
    }

    # check type
    if(class(input) != expected$type) {
      stop()
    }

    # check possible values
    if(!is.na(expected$possible_values)) {
      out <- check_possible(type = expected$type,
                            possible = expected$possible_values,
                            input = input)
      if(!out) {
        stop()
      }
    }

    # add paramter
    if(!is.na(expected$seperator)) {
      param[[expected$parameter]] <- I(paste(input, collapse = expected$seperator))
    } else {
      param[[expected$parameter]] <- input
    }
  }

  # handle optional paramters
  if(length(ll) > 1) {
    n <- nrow(ll$optional)

    for(i in 1:n) {
      # extract parameter
      expected <- ll$optional[i,]
      input <- parameters[[expected$parameter]]

      # check type
      if(class(input) != expected$type) {
        stop()
      }

      # check possible values
      if(!is.na(expected$possible_values)) {
        out <- check_possible(type = expected$type,
                              possible = expected$possible_values,
                              input = input)
        if(!out) {
          stop()
        }
      }

      # add paramter
      if(!is.na(expected$seperator)) {
        param[[expected$parameter]] <- I(paste(input, collapse = expected$seperator))
      } else {
        param[[expected$parameter]] <- input
      }
    }
  }

  # return param
  return(param)
}

#' Send GET request
#'
#' Send a GET request and return the response or the error.
#'
#' @param url A \code{character} string.
#'
#' @return An object of class \code{response}.
#'
#' @importFrom httr http_error GET status_code content
#'
#' @examples
#' \dontrun{
#' # send request
#' url <- 'https://string-db.org/api/tsv/network?identifiers=PTCH1'
#' send_request(url)
#' }
#'
#' @export
send_request <- function(url) {
  # send GET request
  resp <- GET(url)

  # if error, return status code and error message
  if(http_error(resp)) {
    stop(sprintf("API request failed with code %s and the following error/s were returnd: %s",
                 status_code(resp),
                 paste(unlist(content(resp, 'parsed'), use.names = FALSE), collapse = '. ')),
         call. = FALSE)
  }

  # return response object
  return(resp)
}

#' Format the content of a response.
#'
#' Capture the API response in text format then reformat into a tibble.
#'
#' @param resp An object of class \code{response}.
#' @param read_function A funtion to read the response content.
#' @param ... Other arguments passed to \code{read_function}.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # send request
#' url <- 'https://string-db.org/api/tsv/network?identifiers=PTCH1'
#' send_request(url)
#'
#' # format content
#' format_content(resp, format = 'tsv')
#' }
#'
#' @importFrom httr has_content content
#' @importFrom readr read_tsv
#'
#' @export
format_content <- function(resp, read_function = read_tsv, ...) {
  # check resp object hase contents
  if(has_content(resp)) {
    # extract content
    cont <- content(resp, as = 'text')
  } else {
    # or stop
    stop("resp has no content.")
  }

  # read the data in the appropriate format
  res <- read_function(cont, ...)

  # return res
  return(res)
}

#' Check input is possible
#'
#' Test whether the 'input' is among the possible/allowed entries.
#'
#' @param type A character string
#' @param possible A vector of characters or numerics
#' @param input A vector of characters or numerics
#'
#' @export
check_possible <- function(type, possible, input) {
  values <- unlist(strsplit(possible, ','))
  switch(type,
         'numeric' = (input >= as.numeric(values[1])) & (input <= as.numeric(values[2])),
         'character' = input %in% values)
}
