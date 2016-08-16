#' Get Page Content
#'
#' @param link Character string with the url of the Reddit page from which
#' to extract content.
#'
#' @return A character string containing the json data from the Reddit API call
#'
#' @examples
#' get_page("http://www.reddit.com/")
#' get_page("http://www.reddit.com/r/politics/")
#' get_page("https://www.reddit.com/r/funny/comments/4xuzyl/")
#' get_page("https://www.reddit.com/r/funny/comments/4xuzyl/usain_bolt_celebrating_his_olympic_victory_with/")
get_page <- function(link, user_agent = NA){
  # check to see if link is appended with .json
  if(!endsWith(link, ".json")) link <- paste0(link, ".json")

  # fetch page
  page <- RCurl::getURL(link, httpheader = c('User-Agent' = user_agent))

  return(page)
}

#' Parse page into individual elements
#'
#' @param page Object created by \link{\code{get_page}}
#'
#' @return \code{\link[tidyjson]{tbl_json}} with one row for each element
#' of the page.
#'
#' @importFrom magrittr "%>%"
#'
get_elements <- function(page){
  # convert to tidyjson table and gather children elements
  # end result is a tibble with separate rows for each "thing"
  # (as defined in Reddit API JSON format)
  page %>%
    tidyjson::as.tbl_json(.) %>%
    tidyjson::gather_array(.) %>%
    tidyjson::gather_keys(.) %>%
    dplyr::filter(key == "data") %>%
    tidyjson::gather_keys(.) %>%
    dplyr::filter(key == "children") %>%
    tidyjson::gather_array(.) %>%
    tidyjson::spread_values(type = tidyjson::jstring("kind"))
}

#' Get comments from Reddit post
#'
#' @param elements Object created by \link{\code{get_elements}}
#'
#' @return Tibble with one row for each comment, with all available attributes spread across
#' columns
#' @export
#'
#' @examples
get_comments <- function(elements){
  # extract attributes from elements which are t1 (comments)
  comments <- elements %>%
    dplyr::filter(type == "t1") %>%
    tidyjson::gather_keys(.) %>%
    dplyr::filter(key == "data") %>%
    tidyjson::gather_keys(.) %>%
    # spread attributes from each comment
    spread_attributes(.)

  return(comments)
}

#' Extract attributes from Reddit "things"
#'
#' @param elements
#'
#' @return A \code{\link[tibble]{tibble}} with one row for each element,
#' with all available attributes spread across columns.
#'
spread_attributes <- function(elements){
  # extract all attributes to value column
  elements %>%
    tidyjson::append_values_string("value") %>%
    tibble::as_tibble(.) %>%
    na.omit(.) %>%
    # spread keys to columns
    tidyr::spread(key, value, convert = TRUE)
}
