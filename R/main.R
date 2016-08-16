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

  page <- RCurl::getURL(link, httpheader = c('User-Agent' = user_agent))

  return(page)
}

#' Parse page into individual elements
#'
#' @param page Object created by `get_page`
#'
#' @return
#'
#' @importFrom magrittr "%>%"
#'
get_elements <- function(page){
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
