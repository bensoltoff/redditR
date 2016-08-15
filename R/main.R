#' Get Page Content
#'
#' @param link Character string with the url of the Reddit page from which
#' to extract content.
#'
#' @return A character string containing the json data from the Reddit API call
#'
#' @examples
#' get_page_content("http://www.reddit.com/")
#' get_page_content("http://www.reddit.com/r/politics/")
#' get_page_content("https://www.reddit.com/r/funny/comments/4xuzyl/")
#' get_page_content("https://www.reddit.com/r/funny/comments/4xuzyl/usain_bolt_celebrating_his_olympic_victory_with/")
get_page_content <- function(link){
  # check to see if link is appended with .json
  if(endsWith(link, ".json")) url <- paste0(link, ".json")

  return(readLines(link))
}
