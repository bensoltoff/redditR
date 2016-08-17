#' Setup Reddit OAuth2.0
#'
#' @param key Key generated from Reddit developed application
#' @param secret Secret generated from Reddit developed application
#'
#' @return Generates a \code{\link[httr]{Token}} to authenticate Reddit API calls
#' @export
#'
setup_reddit_oauth <- function(key, secret){
  # from httr demo/oauth2-reddit.R
  # 1. Find OAuth settings for reddit:
  #    https://github.com/reddit/reddit/wiki/OAuth2
  reddit <- httr::oauth_endpoint(
    authorize = "https://www.reddit.com/api/v1/authorize",
    access =    "https://www.reddit.com/api/v1/access_token"
  )

  # 2. Register an application at https://www.reddit.com/prefs/apps
  app <- httr::oauth_app("reddit",
                         key = key,
                         secret = secret)

  # 3. Get OAuth credentials
  token <- httr::oauth2.0_token(reddit, app,
                                user_params = list(duration = "permanent"),
                                scope = c("read"),
                                use_basic_auth = TRUE,
                                cache = TRUE)
}
