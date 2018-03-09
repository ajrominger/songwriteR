#' @title Retrieve Spotify access token
#'
#' @description This function creates a Spotify access token.
#' @param clientID Defaults to System Envioronment variable "SPOTIFY_CLIENT_ID"
#' @param clientSecret Defaults to System Envioronment variable "SPOTIFY_CLIENT_SECRET"
#' @keywords auth
#' @export
#' @examples
#' \dontrun{
#' spotifyAccessTokenGet()
#' }

spotifyAccessTokenGet <- function(clientID = Sys.getenv('SPOTIFY_CLIENT_ID'), clientSecret = Sys.getenv('SPOTIFY_CLIENT_SECRET')) {
  post <- httr::POST('https://accounts.spotify.com/api/token',
                     httr::accept_json(), httr::authenticate(clientID, clientSecret),
                     body = list(grant_type = 'client_credentials'),
                     encode = 'form', httr::config(http_version = 2)) %>% content
  
  if (!is.null(post$error)) {
    stop(paste0('Could not authenticate with given Spotify credentials:\n\t', post$error_description))
  }
  
  return(post$access_token)
}
