#' @title Search for and get artists
#'
#' @description These functions search for artist(s) by name (\code{artistSearch}) and unique 
#' ID (\code{artistGet}).
#' @param artistName a vector of artist names; can be one or more long.
#' @param artistID a vector of unique artist IDs as returned by \code{artistSearch}; can be 
#' one or more long.
#' @param returnClosest logical; should only the closest matching name be returned. Defaults 
#' to \code{TRUE}.
#' @param accessToken Spotify Web API token. Defaults to songwriteR::spotifyAccessTokenGet()
#' @keywords artists
#' @return \code{artistSearch} returns a \code{data.frame} with elements corresponding to the 
#' matched names, and the associated unique IDs; \code{artistGet} returns a \code{data.frame} 
#' with elements x x x; 
#' @examples
#' \dontrun{
#' artistSearch('johnny cash')
#' }
#' @export
#' @rdname Find_Artists

artistSearch <- function(artistName, returnClosest = TRUE, 
                         accessToken = spotifyAccessTokenGet()) {
  ## search Spotify API for artist name
  res <- httr::GET('https://api.spotify.com/v1/search', 
                   query = list(q = artistName, type = 'artist', 
                                access_token = accessToken))
  res <- httr::content(res)
  
  if(!is.null(res$error)) {
    stop(paste0(res$error$message, ' (', res$error$status, ')'))
  }
  
  ## extract artist info
  res <- res$artists$items
  
  ## from info, extract only the name and ID
  if(returnClosest) {
    out <- do.call(data.frame, res[[1]][c('name', 'id')])
  } else {
    out <- as.data.frame(t(sapply(res, function(r) unlist(r[c('name', 'id')]))))
  }
  
  return(out)
}
