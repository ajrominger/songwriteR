#' @title Search for and get artists
#'
#' @description These functions search for artist(s) by name (\code{artistSearch}) and unique ID (\code{artistGet}).
#' @param artistName a vector of artist names; can be one or more long.
#' @param artistID a vector of unique artist IDs as returned by \code{artistSearch}; can be one or more long.
#' @param returnClosest logical; should only the closest matching name be returned. Defaults to \code{TRUE}.
#' @param accessToken Spotify Web API token. Defaults to songwriteR::spotifyAccessTokenGet()
#' @keywords artists
#' @return \code{artistSearch} returns a \code{data.frame} with elements corresponding to the original search terms
#' the matched names, and the associated unique IDs; \code{artistGet} returns a \code{data.frame} with elements x x x; 
#' @examples
#' \dontrun{
#' artistSearch('johnny cash')
#' }
#' @export
#' @rdname Find_Artists

get_artists <- function(artist_name, return_closest_artist = FALSE, accessToken = get_spotify_access_token()) {
  
  # Search Spotify API for artist name
  res <- httr::GET('https://api.spotify.com/v1/search', query = list(q = artisName, type = 'artist', access_token = access_token)) %>%
    content
  
  if (!is.null(res$error)) {
    stop(paste0(res$error$message, ' (', res$error$status, ')'))
  }
  
  content <- res$artists %>% .$items
  
  if (return_closest_artist == TRUE) {
    num_loops <- 1
  } else {
    num_loops <- length(content)
  }
  
  # Clean response and combine all returned artists into a dataframe
  artists <- map_df(seq_len(num_loops), function(this_row) {
    this_artist <- content[[this_row]]
    list(
      artist_name = this_artist$name,
      artist_uri = gsub('spotify:artist:', '', this_artist$uri), # remove meta info from the uri string
      artist_img = ifelse(length(this_artist$images) > 0, this_artist$images[[1]]$url, NA) # we'll grab this just for fun
    )
  }) %>% dplyr::filter(!duplicated(tolower(artist_name)))
  
  return(artists)
}
