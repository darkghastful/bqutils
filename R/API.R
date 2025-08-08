#' Content from endpoint
#'
#' @param url API URL
#' @param content_type (default "application/octet-stream")
#' @param accept  (default "application/json")
#' @param fromJSON (default TRUE)
#'
#' @return encoded
#' @export
#'
#' @examples
#' content.from.endpoint("https://api-web.nhle.com/v1/standings-season")
content.from.endpoint <- function(url, content_type="application/octet-stream", accept="application/json", fromJSON=TRUE){
  # url <- generate.endpoint.url(extension)
  response <- httr::VERB("GET", url, httr::content_type(content_type), httr::accept(accept))
  encoded <- httr::content(response, "text", encoding="UTF-8")
  if(fromJSON){
    encoded <- jsonlite::fromJSON(encoded)
  }
  return(encoded)
}
