
#' Format a list of arguments into a Qualtrics API request.
#' 
#' @param api_args List of arguments
#' @export
format_request <- function(api_args, rooturl = "https://survey.qualtrics.com/WRAPI/ControlPanel/api.php") {
  query <- paste(names(api_args), api_args, sep="=", collapse="&")
  request <- paste(rooturl, query, sep="?")
  return(request)
}
