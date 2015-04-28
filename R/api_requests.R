
#' Format a list of arguments into a Qualtrics API request.
#' 
#' @param api_args List of arguments
#' @export
format_request <- function(api_args, rooturl = "https://survey.qualtrics.com/WRAPI/ControlPanel/api.php") {
  api_args_encd<-lapply(api_args,URLencode,reserved=T)
  query <- paste(names(api_args_encd), api_args_encd, sep="=", collapse="&")
  request <- paste(rooturl, query, sep="?")
  return(request)
}
