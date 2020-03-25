#' creds_from_file
#'
#' Reads the API key and base URL from a file and sets up the environment
#' variables necessary for accessing data. Uses
#' \code{\link[qualtRics]{qualtrics_api_credentials}}, but does not save
#' credentials to the .Renviron file.
#'
#'
#' @param creds_yaml The .yaml file that contains the user's credentials
#' @import yaml
#' @import qualtRics
#' @export
creds_from_file <- function(creds_yaml = "credentials.yaml") {
  creds <- yaml.load_file(creds_yaml)
  
  if(is.null(creds$token) || is.null(creds$baseurl)){
    stop(
"Creds file should look like

token: 'API KEY HERE'
baseurl: 'BASE URL HERE'

")
  }
  
  loaded <- qualtRics::qualtrics_api_credentials(api_key = creds$token, base_url = creds$baseurl, install = FALSE)
  
  if(!all(loaded)){
    stop('Could not set API credentials.')
  } else {
    message('Credentials loaded...')
  }
  
  invisible(TRUE)
}