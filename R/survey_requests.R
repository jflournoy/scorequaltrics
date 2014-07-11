library(jsonlite)

#' Return a list of all surveys available to the user
#' 
#' @param creds Qualtrics credentials. A list with values for user and token.
#' @param user Qualtrics username.
#' @param token Qualtrics API token.
#' @param format The format of the requested data. Default is JSON.
#' @param version The API version to use. Defaults to the latest version, 2.3.
#' @export
get_surveys <- function(creds, user = creds$user, token = creds$token, 
                        format = "JSON", version = "2.3") {
  api_args <- list(Request = "getSurveys",
                   User = user,
                   Token = token,
                   Format = format,
                   Version = version)
  
  api_request <- format_request(api_args)
  result <- fromJSON(api_request)
  
  surveys <- result$Surveys$Result
  
  if ("Payload" %in% colnames(surveys)) {
    surveys[["Payload"]] <- NULL # remove the nested columns from Payload
  }
  
  return(surveys)
}

#' Get the name of a Qualtrics survey from the survey ID
#' 
#' @param creds Qualtrics credentials. A list with values for user and token.
#' @param survey_id Qualtris survey id.
#' @param user Qualtrics username.
#' @param token Qualtrics API token.
#' @param format The format of the requested data. Default is JSON.
#' @param version The API version to use. Defaults to the latest version, 2.3.
#' @export
get_survey_name <- function(creds, survey_id, 
                            user = creds$user, token = creds$token, 
                            format = "JSON", version = "2.3") {
  
  surveys <- get_surveys(creds, format = format, version = version)
  survey_name <- surveys[surveys$SurveyID == survey_id, "SurveyName"]
  return(survey_name)
}

#' Get the Qualtrics survey id from the survey name
#' 
#' @param creds Qualtrics credentials. A list with values for user and token.
#' @param survey_name Qualtris survey name.
#' @param user Qualtrics username.
#' @param token Qualtrics API token.
#' @param format The format of the requested data. Default is JSON.
#' @param version The API version to use. Defaults to the latest version, 2.3.
#' @export
get_survey_id <- function(creds, survey_name,
                          user = creds$user, token = creds$token,
                          format = "JSON", version = "2.3") {
  surveys <- get_surveys(creds, format = format, version = version)
  survey_id <- surveys[surveys$SurveyName == survey_name, "SurveyID"]
  return(survey_id)
}
