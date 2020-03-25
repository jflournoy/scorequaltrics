#' Return a list of all surveys available to the user
#' 
#' @import qualtRics
#' @export
get_surveys <- function(...) {
    surveysDF <- qualtRics::all_surveys()
    #for redundancy with scripts written with previous versions of this package
    if('name' %in% names(surveysDF)){
        if(! 'SurveyName' %in% names(surveysDF)){
            surveysDF$SurveyName <- surveysDF$name
        }
    }
    return(surveysDF)
}

#' Return a data.frame of the survey responses for a given survey
#' 
#' See \code{\link[qualtRics]{fetch_survey}} for complete information on available options.
#' 
#' @param surveyID Unique survey ID as returned from \code{\link{get_surveys}}.
#' @param label default is \code{FALSE}.
#' @param convert default is \code{FALSE}.
#' @param force_request default is \code{TRUE}.
#' @param ... See \code{\link[qualtRics]{fetch_survey}.}
#' @import qualtRics
#' @export
get_survey_responses<-function(surveyID, label = FALSE, convert = FALSE, force_request = TRUE, ...) {
    responsesDF <- qualtRics::fetch_survey(surveyID, 
                                           label = label, 
                                           convert = convert, 
                                           force_request = force_request, ...)
    return(responsesDF)
}

#' Get the name of a Qualtrics survey from the survey ID
#' 
#' Depricated
#' 
#' @export
get_survey_name <- function(...){
    stop('This function is depricated.')
    return(NULL)
}

#' Get the Qualtrics survey id from the survey name
#' 
#' Depricated 
#' 
#' @export
get_survey_id <- function(...){
    stop('This function is depricated.')
    return(NULL)
}
