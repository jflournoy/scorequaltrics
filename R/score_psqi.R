#' Score PSQI Item 2
#'
#' This function assumes any value is in minutes.
#'
#' @param psqi_2 character that can be coerced to number after text is stripped.
#'
#' @return the corresponding score for this item.
score_psqi_2 <- function(psqi_2){
    #First, extract the numeric part of the answer, which we assume is in minutes.
    psqi_2_extracted <- gsub('.*?(\\d+).*', '\\1', psqi_2)
    #then make it numeric (we could do this above, but fine to do it in 2 steps)
    psqi_2_num <- as.numeric(psqi_2_extracted)
    
    #now we turn it into a score
    if (is.na(psqi_2_num)){
        psqi_2_score <- NA
    } else if (psqi_2_num <= 15){
        psqi_2_score <- 0
    } else if (psqi_2_num > 15 & psqi_2_num <= 30) {
        psqi_2_score <- 1
    } else if (psqi_2_num > 30 & psqi_2_num <= 60) {
        psqi_2_score <- 2
    } else if (psqi_2_num > 60) {
        psqi_2_score <- 3
    } else {
        psqi_2_score <- NA
    }
    return(psqi_2_score)
}

#' Score PSQI item 4
#'
#' @param psqi_4 duration in hours. May be in the form 'H:M' or as a float like '6.5'.
#' @param return_value 'score' returns the corresponding item score. 'hours' returns
#' numeric number of hours.
#'
#' @return score or number of hours.
score_psqi_4 <- function(psqi_4, return_value = 'score'){
    #If the time is formatted like '6:45'
    if (is.na(psqi_4)) {
        hours <- NA
    } else if (length(gregexpr(':', psqi_4)[[1]]) == 1 & gregexpr(':', psqi_4)[[1]] != -1){
        cleantime <- gsub('.*?(\\d+:\\d+).*', '\\1', psqi_4)
        atime <- lubridate::hm(cleantime)
        hours <- as.numeric(atime)/60^2
    } else { #assume it's a number of hours
        hours <- as.numeric(gsub('.*?(\\d+).*', '\\1', psqi_4))
    }
    
    #now we turn it into a score
    if (is.na(hours)){
        psqi_4_score <- NA
    } else if (hours >= 7){
        psqi_4_score <- 0
    } else if (hours >= 6 & hours < 7) {
        psqi_4_score <- 1
    } else if (hours >= 5 & hours < 6) {
        psqi_4_score <- 2
    } else if (hours < 5) {
        psqi_4_score <- 3
    } else {
        psqi_4_score <- NA
    }
    if (return_value == 'hours'){
        return(hours)
    } else if (return_value == 'score'){
        return(psqi_4_score)
    }
}

#' PSQI time to POSIX
#'
#' @param psqi_time character time string formatted as 'H', 'H:M', 'Ip', 'I p', 'I:M p', or 'I:Mp'.
#' @param assume For bed times, use 'bedtime'. For wake times, use 'waketime'.
#'
#' @return An object of class POSIXct on an arbitrary date.
#' @import lubridate
#' @export
psqi_to_time_date <- function(psqi_time, assume = c('bedtime', 'waketime')){
    format_order <- c('H', 'H:M', 'Ip', 'I p', 'I:M p', 'I:Mp') #formats we expect time to be in
    #check if am/pm is specified
    if(grepl('[AaPp][mM]', psqi_time)){
        psqi_datetime <- lubridate::parse_date_time(psqi_time, orders = format_order)
    } else if (assume == 'bedtime'){
        psqi_datetime <- lubridate::parse_date_time(psqi_time, orders = format_order)
        if (is.na(psqi_datetime)){
            return(psqi_datetime)
        } else if (hour(psqi_datetime) > 5 & hour(psqi_datetime) < 12){
            psqi_datetime <- psqi_datetime + hours(12)
        } else if(hour(psqi_datetime) == 12) {
            psqi_datetime <- psqi_datetime - hours(12)
        }
    } else if (assume == 'waketime'){
        psqi_datetime <- lubridate::parse_date_time(psqi_time, orders = format_order)
        if (is.na(psqi_datetime)){
            return(psqi_datetime)
        } else if (hour(psqi_datetime) < 3){
            psqi_datetime <- psqi_datetime + hours(12)
        }
    }
    return(psqi_datetime)
}

#' Difference between sleep and wake times
#'
#' @param timesleep Object of class POSIXct on same date as \code{timewake}.
#' @param timewake Object of class POSIXct on same date as \code{timesleep}.
#'
#' @return Time elapsed between \code{timesleep} and \code{timewake}.
#' @import lubridate
#' @export
psqi_sleep_wake_diff <- function(timesleep, timewake){
    #timesleep and timewake should both be formatted by psqi_to_time_date
    if(!any(is.na(c(timesleep, timewake))) & 
       lubridate::pm(timesleep) & lubridate::am(timewake)) {
        timesleep <- timesleep - days(1)
    }
    return(difftime(timewake, timesleep))
}

#' Score PSQI component 4
#'
#' @param sleepbedratio A ratio in \code{[0, Inf)}.
#'
#' @return The corresponding PSQI score for component 4.
score_psqi_component_4 <- function(sleepbedratio){
    # (> .85: 0; .75-.84: 1; .65-.74: 2; <.65: 3)
    if (is.na(sleepbedratio)){
        score <- NA
    } else if (sleepbedratio >= .85){
        score <- 0
    } else if (sleepbedratio < .85 & sleepbedratio >= .75){
        score <- 1
    } else if (sleepbedratio < .75 & sleepbedratio >= .65){
        score <- 2
    } else if (sleepbedratio < .65) {
        score <- 3
    }
    return(score)
}

#' Score PSQI component 5
#'
#' @param psqi_5_items A vector of the requisite PSQI sub-items for item 5.
#'
#' @return The corresponding PSQI score for component 5.
score_psqi_component_5 <- function(psqi_5_items){
    if(length(psqi_5_items) != 8){
        stop('Incorrect number of items. Should be 8. Is: ', length(psqi_5_items))
    }
    psqi_5_sum <- sum(as.numeric(psqi_5_items))
    if (is.na(psqi_5_sum)){
        score <- NA
    } else if (psqi_5_sum == 0){
        score <- 0
    } else if (psqi_5_sum >= 1 & psqi_5_sum <= 9){
        score <- 1
    } else if (psqi_5_sum >= 10& psqi_5_sum <= 18){
        score <- 2
    } else if (psqi_5_sum >= 19){
        score <- 3
    }
    return(score)
}

#' Score a single PSQI questionnaire
#'
#' @param psqi_data A row of PSQI data (see details)
#' 
#' @details The vector of PSQI items must have the following names: 
#' PSQI_1_1_TEXT, PSQI_1_2_TEXT, PSQI_2_1_TEXT, PSQI_2_2_TEXT,
#' PSQI_3_1_TEXT, PSQI_3_2_TEXT, PSQI_4_1_TEXT, PSQI_4_2_TEXT,
#' PSQI_5_a, PSQI_5_b, PSQI_5_c, PSQI_5_d, PSQI_5_e, PSQI_5_f,
#' PSQI_5_g, PSQI_5_h, PSQI_5_i, PSQI_6, PSQI_7, PSQI_8, PSQI_9
#' 
#' @return A data frame with columns named scored_scale, 
#' score, n_items, n_missing, method.
score_single_psqi_row <- function(psqi_data){
    #component 1 
    component1 <- as.numeric(psqi_data$PSQI_9)
    c1_n_items <- sum(!is.na(c(psqi_data$PSQI_9)))
    c1_n_missing <- sum(is.na(c(psqi_data$PSQI_9)))
    
    #component 2
    psqi_2_weekdays <- scorequaltrics:::score_psqi_2(psqi_data$PSQI_2_1_TEXT)
    psqi_2_weekends <- scorequaltrics:::score_psqi_2(psqi_data$PSQI_2_2_TEXT)
    psqi_5a <- as.numeric(psqi_data$PSQI_5_a)
    component2_weekends <- (psqi_2_weekends + psqi_5a)
    component2_weekdays <- (psqi_2_weekdays + psqi_5a)
    c2_wkends_n_items <- sum(!is.na(c(psqi_data$PSQI_2_2_TEXT, psqi_data$PSQI_5_a)))
    c2_wkends_n_missing <- sum(is.na(c(psqi_data$PSQI_2_2_TEXT, psqi_data$PSQI_5_a)))
    c2_wkdays_n_items <- sum(!is.na(c(psqi_data$PSQI_2_1_TEXT, psqi_data$PSQI_5_a)))
    c2_wkdays_n_missing <- sum(is.na(c(psqi_data$PSQI_2_1_TEXT, psqi_data$PSQI_5_a)))
    
    #component 3
    psqi_4_weekdays <- scorequaltrics:::score_psqi_4(psqi_data$PSQI_4_1_TEXT)
    psqi_4_weekends <- scorequaltrics:::score_psqi_4(psqi_data$PSQI_4_2_TEXT)
    component3_weekdays <- psqi_4_weekdays
    component3_weekends <- psqi_4_weekends
    c3_wkdays_n_items <- sum(!is.na(c(psqi_data$PSQI_4_1_TEXT)))
    c3_wkdays_n_missing <- sum(is.na(c(psqi_data$PSQI_4_1_TEXT)))
    c3_wkends_n_items <- sum(!is.na(c(psqi_data$PSQI_4_2_TEXT)))
    c3_wkends_n_missing <- sum(is.na(c(psqi_data$PSQI_4_2_TEXT)))
    
    #component 4
    waketime_weekdays <- scorequaltrics:::psqi_to_time_date(psqi_data$PSQI_3_1_TEXT, assume = 'waketime')
    waketime_weekends <- scorequaltrics:::psqi_to_time_date(psqi_data$PSQI_3_2_TEXT, assume = 'waketime')
    bedtime_weekdays <- scorequaltrics:::psqi_to_time_date(psqi_data$PSQI_1_1_TEXT, assume = 'bedtime')
    bedtime_weekends <- scorequaltrics:::psqi_to_time_date(psqi_data$PSQI_1_2_TEXT, assume = 'bedtime')
    time_diff_weekdays <- scorequaltrics:::psqi_sleep_wake_diff(timesleep = bedtime_weekdays, 
                                                               timewake = waketime_weekdays) 
    time_diff_weekends <- scorequaltrics:::psqi_sleep_wake_diff(timesleep = bedtime_weekends, 
                                                               timewake = waketime_weekends) 
    hours_weekdays <- scorequaltrics:::score_psqi_4(psqi_data$PSQI_4_1_TEXT, return_value = 'hours')
    hours_weekends <- scorequaltrics:::score_psqi_4(psqi_data$PSQI_4_2_TEXT, return_value = 'hours')  
    sleepbedratio_weekdays <- hours_weekdays / as.numeric(time_diff_weekdays)
    sleepbedratio_weekends <- hours_weekends / as.numeric(time_diff_weekends)
    component4_weekdays <- scorequaltrics:::score_psqi_component_4(sleepbedratio_weekdays)
    component4_weekends <- scorequaltrics:::score_psqi_component_4(sleepbedratio_weekends)
    c4_wkdays_n_items <- sum(!is.na(c(psqi_data$PSQI_3_1_TEXT, psqi_data$PSQI_1_1_TEXT,
                                      psqi_data$PSQI_4_1_TEXT)))
    c4_wkdays_n_missing <- sum(is.na(c(psqi_data$PSQI_3_1_TEXT, psqi_data$PSQI_1_1_TEXT,
                                       psqi_data$PSQI_4_1_TEXT)))
    c4_wkends_n_items <- sum(!is.na(c(psqi_data$PSQI_3_2_TEXT, psqi_data$PSQI_1_2_TEXT,
                                      psqi_data$PSQI_4_2_TEXT)))
    c4_wkends_n_missing <- sum(is.na(c(psqi_data$PSQI_3_2_TEXT, psqi_data$PSQI_1_2_TEXT,
                                       psqi_data$PSQI_4_2_TEXT)))
    
    #component 5
    c5_items <- c(psqi_data$PSQI_5_b,
                  psqi_data$PSQI_5_c,
                  psqi_data$PSQI_5_d,
                  psqi_data$PSQI_5_e,
                  psqi_data$PSQI_5_f,
                  psqi_data$PSQI_5_g,
                  psqi_data$PSQI_5_h,
                  psqi_data$PSQI_5_i)
    component5 <- scorequaltrics:::score_psqi_component_5(c5_items)
    c5_n_items <- sum(!is.na(c5_items))
    c5_n_missing <- sum(is.na(c5_items))
    
    #component 6
    component6 <- as.numeric(psqi_data$PSQI_6)
    c6_n_items <- sum(!is.na(psqi_data$PSQI_6))
    c6_n_missing <- sum(is.na(psqi_data$PSQI_6))
    
    #component 7
    psqi_7 <- as.numeric(psqi_data$PSQI_7)
    psqi_8 <- as.numeric(psqi_data$PSQI_8)
    component7 <- (psqi_7+psqi_8+1) %/% 2
    c7_n_items <- sum(!is.na(c(psqi_data$PSQI_7, psqi_data$PSQI_8)))
    c7_n_missing <- sum(is.na(c(psqi_data$PSQI_7, psqi_data$PSQI_8)))
    
    #Total
    total_weekdays <- sum(component1, component2_weekdays,
                          component3_weekdays, component4_weekdays,
                          component5, component6,
                          component7)
    total_weekends <- sum(component1, component2_weekends,
                          component3_weekends, component4_weekends,
                          component5, component6,
                          component7)
    total_weekdays_n_items <- sum(c(c1_n_items,
                                    c2_wkdays_n_items,
                                    c3_wkdays_n_items,
                                    c4_wkdays_n_items,
                                    c5_n_items,
                                    c6_n_items,
                                    c7_n_items))
    total_weekdays_n_missing <- sum(c(c1_n_missing,
                                      c2_wkdays_n_missing,
                                      c3_wkdays_n_missing,
                                      c4_wkdays_n_missing,
                                      c5_n_missing,
                                      c6_n_missing,
                                      c7_n_missing))
    total_weekends_n_items <- sum(c(c1_n_items,
                                    c2_wkends_n_items,
                                    c3_wkends_n_items,
                                    c4_wkends_n_items,
                                    c5_n_items,
                                    c6_n_items,
                                    c7_n_items))
    total_weekends_n_missing <- sum(c(c1_n_missing,
                                      c2_wkends_n_missing,
                                      c3_wkends_n_missing,
                                      c4_wkends_n_missing,
                                      c5_n_missing,
                                      c6_n_missing,
                                      c7_n_missing))
    
    scored_scale <- c('psqi_c1', 
                      'psqi_c2_wkdays',
                      'psqi_c2_wkends',
                      'psqi_c3_wkdays', 
                      'psqi_c3_wkends', 
                      'psqi_c4_wkdays',
                      'psqi_c4_wkends',
                      'psqi_c5', 
                      'psqi_c6',
                      'psqi_c7',
                      'psqi_total_weekdays',
                      'psqi_total_weekends')
    score <- c(component1, 
               component2_weekdays,
               component2_weekends,
               component3_weekdays, 
               component3_weekends, 
               component4_weekdays,
               component4_weekends,
               component5, 
               component6,
               component7,
               total_weekdays,
               total_weekends)
    n_items <- c(c1_n_items,
                 c2_wkdays_n_items,
                 c2_wkends_n_items,
                 c3_wkdays_n_items,
                 c3_wkends_n_items,
                 c4_wkdays_n_items,
                 c4_wkends_n_items,
                 c5_n_items,
                 c6_n_items,
                 c7_n_items,
                 total_weekdays_n_items,
                 total_weekends_n_items)
    n_missing <- c(c1_n_missing,
                   c2_wkdays_n_missing,
                   c2_wkends_n_missing,
                   c3_wkdays_n_missing,
                   c3_wkends_n_missing,
                   c4_wkdays_n_missing,
                   c4_wkends_n_missing,
                   c5_n_missing,
                   c6_n_missing,
                   c7_n_missing,
                   total_weekdays_n_missing,
                   total_weekends_n_missing)
    scored_row_df <- data.frame(scored_scale = scored_scale,
                                score = score,
                                n_items = n_items,
                                n_missing = n_missing)
    return(scored_row_df)
}


#' Title
#'
#' @param dataDF A long data frame with columns named: [PID col], item, and value
#' @param pid_col The name of the participant ID column.
#'
#' @return A data frame of scored scales with columns scale_name, scored_scale, [PID col], 
#' score, n_items, n_missing, method.
#' @import dplyr
#' @import tidyr
#' @export
score_psqi <- function(dataDF, pid_col='ID'){
    scoredDF <- dataDF %>%
        dplyr::group_by_at(pid_col) %>%
        dplyr::select_at(c(pid_col, 'item', 'value')) %>%
        dplyr::mutate(value = ifelse(value == "", NA, value)) %>%
        tidyr::spread(item, value) %>%
        dplyr::do({
            scored_psqi <- scorequaltrics:::score_single_psqi_row(.)
        })
    scoredDF$scale_name <- 'PSQI'
    scoredDF$method <- 'psqi'
    return(scoredDF)
}