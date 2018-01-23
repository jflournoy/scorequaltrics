#' Get survey data
#'
#' @param surveysDF A data.frame as returned by the \code{\link{get_surveys}}
#' @param creds A named character with \code{creds$user} and \code{creds$token}
#' @param pid_col A character with the column name specifying user ID.
#'
#' @return Returns a long format data.frame of survey data with names "SID variable name", "item", "value", "survey_name"
#' @import dplyr
#' @import tidyr
#' @export
get_survey_data<-function(surveysDF, creds, pid_col='ID'){
  survey_data<-surveysDF %>%
    group_by(SurveyID) %>%
    do(
      survey_name=.$SurveyName[[1]],
      survey_data=get_survey_responses(
        creds,
        surveyid=.$SurveyID[[1]])
    )
  long_survey_data<-survey_data %>%
    filter(dim(survey_data)[1]>0) %>%
    do({
      gather_cols<-names(.$survey_data)[!grepl(pid_col,names(.$survey_data))]
      aDF<-gather_(.$survey_data,
                   'item',
                   'value',
                   gather_cols)
      aDF$survey_name<-.$survey_name
      aDF
    })
  long_survey_data
}

#' Get rubrics
#'
#' @param rubric_filenames Data frame with column of file paths named "file".
#' @param type "scoring" for special handling of scoring rubrics, or "recoding"
#' for special handling of recoding rubrics.
#' @param source Unused, default's to 'csv' for now.
#'
#' @return If \code{type='scoring'}, returns a long data frame of rubrics with names:
#' "file"           "data_file_name" "scale_name"     "column_name"
#' "reverse"        "transform"      "scored_scale"   "include"
#' "min"            "max". Otherwise, it returns the transforming rubric with names: 
#' "file"           "data_file_name" "scale_name"     "column_name"    "answer"        
#' "response"       "score"    
#' @import dplyr
#' @import tidyr
#' @export
get_rubrics <- function (rubric_filenames, type = 'scoring', source = "csv") 
{
    if(! type %in% c('scoring', 'recoding')){
        stop('Option `type` must be either "scoring" or "recoding"')
    }
    
    csv_rubrics <- rubric_filenames %>% 
        mutate(file = as.character(file)) %>% 
        group_by(file) %>% do({
            data_frame(rubric = list(read.csv(.$file[[1]], header = T, 
                                              stringsAsFactors = F)))
        })
    
    rubric_data_long <- csv_rubrics %>% 
        group_by(file) %>% 
        do({
            thisDF <- .$rubric[[1]]
  
            names(thisDF) <- tolower(gsub(" ", 
                                          "_", 
                                          gsub("\\.", 
                                               "_", names(thisDF))))
            if(type == 'scoring'){
                aDF <- gather(thisDF, 
                              scored_scale, 
                              include, 
                              -one_of("data_file_name", 
                                      "scale_name", "column_name", "reverse", "transform", 
                                      "min", "max")) %>% 
                    mutate_all(funs(as.character))
            } else if (type == 'recoding') {
                
                aDF <- thisDF %>% 
                    mutate_all(funs(as.character))
            }
            aDF
        })
    
    rubric_data_long
}

#' @export
get_uncoercibles <- function(dataDF){
    dataDT <- as.data.table(dataDF)
    dataDT <- dataDT[, checkcol := class(type.convert(value, as.is=T)) == 'character', by = .(item,value)]
    dataDT <- dataDT[checkcol == T]
    return(dataDT[,checkcol := NULL])
}

#' Get items from data file that exist in a rubric
#'
#' @param dataDF A long-form data.frame as would be returned by \code{\link{get_survey_data}}
#' @param rubricDF A rubric data.frame as would be returned by \code{\link{get_rubrics}}
#'
#' @return returns a long-format data.frame with just items appearing in \code{rubricDF}
#' @import data.table
#' @export
get_items_in_rubric <- function(dataDF, rubricDF){
  dataDT <- as.data.table(dataDF)
  rubricCols <- rubricDF$column_name[rubricDF$include %in% c(1, "1", "sum", "prod")]
  smallDF <- as.data.frame(dataDT[item %in% rubricCols])
  return(smallDF)
}

#' Score items
#'
#' @param item_values item_values
#' @param scoring_methods scoring_methods
#' @param na.rm na.rm
#' @param mean.na.rm mean.na.rm
#' @param scale_name scale_name
#' @param scored_scale scored_scale
score_items<-function(item_values,scoring_methods,na.rm=F,mean.na.rm=T,scale_name='', scored_scale=''){
  # item_values should be a vector of numbers
  # scoring_methods should be a function that takes a vector, or '1'
  #check that all elements in `scoring` are the same

  if(!all(scoring_methods[1]==scoring_methods))
    stop(paste0('Scoring methods not all the same. Check that you\'re\n',
                'grouping rows correctly, and that the rubric is correct.\n',
                '(scale name is ',scale_name,', scored scale is ',scored_scale,')\n',
                paste(scoring_methods, collapse='\n')))
  scoring_method<-unique(scoring_methods)
  if (scoring_method==1){
    scoring_func<-mean
    na.rm=mean.na.rm
  }
  else
    scoring_func<-try(get(scoring_method))
  if (class(scoring_func)=='try-error')
    stop(paste('Scoring method "',scoring_method,'" not found. (scale name is ',scale_name,')'))
  if(na.rm)
    do_for_na<-na.exclude
  else
    do_for_na<-na.pass
  scoring_func(do_for_na(item_values))
}

#' Reverse score
#'
#' @param item_values item_values
#' @param min min
#' @param max max
reverse_score<-function(
  item_values,
  min=stop('Must specify min and max score.'),
  max=stop('Must specify min and max score.')){
  rev_items<-max+min-item_values
  rev_items
}

#' RPI flip
#'
#' @param item_values item_values
rpi_flip<-function(item_values){
  one_indexes<-item_values==1
  two_indexes<-item_values==2
  item_values[one_indexes]<-2
  item_values[two_indexes]<-1
  item_values
}

#' transform scores
#'
#' @param item_values item_values
#' @param transformation transformation
#' @param min min
#' @param max max
transform_scores<-function(item_values,transformation,min=NA,max=NA){
  #`transformation` should be a function defined in terms of x that will
  #	be applied to the vector of item_values.
  #Conditional logic will be extracted for subsetting, also in terms of x.
  #`min` and `max` will be passed to `reverse_score` and `pomp_score`.
  #check that all elements in `transformation` are the same
  if(!all(transformation[1]==transformation))
    stop('Scoring methods not all the same. Check that you\'re
         grouping rows correctly, and that the rubric is correct.')
  transformation<-unique(transformation)
  if(regexpr('^if *(\\(.*\\))( | *\\{).*',transformation) != -1){
    logic<-sub('^if *(\\(.*\\))( | *\\{).*','\\1',transformation)
    aFunction<-sub('^if *(\\(.*\\))( | *\\{)(.*)','\\3',transformation)
  }
  else {
    logic<-NULL
    aFunction<-transformation
  }
  if(grepl('reverse_score$',aFunction)){
    aFunction<-paste(aFunction,'(x,min,max)')
  }
  if(!grepl('^function(x)',aFunction)){
    aFunction<-paste('function(x) ',aFunction)
  }
  aFunction<-eval(parse(text=aFunction))
  x<-item_values
  if(is.null(logic)){
    aFunction(x)
  }
  else {
    ifelse(
      eval(parse(text=logic)),
      aFunction(x),
      x)
  }
}

#' Score questionnaire
#'
#' @param dataDF A data.frame as returned by \code{\link{get_survey_data}}
#' @param rubricsDF A data.frame as returned by \code{\link{get_rubrics}}
#' @param psych A flag specifying whether \code{\link[psych]{psych-package}} should be used for scoring. Default is \code{FALSE} but you should probably set it to \code{TRUE} unless you're using this for a DSN lab project
#' @param ... Other arguments passed to one of the scoring engines.
#'
#' @return A long-form data.frame with scale scores.
#' @export
score_questionnaire<-function(dataDF, rubricsDF, psych = FALSE, ...){
  if(psych){
    score_questionnaire_psych(dataDF, rubricsDF, ...)
  } else {
    score_questionnaire_dsn(dataDF, rubricsDF, ...)
  }
}

#' Score questionnaire dsn
#'
#' @param dataDF dataDF
#' @param rubricsDF rubricsDF
#' 
#' @import dplyr
#' @import tidyr
score_questionnaire_dsn <- function(dataDF,rubricsDF){
  #Takes long-form question data and rubric data, and a logical
  # value specifying whether to use _part2 rubrics.
  #Returns scored scales.
  scores_with_scoring_params<-rubricsDF  %>%
    left_join(
      dataDF,
      by=c(
        #"data_file_name" = "survey_name",
        "column_name" = "item")) %>%
    filter(!(include %in% c(0,NA,'0','NA',''))) # this filters the rubrics

  nonNumeric_items = scores_with_scoring_params %>% filter(include %in% 'I')

  transform_scored<-
    scores_with_scoring_params %>%
    group_by(column_name) %>%
    mutate(
      value=as.numeric(
        ifelse(
          !(transform %in% c(0,NA,'')),
          transform_scores(
            value,
            transform,
            min=as.numeric(min[[1]]),
            max=as.numeric(max[[1]])),
          value)))

  reverse_scored<-
    transform_scored %>%
    group_by(column_name) %>%
    mutate(
      value=as.numeric(
        ifelse(
          reverse %in% 1,
          reverse_score(
            value,
            min=as.numeric(min[[1]]),
            max=as.numeric(max[[1]])),
          value))) %>%
    filter(!include %in% 'I')

  non_numeric <- nonNumeric_items %>%
    mutate(na.rm=F) %>%
    group_by(scale_name,scored_scale,SID) %>%
    summarise(
      score=score_items(value,include,na.rm=na.rm[[1]],scale_name=scale_name[[1]],scored_scale=scored_scale[[1]]),
      n_items=sum(!is.na(value)),
      n_missing=sum(is.na(value)),
      method=unique(include))

  scored<-reverse_scored %>%
    mutate(na.rm=F) %>%
    group_by(scale_name,scored_scale,SID) %>%
    summarise(
      score=score_items(value,include,na.rm=na.rm[[1]],scale_name=scale_name[[1]],scored_scale=scored_scale[[1]]),
      n_items=sum(!is.na(value)),
      n_missing=sum(is.na(value)),
      method=unique(include)) %>%
    mutate(
      score=as.character(score)) %>%
    bind_rows(non_numeric,.)
}

#' Score step one and two
#'
#' @param dataDF A data.frame as returned by \code{\link{get_survey_data}}
#' @param rubricsDF A data.frame as returned by \code{\link{get_rubrics}}
#' @return A long-form data.frame with scale scores.
#' 
#' @import dplyr
#' @import tidyr
#' 
#' @export
score_step_one_and_two<-function(dataDF,rubricsDF){
  #Do step one of the scoring...
  rubricsDF_part1<-rubricsDF %>% filter(!grepl('part2',scale_name))
  scored<-score_questionnaire(dataDF,rubricsDF_part1)
  #To do step 2, we need to get our names right, and make sure we
  # are only using the necessary rows.
  rubricsDF_part2<-rubricsDF %>% ungroup %>%
    filter(grepl('part2',scale_name)) %>%
    mutate(column_name=tolower(column_name)) %>%
    dplyr::select(-file,-data_file_name) %>%
    mutate(data_file_name=scale_name) %>%
    mutate(data_file_name=sub('(.*)_part2','\\1',data_file_name))
  part2_cols<-unique(rubricsDF_part2$column_name)
  scored.part1<-scored %>% filter(!scored_scale %in% part2_cols)
  dataDF.part2<-scored %>%
    filter(scored_scale %in% part2_cols) %>%
    rename(survey_name=scale_name,item=scored_scale,value=score)
  scored.part2<-score_questionnaire(dataDF.part2,rubricsDF_part2)
  all_scored<-bind_rows(scored.part1,scored.part2)
  all_scored
}

#' Clean duplicates
#'
#' @param longDF longDF
#' @param pid_col pid_col
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export
clean_dupes <- function(longDF, pid_col = 'ID'){
  cleanedDF <- longDF %>% ungroup() %>%
    group_by_at(.vars = c(pid_col, 'item')) %>% #group by rows with the same item name and SID
    do({ #for each group
      values <- na.exclude(as.numeric(.$value)) #get values in `values` column, make numeric (which yields NA if value==''), and exclude NA (no info, and no possible conflict)
      dropped <- FALSE #Keep track of whether we have to drop this observation due to conflicts
      if(length(values>0) && all(values==values[[1]])){ #if there are multiple values, but they agree (all values are equal to the first value)
        bestValue <- values[[1]] #then just take the first value
      } else {
        #If after excluding missing values, the values are not all the same, this means
        #there must be more than one value, and there are differences between them.
        #There's not a good heuristic here -- this means that either they took the
        #questionnaire twice and answered differently (in which case, which is the 'right' answer),
        #or there is a problem with the SID column, and we need to figure out if
        #there is a miscoding of SID. Either way, there is some manual intervention needed so
        #we set the value to NA, and flag this observation as dropped.
        bestValue <- NA
        dropped <- TRUE
      }
      adf <- .[1,] #retain all the info from the first row of this group
      adf$old.value <- list(values) #wrap up all the values from the group, and save them to aid error checking
      adf$value <- bestValue #set the value to what we decided above
      adf$dropped <- dropped #set the dropped flag
      adf #return the new data frame, which should now be 1 row per item per SID.
    })
}

#' score questionnaire psych
#'
#' @param dataDF dataDF
#' @param rubricsDF rubricsDF
#' @param scale_name scale_name
#' @param return_with_data return_with_data
#'
#' @import psych
#' @import dplyr
#' @import tidyr
score_questionnaire_psych <- function(dataDF, rubricsDF, scale_name = NULL, return_with_data = FALSE){
  require(psych)
  if(!is.null(scale_name)){
    rubricsDF <- filter(rubricsDF, scale_name == scale_name)
  }

  if('include' %in% names(rubricsDF)){
    rubricsDF <- ungroup(filter(rubricsDF, include == 1))
  }

  keys_list_l <- select(mutate(rubricsDF,
                               rscore_col_name = paste0(ifelse(reverse == 1, '-', ''), column_name)),
                        rscore_col_name, scored_scale)

  scored_scale_names <- unique(keys_list_l$scored_scale)
  key_list <- lapply(scored_scale_names, function(x){
    keys_list_l$rscore_col_name[keys_list_l$scored_scale == x]
  })
  names(key_list) <- scored_scale_names

  dataDF_w <- spread(select(dataDF, SID, item, value),
                     item, value)
  scored_scales <- scoreItems(key_list, dataDF_w)
  if(return_with_data){
    scored_scales$input_data <- dataDF_w
  }
  rownames(scored_scales$scores) <- dataDF_w$SID
  return(scored_scales)
}


#' Recode repsonses
#' 
#' Recodes responses according to a rubric.
#' 
#' @param dataDF A long data frame.
#' @param recoding_rubric A recoding rubric with columns `column_name`,
#'   `response`, and `score`
#'   
#' @return the data frame passed to it, with each value in `value` replaced with
#'   the recoded value in `score` from the template.
#' @export
#' @import data.table
recode_responses <- function(dataDF, recoding_rubric){
    recoding_rubric_reduced <- select(ungroup(recoding_rubric),
                                      column_name, response, score)
    
    dataDF_recoding <- dataDF %>%
        left_join(recoding_rubric_reduced, 
                  by = c("item" = "column_name", "value" = "response")) %>%
        as.data.table
    
    to_recode <- dataDF_recoding %>%
        ungroup() %>%
        filter(!is.na(score),
               score != value) %>%
        summarize(N_recoded = n())
    
    dataDF_recoding[, value := ifelse(!is.na(score),
                                      score,
                                      value)]
    dataDF_recoding[, score := NULL]
    
    message('A total of ', to_recode$N_recoded, ' items recoded.')
    
    return(as.data.frame(dataDF_recoding))
}
