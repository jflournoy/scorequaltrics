#' Score pubertal development scale
#'
#' @param long_survey_data_filtered data
#' @param gender_mix gender_mix
#' @param gendercode gendercode
#'
#' @return different version of PDS
#' @export
#'
score_pdss <- function(long_survey_data_filtered, gender_mix='mf', gendercode=c(m=1,f=0)){
    #`gender` can be c('mixed','m','f') where 'm' and 'f' assumes
    # homogeneous gender of either males of females.
    #
    # Takes a long survey data frame with colums
    #   "SID"         "item"        "value"       "survey_name"
    # !! Important: item names must be in the form of
    # PDS_F1 or PDS_M1 to be recognized.
    # 
    # This returns a data frame that should correspond
    # to the scored_scales data frame, with columns:
    #   "scale_name"   "scored_scale" "SID"          "score"        "n_items" 
    #   "n_missing"    "method"      
    
    #Correspondences to the shirtcliff script:	
    #VARIABLE LABELS peta F1 M1 'growth in height'.
    #VARIABLE LABELS petb F2 M2 'growth of body hair'.
    #VARIABLE LABELS petc F3 M3 'noticed skin changes'.
    #VARIABLE LABELS petd F4 M4 'breasts begun to grow/ deepening of voice'.
    #VARIABLE LABELS mpete M5 'male grow hair on face'.
    #VARIABLE LABELS fpete F6 'female begun to menstruate'.
    
    #If you have different names you COULD substitute in here. This
    #would be a good place to start making this code more
    #generalizable	
    qnames <- c('F1'='height',
                'M1'='height',
                'F2'='hair',
                'M2'='hair',
                'F3'='skin',
                'M3'='skin',
                'F4'='bv',
                'M4'='bv',
                'M5'='face',
                'F6'='menstruate',
                'Gender'='Gender')
    names(qnames) <- paste0('PDS_', names(qnames))
    
    #This gets the PDS data from the long_survey_data,
    #adds more readable item names,
    #and removes duplicate entries (or sets them to NA if there
    #are conflicts, e.g., a duplicate entry with a different answer).
    #Also, remove rows depending on gender.
    pds_data <- 
        long_survey_data_filtered %>%
        filter(item %in% c(names(qnames), 'Gender'), !SID %in% c('', NA)) %>%
        group_by(SID, item) %>% #group by rows with the same item name and SID
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
        }) %>%
        group_by(SID) %>%
        do({
            if(gender_mix %in% c('m','f')){
                gender=gendercode[gender_mix]
            } else {
                gender <- .$value[.$item=='PDS_Gender'] 
            }
            if(is.null(gender) || length(gender %in% c(NA, '')) == 0 || gender %in% c(NA, '')) {
                gender <- .$value[.$item=='Gender']
            }
            if(gender==gendercode['m']){ #male
                thisDF <- .[grepl('PDS_M', .$item), ]
            } else if (gender==gendercode['f']){ #female
                thisDF <- .[grepl('PDS_F', .$item), ]
            }
            thisDF$gender <- gender
            thisDF
        })
    
    #VARIABLE LABELS peta F1 M1 'growth in height'.
    #VARIABLE LABELS petb F2 M2 'growth of body hair'.
    #VARIABLE LABELS petc F3 M3 'noticed skin changes'.
    #VARIABLE LABELS petd F4 M4 'breasts begun to grow/ deepening of voice'.
    #VARIABLE LABELS mpete M5 'male grow hair on face'.
    #VARIABLE LABELS fpete F6 'female begun to menstruate'.
    female_pdss <- function(height, hair, skin, breasts, menstruate, na.rm=F){
        hair.f <- c(1, 2, 4, 5)[hair]
        skin.f <- c(1, 2, 4, 5)[skin]
        adren.f <- apply(data.frame(hair.f, skin.f), 1, mean, na.rm=T)
        adren.f2 <- ifelse(adren.f==1.5,
                           hair,
                           c('1'=1,
                             '2'=2,
                             '2.5'=3,
                             '3'=3,
                             '3.5'=4,
                             '4'=4,
                             '4.5'=5,
                             '5'=5,
                             '5.5'=5 #shirtcliff script includes this for some reason
                           )[as.character(adren.f)])
        height.f <- c(1, 2, 3, 5)[height]
        breasts.f <- c(1, 3, 4, 5)[breasts]
        menstruate.f <- c(1, NA, NA, 5)[menstruate]
        gonad.f <- apply(data.frame(height.f, breasts.f), 1, mean, na.rm=T)
        gonad.f2 <- ifelse(menstruate.f==1,
                           c('1'=1,
                             '1.5'=1,
                             '2'=2,
                             '2.5'=2,
                             '3'=3,
                             '3.5'=3,
                             '4'=3,
                             '4.5'=4,
                             '5'=4 
                           )[as.character(gonad.f)],
                           ifelse(menstruate.f==5,
                                  c('1'=2,
                                    '1.5'=3,
                                    '2'=4,
                                    '2.5'=4,
                                    '3'=4,
                                    '3.5'=5,
                                    '4'=5,
                                    '4.5'=5,
                                    '5'=5 
                                  )[as.character(gonad.f)],
                                  NA))
        pdss <- apply(data.frame(adren.f2, gonad.f2), 1, mean, na.rm=T)
        adren.n_missing <- apply(data.frame(hair.f, skin.f), 1, 
                                 function(x) sum(is.na(x)))
        gonad.n_missing <- apply(data.frame(height.f, breasts.f, menstruate.f), 1, 
                                 function(x) sum(is.na(x)))
        pdss.n_missing <- adren.n_missing + gonad.n_missing
        retDF <- data.frame(adren.score=adren.f2, adren.n_items=2-adren.n_missing, adren.n_missing,
                            gonad.score=gonad.f2, gonad.n_items=3-gonad.n_missing, gonad.n_missing,
                            pdss.score=pdss, pdss.n_items=5-pdss.n_missing, pdss.n_missing)
        return(retDF)
    }
    
    #VARIABLE LABELS peta F1 M1 'growth in height'.
    #VARIABLE LABELS petb F2 M2 'growth of body hair'.
    #VARIABLE LABELS petc F3 M3 'noticed skin changes'.
    #VARIABLE LABELS petd F4 M4 'breasts begun to grow/ deepening of voice'.
    #VARIABLE LABELS mpete M5 'male grow hair on face'.
    #VARIABLE LABELS fpete F6 'female begun to menstruate'.
    male_pdss <- function(height, hair, skin, voice, face){
        hair.m <- c(1, 2, 4, 5)[hair]
        skin.m <- c(1, 2, 3, 4)[skin]
        adren.m <- apply(data.frame(hair.m, skin.m), 1, mean)
        adren.m2 <- apply(data.frame(adren.m, skin.m, hair.m), 1, function(aRow){
            skin <- aRow['skin.m']
            hair4 <- aRow['hair.m'] == 4
            adren.m2 <- c('1'=1,
                          '1.5'=skin[[1]],
                          '2'=2,
                          '2.5'=2+hair4[[1]], #extra point if hair==4
                          '3'=3,
                          '3.5'=4,
                          '4'=4,
                          '4.5'=5,
                          '5'=5,
                          '5.5'=5 #shirtcliff script includes this for some reason
            )[as.character(as.character(aRow['adren.m']))]
            return(adren.m2)
        })
        height.m <- c(1, 3, 4, 5)[height]
        voice.m <- c(1, 2, 3, 5)[voice]
        gonad.m <- apply(data.frame(height.m, voice.m), 1, mean)
        gonad.m2 <- apply(data.frame(gonad.m, voice.m, face), 1, function(aRow){
            with(as.list(aRow),{
                if (is.na(gonad.m)) return(NA)
                if (gonad.m > 4) return(5)
                if (gonad.m==3) return(3)
                if (is.na(face)) return(NA)
                if (gonad.m==1 & face==1) return(1)
                if (gonad.m==1 & face > 1) return(2)
                if (gonad.m==1.5 & face==1) return(1)
                if (gonad.m==1.5 & face > 1) return(2)
                if (gonad.m==2 & face > 1) return(3)
                if (gonad.m==2.5 & face==1) return(2)
                if (gonad.m==2.5 & face > 1) return(3)
                if (gonad.m==3.5 & face==1) return(4)
                if (gonad.m==3.5 & face==2) return(4)
                if (gonad.m==3.5 & face > 2) return(5)
                if (gonad.m==4 & face==1) return(4)
                if (gonad.m==4 & face==2) return(4)
                if (gonad.m==4 & face > 2) return(5)
                if (is.na(voice.m)) return(NA)
                if (gonad.m==2 & face==1 & voice.m==1) return(1)
                if (gonad.m==2 & face==1 & voice.m > 1) return(2)
            })
        })
        pdss <- apply(data.frame(adren.m2, gonad.m2), 1, mean)
        adren.n_missing <- apply(data.frame(hair.m, skin.m), 1, 
                                 function(x) sum(is.na(x)))
        gonad.n_missing <- apply(data.frame(height.m, voice.m, face), 1, 
                                 function(x) sum(is.na(x)))
        pdss.n_missing <- adren.n_missing + gonad.n_missing
        retDF <- data.frame(adren.score=adren.m2, adren.n_items=2-adren.n_missing, adren.n_missing,
                            gonad.score=gonad.m2, gonad.n_items=3-gonad.n_missing, gonad.n_missing,
                            pdss.score=pdss, pdss.n_items=5-pdss.n_missing, pdss.n_missing)
        return(retDF)
    }
    
    scored_pdss <- pds_data %>% ungroup %>% 
        mutate(item.new=qnames[item]) %>%
        dplyr::select(SID, gender, value, item.new) %>%
        spread(item.new, value) %>%
        group_by(gender) %>%
        do({
            if(.$gender[[1]]==gendercode['m']){
                retDF <- male_pdss(.$height, .$hair, .$skin, .$bv, .$face)
                retDF$gender.score <- gendercode['m']
                retDF$SID <- .$SID
            } else if(.$gender[[1]]==gendercode['f']){
                retDF <- female_pdss(.$height, .$hair, .$skin, .$bv, .$menstruate)
                retDF$gender.score <- gendercode['f']
                retDF$SID <- .$SID
            } else {
                retDF <- .[, c('SID', 'gender')]%>%rename(gender.score=gender)
            }
            retDF
        }) %>% ungroup %>%
        dplyr::select(-gender) %>%
        gather(key, value, -SID) %>% #distinct(key)
        extract(key, c('scored_scale', 'attribute'), '(\\w+)\\.(\\w+)') %>%
        spread(attribute, value) %>%
        mutate(scale_name='PDSS', method='pdss', score=as.character(score)) %>%
        dplyr::select(scale_name, scored_scale, SID, 
                      score, n_items, n_missing, method)
    
    return(scored_pdss)
}
