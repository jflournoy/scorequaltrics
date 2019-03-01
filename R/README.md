# score_pds

This scores the PDS in accordance with the method alluded to in Shirtcliff, Dahl, and Pollak (2009).

## Getting started
You will need to install this package by running...

```{r}
install.packages("devtools")
devtools::install_github("jflournoy/qualtrics")
```

## Set up

### Label variables
The script expects the item names to vary by gender, with all variables for females starting with "F", and all variables for males starting with "M".

The survey abbreviation (PDS) is also expected to be a part of the item name. Variables can be in the format "PDS_F1" or "F1_PDS."

Item name| Corresponding question
----|---
F1 or M1| 'growth in height'
F2 or M2| 'growth of body hair'
F3 or M3| 'noticed skin changes'
F4 or M4| females: 'breasts begun to grow', males: 'deepening of voice'

For males only:

Item name| Corresponding question
---|---
M5 | male: 'grew hair on face'

For females only:

Item name| Corresponding question
---|---
F6 | 'began to menstruate'*

* Note, the script expects **item F6** to be coded **a 1 for 'no' and 4 for 'yes'**.

Thus, each female participant should have a value for each of the following items: F1, F2, F3, F4, and F6. Meanwhile, male participants should have a value for each of the following items: M1, M2, M3, M4, M5.


### Mixed or single-gender input data
* If you have male and female subjects in the same dataset, male participants will have 'NA's for items F1-F4 and F6 and female participants will have 'NA's for items M1-M5.
* Another option is to score female and male participants separately. In this case, your input data for males should only contain items M1-M5; female input data should only contains items F1-F4 and F6.

### Data format
* The data need to be in [long, tidy format](http://r4ds.had.co.nz/tidy-data.html)

Expected column title | Contents
---|---
SID| subject identification numbers
item| names of items, formatted "PDS_F1" or "F1_PDS" (character)
value | participant response (numeric)
survey_name | "PDS" (character)

## Deploying the script

```{r}
score_pdss(long_survey_data_filtered, gender_mix='mf', gendercode=c(m=1,f=0))
```
* gender can be 'mixed', 'm', or 'f', where 'm' and 'f' assumes homogeneous gender of either males of females
