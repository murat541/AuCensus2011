
#=============================================================================
# Variable Splitting
# - splitting variables from named columns.
# - e.g. Table B10 has column names such as:
#        Bosnia_and_Herzegovina_Year_of_arrival_Before_1941
# - look to splitting this into into a number of variables
# - e.g.  origin = Bosnia_and_Herzegovina   year_range = <1941
#=============================================================================

#' Variable Split by Gender
#'
#' Split the gender from a measure variable into it's own column
#'
#' Long description here.
#'
#' @param colname vector of column names to split
#'
split_bygender <- function(longdata) {
    longdata %>% mutate(colname = as.character(colname)) %>%
        mutate(stat   = str_match(colname, pattern="(.*?)_(Males|Females|Persons)$")[,2],
               gender = str_match(colname, pattern="(.*?)_(Males|Females|Persons)$")[,3]) %>%
        select(region_id, colname, stat, gender, count)
}

#' operates on a single string at a time.
split_b01_columns <- function(column_names) {
    column_names <- as.character(column_names)
    labels <- dplyr::data_frame(colname=column_names, stat1="", stat2="", gender="")

    patterns <- c(
        '^(Total_Persons|Australian_citizen)()_(Males|Females|Persons)$',
        '^(Age_group)s_(.*)_years.*_(Males|Females|Persons)$',
        '^(Counted_on_Census_Night)_(.*)_(Males|Females|Persons)$',
        '^(Indigenous_Persons)_(.*)_(Males|Females|Persons)$',
        '^(Birthplace)_(.*)_(Males|Females|Persons)$',
        '^(Language_spoken_at_home)_(.*?)_(Males|Females|Persons)$',
        '^(Age_of_Persons_attending_an_educational_institution)_(.*)_years.*_(Males|Females|Persons)',
        '^(Highest_year_of_school_completed)_(.*?)_(Males|Females|Persons)$',
        '^(Count_of_Persons)_in_(.*)_(Males|Females|Persons)$'
    )

    for (pattern in patterns) {
        ii <- str_detect(column_names, pattern)
        match <- str_match(column_names[ii], pattern)
        labels$stat1[ii]  <- match[,2]
        labels$stat2[ii]  <- match[,3]
        labels$gender[ii] <- match[,4]
    }
    labels
}

split_b02_columns <- function(column_names) {
    column_names <- as.character(column_names)
    labels <- dplyr::data_frame(colname=column_names, stat1="", stat2="")

    patterns <- c(
        '^(Counted_at_home_on_Census_Night)_Age_(.*)_years',
        '^(Counted_at_home_on_Census_Night)_(Total)$',

        '^Visitor_from_(Same_Statistical_Area_Level_2_SA2)_Age_(.*)_years',
        '^Visitor_from_(Same_Statistical_Area_Level_2_SA2)_(Total)',

        '^Visitor_from_Different_SA2_in_(.*)_Age_(.*)_years',
        '^Visitor_from_Different_SA2_in_(.*)_(Total)$',

        '^Visitor_from_(Different_SA2)_in_Total_Age_(.*)_years',
        '^Visitor_from_(Different_SA2)_in_Total_(Total)$',

        '^Visitor_from_(Total_visitors)_Age_(.*)_years',
        '^Visitor_from_(Total_visitors)_(Total)$',
        '^(Total)_Age_(.*)_years',
        '^(Total)_(Total)$'
    )

    for (pattern in patterns) {
        ii <- str_detect(column_names, pattern)
        match <- str_match(column_names[ii], pattern)
        labels$stat1[ii]  <- match[,2]
        labels$stat2[ii]  <- match[,3]
    }
    labels
}

df <- read_abs('BCP', 'B03', 'AUS', long=TRUE)

