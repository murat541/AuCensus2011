
#=============================================================================
# Variable Splitting
# - splitting variables from named columns.
# - e.g. Table B10 has column names such as:
#        Bosnia_and_Herzegovina_Year_of_arrival_Before_1941
# - look to splitting this into into a number of variables
# - e.g.  origin = Bosnia_and_Herzegovina   year_range = <1941
#=============================================================================


#-----------------------------------------------------------------------------
# split a vector of column names given a table configuration.
# config$patterns : regexes to match and split the names
# config$stats    : the names of the new split variables
#                   number of stats must match the number of captured groups
#                   in the set of patterns.
#-----------------------------------------------------------------------------
split_column_names <- function(column_names, config) {
    column_names <- as.character(unique(column_names))
    labels <- dplyr::data_frame(colname=column_names)
    # initialise blank columns for dumping the split variables
    for (stat in config$stats) {
        labels[[stat]] <- ''
    }

    # match the patterns and split the column names
    for (pattern in config$patterns) {
        # Find the ones which match the pattern
        ii    <- str_detect(column_names, pattern)
        # Split on those that match the pattern
        match <- str_match(column_names[ii], pattern)
        # assign the vars into the labels df
        mcount <- 2
        for (stat in config$stats) {
            labels[[stat]][ii] <- match[,mcount]
            mcount <- mcount + 1
        }
    }

    if (!is.null(config[['extra_manipulation']])) {
        labels <- labels %>% (config[['extra_manipulation']])
    }

    labels
}

#-----------------------------------------------------------------------------
# Configs
#-----------------------------------------------------------------------------

b01config <- list(
    table    = 'B01',
    patterns = c(
        '^(Total_Persons|Australian_citizen)()_(Males|Females|Persons)$',
        '^(Age_group)s_(.*)_years.*_(Males|Females|Persons)$',
        '^(Counted_on_Census_Night)_(.*)_(Males|Females|Persons)$',
        '^(Indigenous_Persons)_(.*)_(Males|Females|Persons)$',
        '^(Birthplace)_(.*)_(Males|Females|Persons)$',
        '^(Language_spoken_at_home)_(.*?)_(Males|Females|Persons)$',
        '^(Age_of_Persons_attending_an_educational_institution)_(.*)_years.*_(Males|Females|Persons)',
        '^(Highest_year_of_school_completed)_(.*?)_(Males|Females|Persons)$',
        '^(Count_of_Persons)_in_(.*)_(Males|Females|Persons)$'
    ),
    stats = c('stat1', 'stat2', 'gender')
)

b03config <- list(
    table    = 'B03',
    patterns = c(
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
    ),
    stats = c('stat1', 'stat2')
)

b04config <- list(
    table    = 'B04',
    patterns = c(
        '^Age_years_(.*)_(Males|Females|Persons)$',
        '^(Total)_(Males|Females|Persons)$'
    ),
    stats = c('stat1', 'gender')
)

b05config <- list(
    table    = 'B05',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_(Total|Widowed|Divorced|Never_Married|Married|Separated)$',
        '^(Males|Females|Persons)_(.*)_(Never_Married)$'
    ),
    stats = c('stat1', 'stat2', 'gender')
)

b06config <- list(
    table    = 'B06',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_(Total|Not_married|Married_in_a_registered_marriage|Married_in_a_de_facto_marriage)$'
    ),
    stats = c('gender', 'stat1', 'stat2')
)

b07config <- list(
    table    = 'B07',
    patterns = c(
        '^(.*)_years_(.*)_(Males|Females|Persons)$',
        '^(.*)_years_and_over_(.*)_(Males|Females|Persons)$',
        '^(Total)_(.*)_(Males|Females|Persons)$'
    ),
    stats = c('stat1', 'stat2', 'gender')
)

b08config <- list(
    table    = 'B08',
    patterns = c(
        '^(.*)_(Mother_only|Father_only|Both_parents)_(born_in_Australia|born_overseas)$',
        '^(.*)()_(Birthplace_not_stated|Total_Responses)$'
    ),
    stats = c('stat1', 'stat2', 'stat3')
)

b09config <- list(
    table    = 'B09',
    patterns = c(
        '^(.*)_(Males|Females|Persons|Person|Total)$'
    ),
    stats = c('stat1', 'gender'),
    extra_manipulation = . %>% mutate(gender = ifelse(gender %in% c('Total', 'Person'), 'Persons', gender))
)

b10config <- list(
    table    = 'B10',
    patterns = c(
        '^(.*)_Year_of_arrival_(.*)$',
        '^(.*)_(Total)$'
    ),
    stats = c('stat1', 'stat2')
)

b46config <- list(
    table    = 'B46',
    patterns = c(
        '^(.*)()_(Males|Females|Persons)$',
        '^(One_method|Two_methods|Three_methods)_(.*)_(Males|Females|Persons)$'
    ),
    stats = c('stat1', 'stat2', 'gender')
)

if (FALSE) {
    df <- read_abs('BCP', 'B10', 'AUS', long=TRUE); column_names <- df$colname
    df <- split_column_names(df$colname, b10config)
    df
}
