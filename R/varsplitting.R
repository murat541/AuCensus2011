
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
        if (nrow(match)==0) {
            warning("Failed")
            print(pattern)
            stop("Failed")
        }
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

# English proficiency
b11config <- list(
    table    = 'B11',
    patterns = c(
        '^(Males|Females|Persons)_(.*?)_Year_of_arrival_(.*)$'
    ),
    stats = c('gender', 'stat1', 'stat2')
)

# language with dependents
b12config <- list(
    table    = 'B12',
    patterns = c(
        '^Dependent_children_aged_(.*)_years_female_parent_(.*)_male_parent_(.*)$',
        '^(Total)_dependent_children_female_parent_(.*)_male_parent_(.*)$'
    ),
    stats = c('children_age', 'female_parent', 'male_parent')
)

# language spoken at home
b13config <- list(
    table    = 'B13',
    patterns = c(
        '^Speaks_(English_only)_(Males|Females|Persons)$',
        '^Speaks_other_language_(.*)_(Males|Females|Persons)$',
        '^Language_spoken_at_home_(not_stated)_(Males|Females|Persons)$',
        '^(Total)_(Males|Females|Persons)$'
    ),
    stats = c('language', 'gender')
)

# religion
b14config <- list(
    table    = 'B14',
    patterns = c(
        '^(.*)_(Males|Females|Persons)$'
    ),
    stats = c('religion', 'gender')
)

# Educational Institution. MFC TODO not finished.
b15config <- list(
    table    = 'B15',
    patterns = c(
        '^(.*)()()()_(Males|Females|Persons)$',
        # '^(.*)_(Government|Catholic|Non_Government|Total)()_(Total)_(Males|Females|Persons)$'
        '^(.*)()_(Full_Part_time|Full_time)_student_Aged_(.*)_(Males|Females|Persons)$'
    ),
    stats = c('inst1', 'inst2', 'student_type', 'age', 'gender')
)

# highest school level
b16config <- list(
    table    = 'B16',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_Age_(.*)_years',
        '^(Males|Females|Persons)_(.*)_(Total)$'
    ),
    stats = c('gender', 'school', 'age')
)

# Personal income
b17config <- list(
    table    = 'B17',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_Age_(.*)_years',
        '^(Males|Females|Persons)_(.*)_(Total)'
    ),
    stats = c('gender', 'income', 'age')
)

# persons needing assistance
b18config <- list(
    table    = 'B18',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_years_(.*)',
        '^(Males|Females|Persons)_(.*_years_and_over)_(.*)',
        '^(Males|Females|Persons)_(Total)_(.*)'
    ),
    stats = c('gender', 'age', 'assistance')
)

# volunteering
b19config <- list(
    table    = 'B19',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_years_(.*)',
        '^(Males|Females|Persons)_(.*_years_and_over)_(.*)',
        '^(Males|Females|Persons)_(Total)_(.*)'
    ),
    stats = c('gender', 'age', 'volunteer')
)

# housework
b20config <- list(
    table    = 'B20',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_years_(.*)',
        '^(Males|Females|Persons)_(.*_years_and_over)_(.*)',
        '^(Males|Females|Persons)_(Total)_(.*)'
    ),
    stats = c('gender', 'age', 'housework')
)

# providing assistance
b21config <- list(
    table    = 'B21',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_years_(.*)',
        '^(Males|Females|Persons)_(.*_years_and_over)_(.*)',
        '^(Males|Females|Persons)_(Total)_(.*)'
    ),
    stats = c('gender', 'age', 'assistance')
)

# childcare
b22config <- list(
    table    = 'B22',
    patterns = c(
        '^(Males|Females|Persons)_(.*)_years_(.*)',
        '^(Males|Females|Persons)_(.*_years_and_over)_(.*)',
        '^(Males|Females|Persons)_(Total)_(.*)'
    ),
    stats = c('gender', 'age', 'childcare')
)

# household makeup
age_range <- make_age_pattern('Age')
patterns <- make_patterns(gender_pattern, wild_pattern, age_range)

b23config <- list(
    table    = 'B23',
    patterns = patterns,
    stats = c('gender', 'status', 'age')
)

# children
age_range <- make_age_pattern('Age_group_of_parent')
Nchildren <- c('Number_of_children_ever_born_(.*)', '(Total)')
patterns <- make_patterns(age_range, Nchildren)
b24config <- list(
    table    = 'B24',
    patterns = patterns,
    stats = c('parent_age', 'children')
)


# Family type
famtype <- '(Couple_family|One_parent_family|Total)'
group <- '(Families|Persons)'
patterns <- make_patterns(famtype, wild_pattern, group)
patterns

b25config <- list(
    table    = 'B25',
    patterns = patterns,
    stats = c('stat1', 'stat2', 'group')
)


# B26 Household incomine by family type
famtype <- c('(Couple_family_with_children|Other_family|One_parent_family|Total|Couple_family_with_no_children)')
patterns <- make_patterns(wild_pattern, famtype)
patterns

b26config <- list(
    table    = 'B26',
    patterns = patterns,
    stats = c('income', 'family')
)

# B27 Family type
p1 <- c('(.*)_with_(.*)', '(Total)()')
patterns <- make_patterns(p1, 'Families')
patterns

b27config <- list(
    table    = 'B27',
    patterns = patterns,
    stats = c('family', 'children')
)

# family incomes age
households <- '(Family_households|Total|Non_family_households)'
patterns <- make_patterns(wild_pattern, households)
patterns

b28config <- list(
    table    = 'B28',
    patterns = patterns,
    stats = c('income', 'household')
)

# motor vehicles per dwelling
vehicles <- c('(Total)', 'Number_of_motor_vehicles_per_dwelling_(.*)', 'Number_of_motor_vehicles_(not_stated)')
patterns <- make_patterns(vehicles, 'Dwellings')
patterns

b29config <- list(
    table    = 'B29',
    patterns = patterns,
    stats = c('cars')
)

# usual residency
households <- c('(Family|Non_family)_households', '(Total)')
number <- c('Number_of_Persons_usually_resident_(.*)', '(Total)')
patterns <- make_patterns(number, households)
patterns

b30config <- list(
    table    = 'B30',
    patterns = patterns,
    stats = c('number', 'household')
)

# b31 house type
dwellperson <- '(Dwellings|Persons)'
occupied <- '(Total|Occupied|Unoccupied)_private_dwellings'
patterns <- c(make_patterns(occupied, wild, dwellperson),
              make_patterns(occupied,       paste0('()', dwellperson))) # MFC TODO need better skipped pattern
patterns

b31config <- list(
    table    = 'B31',
    patterns = patterns,
    stats = c('occupied', 'stat1', 'dp')
)

# housing tenure
tenure <- c('(.*)', '(.*)_Dwelling_structure')
structure <- '(Total|Separate_house|Semi_detached_row_or_terrace_house_townhouse_etc|Flat_unit_or_apartment|Other_dwelling|not_stated)'
patterns <- make_patterns(tenure, structure)
patterns

b32config <- list(
    table    = 'B32',
    patterns = patterns,
    stats = c('tenure', 'structure')
)


# B33 mortgage
mortgage <- c('(.*)', '(.*)_Dwelling_structure')
structure <- '(Total|Separate_house|Semi_detached_row_or_terrace_house_townhouse_etc|Flat_unit_or_apartment|Other_dwelling|Not_stated)'
patterns <- make_patterns(mortgage, structure)
patterns
b33config <- list(
    table    = 'B33',
    patterns = patterns,
    stats = c('mortgage', 'structure')
)

# B34
b34config <- list(
    table    = 'B34',
    patterns = patterns,
    stats = c('mortgage', 'structure')
)


config <- b34config
df <- read_abs('BCP', config$table, 'AUS', long=TRUE);
column_names <- df$colname
df <- split_column_names(df$colname, config)
df
df %>% as.data.frame
