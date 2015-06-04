
source("pattern-helper.R")  # helper code for building regex patterns

#=============================================================================
# Table configuration information
#=============================================================================

make_tableconfig <- function() {
    tableconfig <- list()

    #-----------------------------------------------------------------------------
    table <- 'B01'
    desc  <- 'Random stuff'
    #-----------------------------------------------------------------------------
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
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
        stats    = c('stat1', 'stat2', 'gender')
    )

    #-----------------------------------------------------------------------------
    table <- "B03"
    desc  <- "Visitor Origin"
    #-----------------------------------------------------------------------------
    place    <- c('(Counted_at_home_on_Census_Night)',
                  'Visitor_from_(Same_Statistical_Area_Level_2_SA2)',
                  'Visitor_from_(.*?)',
                  'Visitor_from_Different_SA2_in_(.*?)',
                  'Visitor_from_(Different_SA2_in_Total)',
                  '(Total)')
    age      <- make_age_pattern('Age')
    patterns <- make_patterns(place, age)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('stat1', 'stat2')
    )


    #-----------------------------------------------------------------------------
    table <- "B04"
    desc  <- ""
    #-----------------------------------------------------------------------------
    age <- c('Age_years_(.*)',
             '(Total)')
    patterns <- make_patterns(age, gender_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('stat1', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B05"
    desc  <- "Marriage status"
    #-----------------------------------------------------------------------------
    marriage <- c('(Total|Widowed|Divorced|Married|Separated)',
                  '(Never_Married)')
    patterns <- make_patterns(gender_pattern, make_age_pattern(), marriage)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'marriage_status')
    )


    #-----------------------------------------------------------------------------
    table <- "B06"
    desc  <- "Marriage type"
    #-----------------------------------------------------------------------------
    marriage <- '(Total|Not_married|Married_in_a_registered_marriage|Married_in_a_de_facto_marriage)'
    patterns <- make_patterns(gender_pattern, make_age_pattern(), marriage)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'marriage_type')
    )


    #-----------------------------------------------------------------------------
    table <- "B07"
    desc  <- "Indigenous Status"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(make_age_pattern(), wild_pattern, gender_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('age', 'indigenous_status', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B08"
    desc  <- "Nationality & Nationality of Parents"
    #-----------------------------------------------------------------------------
    parents  <- c('(Mother_only|Father_only|Both_parents)_(born_in_Australia|born_overseas)',
                  '()(Birthplace_not_stated|Total_Responses)')
    patterns <- make_patterns(wild_pattern, parents)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('nationality', 'parents', 'parents_origin')
    )


    #-----------------------------------------------------------------------------
    table <- "B09"
    desc  <- "Place of birth"
    # Table has some typos need 'extra_manipulation' to fix
    #-----------------------------------------------------------------------------
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = c('^(.*)_(Males|Females|Persons|Person|Total)$'),
        stats    = c('birthplace', 'gender'),
        extra_manipulation = . %>% mutate(gender = ifelse(gender %in% c('Total', 'Person'), 'Persons', gender))
    )


    #-----------------------------------------------------------------------------
    table <- "B10"
    desc  <- "Immigration"
    #-----------------------------------------------------------------------------
    origin          <- wild_pattern
    year_of_arrival <- c('Year_of_arrival_(.*)',
                         '(Total)')
    patterns <- make_patterns(origin, year_of_arrival)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('origin', 'arrival_year')
    )


    #-----------------------------------------------------------------------------
    table <- "B11"
    desc  <- "English proficiency"
    #-----------------------------------------------------------------------------
    year_of_arrival <- c('Year_of_arrival_(.*)',
                         '(Total)')
    patterns        <- make_patterns(gender_pattern, wild_pattern, year_of_arrival)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'language', 'arrival_year')
    )


    #-----------------------------------------------------------------------------
    table <- "B12"
    desc  <- "Language with dependents"
    #-----------------------------------------------------------------------------
    age      <- c('Dependent_children_aged_(.*)_years',
                  '(Total)_dependent_children')
    female   <- 'female_parent_(.*)'
    male     <- 'male_parent_(.*)'
    patterns <- make_patterns(age, female, male)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('children_age', 'female_parent', 'male_parent')
    )


    #-----------------------------------------------------------------------------
    table <- "B13"
    desc  <- "Language spoken at home"
    #-----------------------------------------------------------------------------
    language <- c('Speaks_(English_only)',
                  'Speaks_other_language_(.*)',
                  'Language_spoken_at_home_(not_stated)',
                  '(Total)')
    patterns <- make_patterns(language, gender_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('language', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B14"
    desc  <- "Religion"
    #-----------------------------------------------------------------------------
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = c('^(.*)_(Males|Females|Persons)$'),
        stats    = c('religion', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B15"
    desc  <- "Educational Institution"
    #-----------------------------------------------------------------------------
    pre_school <- '(.*)()()()'
    infants    <- 'Infants_(Primary)_(.*)()()'
    secondary  <- '(Secondary)_(.*)()()'
    tertiary   <- '(.*)()_(Part_time|Full_Part_time|Full_time)_student'
    age        <- c('Aged_(15_24)_years',
                    'Aged_(25_years_and_over)')
    tertiary2  <- '(.*)()_Full_Part_time_(student_status_not_stated)()'
    tertiary3  <- '(.*)()_(Total)()'
    tertiary4  <- '(.*)()_(Part_time|Full_Part_time|Full_time)()_student'

    patterns <- c(make_patterns(pre_school     , gender_pattern),
                  make_patterns(tertiary3      , gender_pattern),
                  make_patterns(infants        , gender_pattern),
                  make_patterns(secondary      , gender_pattern),
                  make_patterns(tertiary  , age, gender_pattern),
                  make_patterns(tertiary2      , gender_pattern),
                  make_patterns(tertiary4      , gender_pattern))

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('inst1', 'inst2', 'student_type', 'age', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B16"
    desc  <- "Highest school level"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, wild_pattern, make_age_pattern('Age'))

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'school_level', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- "B17"
    desc  <- "Personal income"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, wild_pattern, make_age_pattern('Age'))

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'income', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- "B18"
    desc  <- "Persons needing assistance"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'need_assistance_status')
    )


    #-----------------------------------------------------------------------------
    table <- "B19"
    desc  <- "Volunteering"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'volunteer_status')
    )


    #-----------------------------------------------------------------------------
    table <- "B20"
    desc  <- "Housework"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'housework')
    )


    #-----------------------------------------------------------------------------
    table <- "B21"
    desc  <- "Providing unpaid assistance"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'unpaid_assistance')
    )


    #-----------------------------------------------------------------------------
    table <- "B22"
    desc  <- "Childcare"
    #-----------------------------------------------------------------------------
    patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'childcare')
    )


    #-----------------------------------------------------------------------------
    table <- "B23"
    desc  <- "Household makeup"
    #-----------------------------------------------------------------------------
    age_range <- make_age_pattern('Age')
    patterns  <- make_patterns(gender_pattern, wild_pattern, age_range)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'status', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- "B24"
    desc  <- "Children"
    #-----------------------------------------------------------------------------
    age_range <- make_age_pattern('Age_group_of_parent')
    Nchildren <- c('Number_of_children_ever_born_(.*)',
                   '(Total)')
    patterns  <- make_patterns(age_range, Nchildren)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('parent_age', 'children')
    )


    #-----------------------------------------------------------------------------
    table <- "B25"
    desc  <- "Family type"
    #-----------------------------------------------------------------------------
    famtype  <- '(Couple_family|One_parent_family|Total)'
    child1   <- c('(with_no_children)()()')
    child2   <- '(with_children_under_15|with_no_children_under_15)_and_(dependent_students|no_dependent_students)_and_(non_dependent_children|no_non_dependent_children)'
    child3   <- '(with_children_under_15|with_no_children_under_15)_and_(Total)()'
    child4   <- 'with_(Total)()()'
    group    <- '(Families|Persons)'
    patterns <- c(make_patterns('(.*)()()()', group),
                  make_patterns(famtype, child1, group),
                  make_patterns(famtype, child2, group),
                  make_patterns(famtype, child3, group),
                  make_patterns(famtype, child4, group))

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('stat1', 'stat2', 'stat3', 'stat4', 'group')
    )


    #-----------------------------------------------------------------------------
    table <- "B26"
    desc  <- "Household income by family type"
    #-----------------------------------------------------------------------------
    famtype  <- '(Couple_family_with_children|Other_family|One_parent_family|Total|Couple_family_with_no_children)'
    patterns <- make_patterns(wild_pattern, famtype)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('income', 'family_type')
    )



    #-----------------------------------------------------------------------------
    table <- "B27"
    desc  <- "Family type"
    #-----------------------------------------------------------------------------
    p1       <- c('(.*)_with_(.*)', '(Total)()')
    patterns <- make_patterns(p1, 'Families')

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('family', 'children')
    )


    #-----------------------------------------------------------------------------
    table <- "B28"
    desc  <- "Household income"
    #-----------------------------------------------------------------------------
    households <- '(Family_households|Total|Non_family_households)'
    patterns   <- make_patterns(wild_pattern, households)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('income', 'household')
    )


    #-----------------------------------------------------------------------------
    table <- "B29"
    desc  <- "Motor vehicles per dwelling"
    #-----------------------------------------------------------------------------
    vehicles <- c('(Total)',
                  'Number_of_motor_vehicles_per_dwelling_(.*)', 'Number_of_motor_vehicles_(not_stated)')
    patterns <- make_patterns(vehicles, 'Dwellings')

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('cars_per_dwelling')
    )


    #-----------------------------------------------------------------------------
    table <- "B30"
    desc  <- "usual residency"
    #-----------------------------------------------------------------------------
    households <- c('(Family|Non_family)_households',
                    '(Total)')
    number     <- c('Number_of_Persons_usually_resident_(.*)',
                    '(Total)')
    patterns   <- make_patterns(number, households)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('usual_residents', 'household')
    )


    #-----------------------------------------------------------------------------
    table <- "B31"
    desc  <- "house type"
    #-----------------------------------------------------------------------------
    dwellperson <- '(Dwellings|Persons)'
    occupied    <- '(Total|Occupied|Unoccupied)_private_dwellings'
    patterns    <- c(make_patterns(occupied, wild_pattern, dwellperson),
                     make_patterns(occupied, paste0('()', dwellperson)))

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('occupied', 'stat1', 'dp')
    )


    #-----------------------------------------------------------------------------
    table <- "B32"
    desc  <- "housing tenure"
    #-----------------------------------------------------------------------------
    tenure    <- c('(.*)',
                   '(.*)_Dwelling_structure')
    structure <- '(Total|Separate_house|Semi_detached_row_or_terrace_house_townhouse_etc|Flat_unit_or_apartment|Other_dwelling|not_stated)'
    patterns  <- make_patterns(tenure, structure)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('tenure', 'structure')
    )


    #-----------------------------------------------------------------------------
    table <- "B33"
    desc  <- "Mortgage"
    #-----------------------------------------------------------------------------
    mortgage  <- wild_pattern
    structure <- c('Dwelling_structure_(.*?)',
                   '(Total)')
    patterns  <- make_patterns(mortgage, structure)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('mortgage', 'structure')
    )


    #-----------------------------------------------------------------------------
    table <- "B34"
    desc  <- "Rent payments"
    #-----------------------------------------------------------------------------
    agent_pattern <- c('Landlord_type_(.*?)',
                       '(Total)')
    patterns      <- make_patterns(wild_pattern, agent_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('rent', 'landlord')
    )


    #-----------------------------------------------------------------------------
    table <- "B35"
    desc  <- "Internet"
    #-----------------------------------------------------------------------------
    structure  <- c('Dwelling_structure_(.*?)',
                    '(Total)')
    connection <- c('(No_Internet_connection|Internet_connection_not_stated|Total)',
                    'Type_of_Internet_connection_(.*)')
    patterns   <- make_patterns(connection, structure)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('rent', 'landlord')
    )


    #-----------------------------------------------------------------------------
    table <- "B36"
    desc  <- "Bedrooms"
    #-----------------------------------------------------------------------------
    structure <- wild_pattern
    bedrooms  <- c('Number_of_bedrooms_(.*)',
                   '(Total)')
    patterns  <- make_patterns(structure, bedrooms)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('dwelling', 'bedrooms')
    )


    #-----------------------------------------------------------------------------
    table <- "B37"
    desc  <- "Random stuff"
    #-----------------------------------------------------------------------------
    age          <- make_patterns('Persons_(aged_15_years_and_over)', gender_pattern)
    labour_force <- make_patterns('Labour_force_status_(.*)'        , gender_pattern)
    pc_employ    <- make_patterns('Percent_(.*)'                    , gender_pattern)
    nsq          <- make_patterns('Non_school_qualifications_(.*)'  , gender_pattern)
    migration    <- make_patterns('Migration_Lived_at_(.*)'         , gender_pattern)
    patterns     <- c(age, labour_force, pc_employ, nsq, migration)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('stat1', 'stat2')
    )


    #-----------------------------------------------------------------------------
    table <- "B38"
    desc  <- "Location 1 year ago"
    #-----------------------------------------------------------------------------
    address  <- c('(.*?)()',
                  '(Same_usual_address_1_year_ago)_(as_in_2011)',
                  '(Different_usual_address_1_year_ago)_(.*)',
                  '(Different_usual_address_1_year_ago)_Different_SA2_in_(.*)',
                  '(Different_usual_address_1_year_ago)_(Different_SA2_in_Total)')
    patterns <- make_patterns(address, gender_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('stat1', 'stat2', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B39"
    desc  <- "Address 5 years ago"
    #-----------------------------------------------------------------------------
    address <- c('(.*?)()',
                 '(Same_usual_address_5_years_ago)_(as_in_2011)',
                 '(Different_usual_address_5_years_ago)_(.*)',
                 '(Different_usual_address_5_years_ago)_Different_SA2_in_(.*)',
                 '(Different_usual_address_5_years_ago)_(Different_SA2_in_Total)')
    patterns <- make_patterns(address, gender_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('stat1', 'stat2', 'gender')
    )


    #-----------------------------------------------------------------------------
    table <- "B40"
    desc  <- "Post-school education"
    #-----------------------------------------------------------------------------
    age_pattern <- make_age_pattern('Age')
    patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'education', 'age')
    )

    #-----------------------------------------------------------------------------
    table <- "B41"
    desc  <- "Field of study"
    #-----------------------------------------------------------------------------
    age_pattern <- make_age_pattern('Age')
    patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'field', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- "B42"
    desc  <- "Employment status"
    #-----------------------------------------------------------------------------
    age_pattern <- make_age_pattern('Age')
    patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'employment', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- "B43"
    desc  <- "Employment area"
    #-----------------------------------------------------------------------------
    age_pattern <- make_age_pattern('Age')
    patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'employment_area', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- "B44"
    desc  <- ""
    #-----------------------------------------------------------------------------
    occupation <- c('Occupation_(.*?)', '(Total)')
    patterns    <- make_patterns(wild_pattern, occupation)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('employment_area', 'job_description')
    )


    #-----------------------------------------------------------------------------
    table <- "B45"
    desc  <- ""
    #-----------------------------------------------------------------------------
    age_pattern <- make_age_pattern()
    occupation  <- c('Occupation_(.*?)', '(Total)')
    patterns    <- make_patterns(gender_pattern, make_age_pattern(), occupation)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('gender', 'age', 'job_description')
    )


    #-----------------------------------------------------------------------------
    table <- "B46"
    desc  <- "Method of Travel to work"
    #-----------------------------------------------------------------------------
    method   <- c('(.*)()',
                  '(One_method|Two_methods|Three_methods)_(.*)')
    patterns <- make_patterns(method, gender_pattern)

    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = patterns,
        stats    = c('methodcount', 'method', 'gender')
    )

    tableconfig
}


