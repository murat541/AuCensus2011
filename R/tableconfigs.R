
source("patterns.R")

#=============================================================================
# Table configuration information
#=============================================================================

#-----------------------------------------------------------------------------
# B01
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
    stats    = c('stat1', 'stat2', 'gender')
)

#-----------------------------------------------------------------------------
# B03 Visitor Origin
#-----------------------------------------------------------------------------
place    <- c('(Counted_at_home_on_Census_Night)',
              'Visitor_from_(Same_Statistical_Area_Level_2_SA2)',
              'Visitor_from_(.*?)',
              'Visitor_from_Different_SA2_in_(.*?)',
              'Visitor_from_(Different_SA2_in_Total)',
              '(Total)')
age      <- make_age_pattern('Age')
patterns <- make_patterns(place, age)

b03config <- list(
    table    = 'B03',
    patterns = patterns,
    stats    = c('stat1', 'stat2')
)


#-----------------------------------------------------------------------------
# B04
#-----------------------------------------------------------------------------
age <- c('Age_years_(.*)',
         '(Total)')
patterns <- make_patterns(age, gender_pattern)

b04config <- list(
    table    = 'B04',
    patterns = patterns,
    stats    = c('stat1', 'gender')
)


#-----------------------------------------------------------------------------
# B05 Marriage status
#-----------------------------------------------------------------------------
marriage <- c('(Total|Widowed|Divorced|Married|Separated)',
              '(Never_Married)')
patterns <- make_patterns(gender_pattern, make_age_pattern(), marriage)

b05config <- list(
    table    = 'B05',
    patterns = patterns,
    stats    = c('gender', 'age', 'marriage_status')
)


#-----------------------------------------------------------------------------
# B06 Marriage type
#-----------------------------------------------------------------------------
marriage <- '(Total|Not_married|Married_in_a_registered_marriage|Married_in_a_de_facto_marriage)'
patterns <- make_patterns(gender_pattern, make_age_pattern(), marriage)

b06config <- list(
    table    = 'B06',
    patterns = patterns,
    stats    = c('gender', 'age', 'marriage_type')
)


#-----------------------------------------------------------------------------
# B07 Indigenous Status
#-----------------------------------------------------------------------------
patterns <- make_patterns(make_age_pattern(), wild_pattern, gender_pattern)

b07config <- list(
    table    = 'B07',
    patterns = patterns,
    stats    = c('age', 'indigenous_status', 'gender')
)


#-----------------------------------------------------------------------------
# B08 Nationality & Nationality of Parents
#-----------------------------------------------------------------------------
parents  <- c('(Mother_only|Father_only|Both_parents)_(born_in_Australia|born_overseas)',
              '()(Birthplace_not_stated|Total_Responses)')
patterns <- make_patterns(wild_pattern, parents)

b08config <- list(
    table    = 'B08',
    patterns = patterns,
    stats    = c('nationality', 'parents', 'parents_origin')
)


#-----------------------------------------------------------------------------
# B09 Place of birth.  Table has some typos need 'extra_manipulation' to fix
#-----------------------------------------------------------------------------
b09config <- list(
    table    = 'B09',
    patterns = c('^(.*)_(Males|Females|Persons|Person|Total)$'),
    stats    = c('birthplace', 'gender'),
    extra_manipulation = . %>% mutate(gender = ifelse(gender %in% c('Total', 'Person'), 'Persons', gender))
)


#-----------------------------------------------------------------------------
# B10 Immigration
#-----------------------------------------------------------------------------
origin          <- wild_pattern
year_of_arrival <- c('Year_of_arrival_(.*)',
                     '(Total)')
patterns <- make_patterns(origin, year_of_arrival)

b10config <- list(
    table    = 'B10',
    patterns = patterns,
    stats    = c('origin', 'arrival_year')
)


#-----------------------------------------------------------------------------
# B11 English proficiency
#-----------------------------------------------------------------------------
year_of_arrival <- c('Year_of_arrival_(.*)',
                     '(Total)')
patterns        <- make_patterns(gender_pattern, wild_pattern, year_of_arrival)

b11config <- list(
    table    = 'B11',
    patterns = patterns,
    stats    = c('gender', 'language', 'arrival_year')
)


#-----------------------------------------------------------------------------
# B12 language with dependents
#-----------------------------------------------------------------------------
age      <- c('Dependent_children_aged_(.*)_years',
              '(Total)_dependent_children')
female   <- 'female_parent_(.*)'
male     <- 'male_parent_(.*)'
patterns <- make_patterns(age, female, male)

b12config <- list(
    table    = 'B12',
    patterns = patterns,
    stats    = c('children_age', 'female_parent', 'male_parent')
)


#-----------------------------------------------------------------------------
# B13 language spoken at home
#-----------------------------------------------------------------------------
language <- c('Speaks_(English_only)',
              'Speaks_other_language_(.*)',
              'Language_spoken_at_home_(not_stated)',
              '(Total)')
patterns <- make_patterns(language, gender_pattern)

b13config <- list(
    table    = 'B13',
    patterns = patterns,
    stats    = c('language', 'gender')
)


#-----------------------------------------------------------------------------
# B14 Religion
#-----------------------------------------------------------------------------
b14config <- list(
    table    = 'B14',
    patterns = c('^(.*)_(Males|Females|Persons)$'),
    stats    = c('religion', 'gender')
)


#-----------------------------------------------------------------------------
# B15 Educational Institution.
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

b15config <- list(
    table    = 'B15',
    patterns = patterns,
    stats    = c('inst1', 'inst2', 'student_type', 'age', 'gender')
)


#-----------------------------------------------------------------------------
# B16 highest school level
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, wild_pattern, make_age_pattern('Age'))

b16config <- list(
    table    = 'B16',
    patterns = patterns,
    stats    = c('gender', 'school_level', 'age')
)


#-----------------------------------------------------------------------------
# B17 Personal income
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, wild_pattern, make_age_pattern('Age'))

b17config <- list(
    table    = 'B17',
    patterns = patterns,
    stats    = c('gender', 'income', 'age')
)


#-----------------------------------------------------------------------------
# B18 persons needing assistance
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

b18config <- list(
    table    = 'B18',
    patterns = patterns,
    stats    = c('gender', 'age', 'need_assistance_status')
)


#-----------------------------------------------------------------------------
# B19 volunteering
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

b19config <- list(
    table    = 'B19',
    patterns = patterns,
    stats    = c('gender', 'age', 'volunteer_status')
)


#-----------------------------------------------------------------------------
# B20 housework
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

b20config <- list(
    table    = 'B20',
    patterns = patterns,
    stats    = c('gender', 'age', 'housework')
)


#-----------------------------------------------------------------------------
# B21 providing assistance
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

b21config <- list(
    table    = 'B21',
    patterns = patterns,
    stats    = c('gender', 'age', 'unpaid_assistance')
)


#-----------------------------------------------------------------------------
# B22 childcare
#-----------------------------------------------------------------------------
patterns <- make_patterns(gender_pattern, make_age_pattern(), wild_pattern)

b22config <- list(
    table    = 'B22',
    patterns = patterns,
    stats    = c('gender', 'age', 'childcare')
)


#-----------------------------------------------------------------------------
# B23 household makeup
#-----------------------------------------------------------------------------
age_range <- make_age_pattern('Age')
patterns  <- make_patterns(gender_pattern, wild_pattern, age_range)

b23config <- list(
    table    = 'B23',
    patterns = patterns,
    stats    = c('gender', 'status', 'age')
)


#-----------------------------------------------------------------------------
# B24 children
#-----------------------------------------------------------------------------
age_range <- make_age_pattern('Age_group_of_parent')
Nchildren <- c('Number_of_children_ever_born_(.*)',
               '(Total)')
patterns  <- make_patterns(age_range, Nchildren)

b24config <- list(
    table    = 'B24',
    patterns = patterns,
    stats    = c('parent_age', 'children')
)


#-----------------------------------------------------------------------------
# B25 Family type
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

b25config <- list(
    table    = 'B25',
    patterns = patterns,
    stats    = c('stat1', 'stat2', 'stat3', 'stat4', 'group')
)


#-----------------------------------------------------------------------------
# B26 Household incomine by family type
#-----------------------------------------------------------------------------
famtype  <- '(Couple_family_with_children|Other_family|One_parent_family|Total|Couple_family_with_no_children)'
patterns <- make_patterns(wild_pattern, famtype)

b26config <- list(
    table    = 'B26',
    patterns = patterns,
    stats    = c('income', 'family_type')
)



#-----------------------------------------------------------------------------
# B27 Family type
#-----------------------------------------------------------------------------
p1       <- c('(.*)_with_(.*)', '(Total)()')
patterns <- make_patterns(p1, 'Families')

b27config <- list(
    table    = 'B27',
    patterns = patterns,
    stats    = c('family', 'children')
)


#-----------------------------------------------------------------------------
# B28 family incomes age
#-----------------------------------------------------------------------------
households <- '(Family_households|Total|Non_family_households)'
patterns   <- make_patterns(wild_pattern, households)

b28config <- list(
    table    = 'B28',
    patterns = patterns,
    stats    = c('income', 'household')
)


#-----------------------------------------------------------------------------
# B29 motor vehicles per dwelling
#-----------------------------------------------------------------------------
vehicles <- c('(Total)',
              'Number_of_motor_vehicles_per_dwelling_(.*)', 'Number_of_motor_vehicles_(not_stated)')
patterns <- make_patterns(vehicles, 'Dwellings')

b29config <- list(
    table    = 'B29',
    patterns = patterns,
    stats    = c('cars_per_dwelling')
)


#-----------------------------------------------------------------------------
# B30 usual residency
#-----------------------------------------------------------------------------
households <- c('(Family|Non_family)_households',
                '(Total)')
number     <- c('Number_of_Persons_usually_resident_(.*)',
                '(Total)')
patterns   <- make_patterns(number, households)

b30config <- list(
    table    = 'B30',
    patterns = patterns,
    stats    = c('usual_residents', 'household')
)


#-----------------------------------------------------------------------------
# B31 house type
#-----------------------------------------------------------------------------
dwellperson <- '(Dwellings|Persons)'
occupied    <- '(Total|Occupied|Unoccupied)_private_dwellings'
patterns    <- c(make_patterns(occupied, wild, dwellperson),
                 make_patterns(occupied, paste0('()', dwellperson)))

b31config <- list(
    table    = 'B31',
    patterns = patterns,
    stats    = c('occupied', 'stat1', 'dp')
)


#-----------------------------------------------------------------------------
# B32 housing tenure
#-----------------------------------------------------------------------------
tenure    <- c('(.*)',
               '(.*)_Dwelling_structure')
structure <- '(Total|Separate_house|Semi_detached_row_or_terrace_house_townhouse_etc|Flat_unit_or_apartment|Other_dwelling|not_stated)'
patterns  <- make_patterns(tenure, structure)

b32config <- list(
    table    = 'B32',
    patterns = patterns,
    stats    = c('tenure', 'structure')
)


#-----------------------------------------------------------------------------
# B33 Mortgage
#-----------------------------------------------------------------------------
mortgage  <- wild_pattern
structure <- c('Dwelling_structure_(.*?)',
               '(Total)')
patterns  <- make_patterns(mortgage, structure)

b33config <- list(
    table    = 'B33',
    patterns = patterns,
    stats    = c('mortgage', 'structure')
)


#-----------------------------------------------------------------------------
# B34 Rent payments
#-----------------------------------------------------------------------------
agent_pattern <- c('Landlord_type_(.*?)',
                   '(Total)')
patterns      <- make_patterns(wild_pattern, agent_pattern)

b34config <- list(
    table    = 'B34',
    patterns = patterns,
    stats    = c('rent', 'landlord')
)


#-----------------------------------------------------------------------------
# B35 Internet
#-----------------------------------------------------------------------------
structure  <- c('Dwelling_structure_(.*?)',
                '(Total)')
connection <- c('(No_Internet_connection|Internet_connection_not_stated|Total)',
                'Type_of_Internet_connection_(.*)')
patterns   <- make_patterns(connection, structure)

b35config <- list(
    table    = 'B35',
    patterns = patterns,
    stats    = c('rent', 'landlord')
)


#-----------------------------------------------------------------------------
# B36 Bedrooms
#-----------------------------------------------------------------------------
structure <- wild_pattern
bedrooms  <- c('Number_of_bedrooms_(.*)',
               '(Total)')
patterns  <- make_patterns(structure, bedrooms)

b36config <- list(
    table    = 'B36',
    patterns = patterns,
    stats    = c('dwelling', 'bedrooms')
)


#-----------------------------------------------------------------------------
# B37 random
#-----------------------------------------------------------------------------
age          <- make_patterns('Persons_(aged_15_years_and_over)', gender_pattern)
labour_force <- make_patterns('Labour_force_status_(.*)'        , gender_pattern)
pc_employ    <- make_patterns('Percent_(.*)'                    , gender_pattern)
nsq          <- make_patterns('Non_school_qualifications_(.*)'  , gender_pattern)
migration    <- make_patterns('Migration_Lived_at_(.*)'         , gender_pattern)
patterns     <- c(age, labour_force, pc_employ, nsq, migration)

b37config <- list(
    table    = 'B37',
    patterns = patterns,
    stats    = c('stat1', 'stat2')
)


#-----------------------------------------------------------------------------
# B38 Location 1 year ago
#-----------------------------------------------------------------------------
address  <- c('(.*?)()',
              '(Same_usual_address_1_year_ago)_(as_in_2011)',
              '(Different_usual_address_1_year_ago)_(.*)',
              '(Different_usual_address_1_year_ago)_Different_SA2_in_(.*)',
              '(Different_usual_address_1_year_ago)_(Different_SA2_in_Total)')
patterns <- make_patterns(address, gender_pattern)

b38config <- list(
    table    = 'B38',
    patterns = patterns,
    stats    = c('stat1', 'stat2', 'gender')
)


#-----------------------------------------------------------------------------
# B39 Address 5 years ago
#-----------------------------------------------------------------------------
address <- c('(.*?)()',
             '(Same_usual_address_5_years_ago)_(as_in_2011)',
             '(Different_usual_address_5_years_ago)_(.*)',
             '(Different_usual_address_5_years_ago)_Different_SA2_in_(.*)',
             '(Different_usual_address_5_years_ago)_(Different_SA2_in_Total)')
patterns <- make_patterns(address, gender_pattern)

b39config <- list(
    table    = 'B39',
    patterns = patterns,
    stats    = c('stat1', 'stat2', 'gender')
)


#-----------------------------------------------------------------------------
# B40 Post-school education
#-----------------------------------------------------------------------------
age_pattern <- make_age_pattern('Age')
patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

b40config <- list(
    table    = 'B40',
    patterns = patterns,
    stats    = c('gender', 'education', 'age')
)

#-----------------------------------------------------------------------------
# B41 Field of study
#-----------------------------------------------------------------------------
age_pattern <- make_age_pattern('Age')
patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

b41config <- list(
    table    = 'B41',
    patterns = patterns,
    stats    = c('gender', 'field', 'age')
)


#-----------------------------------------------------------------------------
# B42 Employment status
#-----------------------------------------------------------------------------
age_pattern <- make_age_pattern('Age')
patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

b42config <- list(
    table    = 'B42',
    patterns = patterns,
    stats    = c('gender', 'employment', 'age')
)


#-----------------------------------------------------------------------------
# B43 Employment area
#-----------------------------------------------------------------------------
age_pattern <- make_age_pattern('Age')
patterns    <- make_patterns(gender_pattern, wild_pattern, age_pattern)

b43config <- list(
    table    = 'B43',
    patterns = patterns,
    stats    = c('gender', 'employment_area', 'age')
)


#-----------------------------------------------------------------------------
# B44
#-----------------------------------------------------------------------------
occupation <- c('Occupation_(.*?)', '(Total)')
patterns    <- make_patterns(wild_pattern, occupation)

b44config <- list(
    table    = 'B44',
    patterns = patterns,
    stats    = c('employment_area', 'job_description')
)


#-----------------------------------------------------------------------------
# B45
#-----------------------------------------------------------------------------
age_pattern <- make_age_pattern()
occupation  <- c('Occupation_(.*?)', '(Total)')
patterns    <- make_patterns(gender_pattern, make_age_pattern(), occupation)

b45config <- list(
    table    = 'B45',
    patterns = patterns,
    stats    = c('gender', 'age', 'job_description')
)


#-----------------------------------------------------------------------------
# B46 Method of Travel to work
#-----------------------------------------------------------------------------
method   <- c('(.*)()',
              '(One_method|Two_methods|Three_methods)_(.*)')
patterns <- make_patterns(method, gender_pattern)

b46config <- list(
    table    = 'B46',
    patterns = patterns,
    stats    = c('methodcount', 'method', 'gender')
)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
config <- b46config
df <- read_abs('BCP', config$table, 'AUS', long=TRUE);
column_names <- df$colname
df <- split_column_names(df$colname, config)
df
df %>% as.data.frame


