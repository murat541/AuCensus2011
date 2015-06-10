
source("pattern-helper.R")  # helper code for building regex patterns

#=============================================================================
# Table configuration information
#=============================================================================

make_XCP_tableconfig <- function() {
    tableconfig <- list()

    #-----------------------------------------------------------------------------
    table <- 'X01'
    desc  <- 'Country of Birth of Person by Age by Sex'
    #-----------------------------------------------------------------------------
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = make_patterns(gender_pattern, wild_pattern, make_age_pattern('Age')),
        stats    = c('gender', 'country', 'age')
    )


    #-----------------------------------------------------------------------------
    table <- 'X02'
    desc  <- 'Country of Birth (Major Group) of Person by Age by Sex'
    #-----------------------------------------------------------------------------
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = make_patterns(gender_pattern, wild_pattern, make_age_pattern('Age')),
        stats    = c('gender', 'country_major_group', 'age')
    )

    #-----------------------------------------------------------------------------
    table <- 'X03'
    desc  <- 'Country of Birth (Minor Group) of Person by Year of Arrival in Australia'
    #-----------------------------------------------------------------------------
    year_pattern <- c('(Before_1997)', '(1997_2000)', '(2001_2005)', '(2006)', '(2007)', '(2008)',
                      '(2009)', '(2010)', '(2011)', '(Year_of_arrival_not_stated)', '(Total)')
    country_group <- c('(Oceania_and_Antarctica|North_West_Europe|Southern_and_Eastern_Europe|North_Africa_and_the_Middle_East|South_East_Asia|North_East_Asia|Southern_and_Central_Asia|Americas|Sub_Saharan_Africa|Other|Total)')
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = make_patterns(country_group, wild_pattern, year_pattern),
        stats    = c('country_minor_group', 'group',  'year_of_arrival')
    )

    #-----------------------------------------------------------------------------
    table <- 'X04'
    desc  <- 'Proficiency in Spoken English/Language by Year of Arrival in Australia by Age'
    #-----------------------------------------------------------------------------

    #-----------------------------------------------------------------------------
    table <- 'X05'
    desc  <- 'Language Spoken at Home by Proficiency in Spoken English/Language by Sex'
    #-----------------------------------------------------------------------------


    #-----------------------------------------------------------------------------
    table <- 'X06'
    desc  <- 'Ancestry by Birthplace of Parents by Sex'
    #-----------------------------------------------------------------------------
    parent_pattern <- '(Father_only_born_overseas|Mother_only_born_overseas|Both_parents_born_overseas|Both_parents_born_in_Australia|Birthplace_not_stated|Total_responses)'
    tableconfig[[table]] <- list(
        table    = table,
        desc     = desc,
        patterns = make_patterns(gender_pattern, wild_pattern, parent_pattern),
        stats    = c('gender', 'zncestry',  'parents_status')
    )

    config <- tableconfig[[table]]
    df <- read_abs('XCP', table, 'SA4')
    colnames(df)
    split_column_names(colnames(df)[-1], config) %>% as.data.frame %>% head(100)


    tableconfig
}