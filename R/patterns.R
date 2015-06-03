
#=============================================================================
# make patterns for column splitting
#=============================================================================

make_age_pattern <- function(prefix="") {
    age_range_basic <- c('(.*)_years', '(.*_years_and_over)', '(Total)')
    if (prefix != "")
        age_range <- c(paste(prefix, age_range_basic[1:2], sep='_'), '(Total)')
    age_range
}

gender_pattern    <- '(Males|Females|Persons)'
wild_pattern      <- '(.*)'

make_patterns <- function(...) {
    patterns <- expand.grid(...)
    patterns <- apply(patterns, 1, paste0, collapse="_")
    paste0('^', patterns, '$')
}