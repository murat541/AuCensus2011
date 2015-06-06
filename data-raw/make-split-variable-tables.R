
library(stringr)
source('make-tableconfig.R')

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

    # make everything factors
    labels <- labels %>% mutate(colname = as.factor(colname))
    for (stat in config$stats) {
        labels[[stat]] <- as.factor(labels[[stat]])
    }

    labels
}

#-----------------------------------------------------------------------------
# Create and save tableconfig as a data element of this package
#-----------------------------------------------------------------------------
tableconfig <- make_tableconfig()
save(tableconfig, file="../data/tableconfig.rda", compress='bzip2')

#-----------------------------------------------------------------------------
# Make a list of components corresponding to splitting the column name
#-----------------------------------------------------------------------------
library(foreach)

split.variables <- foreach(config=tableconfig) %do% {
    df <- read_abs('BCP', config$table, 'AUS', long=TRUE);
    column_names <- df$colname
    split_columns <- split_column_names(df$colname, config)
    split_columns
}
names(split.variables) <- names(tableconfig)
save(split.variables, file="../data/split.variables.rda", compress='bzip2')

