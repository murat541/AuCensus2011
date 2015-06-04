
#=============================================================================
# The ABS Census data is pretty big. To put all the data for all geographical
# areas would make it unweildy.  Also, analyses would mostly only take place
# at one or two geographical levels at a time, and pulling in data for all
# regions is unnecessary.
#
# With that in mind, these functions aim to create region-specific collections
# of ABS data.
#
# Example:
#         create_package(level='STE', dest_root="../region_packages/")
# Output:
#    - complete package in "../region_packages/AuCensus2011.STE"
#=============================================================================

library(whisker)

render_template <- function(data, template_file, output_file) {
    template <- readLines(template_file)
    writeLines(whisker.render(template, data), output_file)
}


load("data/tableconfig.rda")
load("data/geog.desc.rda")
package_path <- paste("../AuCensus2011", this_level, sep=".")

this_level <- 'STE'

# Save the region descriptions for just this level
region.description <- geog.desc %>% filter(level==this_level)
data_path          <- paste0(package_path, "/data/region.description.rda")
save(region.description, file=data_path)

# Copy a templated version of the docs for this file.
render_template(data          =  list(level = this_level),
                template_file = "package-templates/region.description.R",
                output_file   = paste0(package_path, "/R/region.description.R"))


# Load the table into a named variable
config <- tableconfig[['B01']]
for (config in tableconfig) {
    table  <- config$table
    assign(table, read_abs(profile='BCP', table=table, level=this_level, long=TRUE))

    # Save the table to the given output directory
    data_path    <- paste0(package_path, "/data/", table, ".rda")
    save(list=c(table), file=data_path)

    # Save a templated version of the docs for this file.
    data          <- list(level=this_level, table=config$table, desc=config$desc)
    template_file <- "package-templates/Bxx.R"
    output_file   <- paste0(package_path, "/R/", table, ".R")
    render_template(data, template_file, output_file)
}




