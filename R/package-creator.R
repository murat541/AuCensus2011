
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
library(dplyr)

render_template <- function(data, template_file, output_file) {
    template <- readLines(template_file)
    writeLines(whisker::whisker.render(template, data), output_file)
}

create_level_package <- function(this_level) {
    load("data/tableconfig.rda")
    load("data/asgs.info.rda")
    load("data/asgs.code.rda")



    this_level_desc <- (asgs.info %>% filter(level == this_level))$desc
    package_path <- paste("../AuCensus2011", this_level, sep=".")


    #-----------------------------------------------------------------------------
    # Copy across the package skeleton
    #-----------------------------------------------------------------------------
    # directory structure
    system(paste("cp -R 'package-skeleton/'", package_path))

    # project template
    command <- sprintf("cp 'package-templates/AuCensus2011.XXX.Rproj' %s/AuCensus2011.%s.Rproj",
                       package_path,
                       this_level)
    system(command)

    #-----------------------------------------------------------------------------
    # Render templated version of the README.md file.
    #-----------------------------------------------------------------------------
    render_template(data          =  list(level = this_level, level_desc = this_level_desc),
                    template_file = "package-templates/README.md",
                    output_file   = paste0(package_path, "/README.md"))


    #-----------------------------------------------------------------------------
    # Render templated version of the DESCRIPTION file.
    #-----------------------------------------------------------------------------
    render_template(data          =  list(level = this_level, level_desc = this_level_desc),
                    template_file = "package-templates/DESCRIPTION",
                    output_file   = paste0(package_path, "/DESCRIPTION"))


    #-----------------------------------------------------------------------------
    # Save the region descriptions for just this level
    #-----------------------------------------------------------------------------
    region.description <- asgs.code %>% filter(level==this_level)
    data_path          <- paste0(package_path, "/data/region.description.rda")
    save(region.description, file=data_path)

    #-----------------------------------------------------------------------------
    # Render a templated version of the docs for this region.description
    #-----------------------------------------------------------------------------
    render_template(data          =  list(level = this_level, level_desc = this_level_desc),
                    template_file = "package-templates/region.description.R",
                    output_file   = paste0(package_path, "/R/region.description.R"))


    #-----------------------------------------------------------------------------
    # Load all ABS tables for this level, and save in long format to the
    # package directory
    #-----------------------------------------------------------------------------
    config <- tableconfig[['B01']]
    for (config in tableconfig) {
        table  <- config$table
        # Load the table into a named variable
        assign(table, read_abs(profile='BCP', table=table, level=this_level))

        # Save the data to the given output directory
        data_path    <- paste0(package_path, "/data/", table, ".rda")
        save(list=c(table), file=data_path, compress='xz')

        # Render a templated version of the docs for this file.
        data          <- list(level=this_level, level_desc = this_level_desc, table=config$table, desc=config$desc)
        template_file <- "package-templates/Bxx.R"
        output_file   <- paste0(package_path, "/R/", table, ".R")
        render_template(data, template_file, output_file)
    }


    # Check/build the package
    devtools::check(package_path)
    devtools::build(package_path)



}