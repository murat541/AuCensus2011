
library(magrittr)
library(dplyr)
library(readr)
library(knitr)

# Original CSV file from
url <- "http://www.abs.gov.au/websitedbs/censushome.nsf/home/datapacksdetails/$file/2011Census_geog_desc_1st_and_2nd_release.csv"
filename <- basename(url)

if (!file.exists(filename)) {
    download.file(url, filename, quiet = TRUE)
}

asgs.code <- readr::read_csv("2011Census_geog_desc_1st_and_2nd_release.csv")

# For some reason there are some empty columns in the data set. Ignore them.
asgs.code <- asgs.code[, 1:5]

# Some renaming.
asgs.code %<>% select(level=Level, code=Code, label=Label, area=`Area sqkm`) %>%
    mutate(level = as.factor(level))

save(asgs.code, file='../data/asgs.code.rda', compress='bzip2')

knitr::kable(head(asgs.code, 20), caption="Statistical Area Information")