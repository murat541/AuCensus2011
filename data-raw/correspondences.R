
library(foreach)
library(magrittr)
library(dplyr)
library(readr)

#-----------------------------------------------------------------------------
# Allocation tables: http://www.abs.gov.au/ausstats/abs@.nsf/Latestproducts/1216.0.55.004Main%20Features52012?opendocument&tabname=Summary&prodno=1216.0.55.004&issue=2012&num=&view=
# "Allocation tables, also known as hierarchy tables, describe a hierarchy of regions where the smaller regions fit precisely within the larger regions"
# Correspondences
# extracted from Volumes1-5 of ASGS definition
#-----------------------------------------------------------------------------
corr_dir <- paste(Sys.getenv('HOME'), "/projectsdata/ABS2011/ASGS.Correspondence", sep="/")

#-----------------------------------------------------------------------------
# Load and merge all the meshblock equivalences
#-----------------------------------------------------------------------------
# Get names of all Meshblock correspondence files
mb_files <- list.files(corr_dir, pattern="MB_*", full.names = TRUE)
mb_files

# Read them all in
mb <- foreach(mb_file=mb_files, .combine=rbind) %do% {
    read_csv(mb_file)
}
pryr::object_size(mb)

# Delete redundant NAME columns (get these from asgs.code)
mb <- mb %>% select(MB_CODE_2011, MB_CATEGORY_2011, SA1_MAINCODE_2011, SA1_7DIGITCODE_2011,
                    SA2_MAINCODE_2011, SA2_5DIGITCODE_2011, SA3_CODE_2011, SA4_CODE_2011,
                    GCCSA_CODE_2011, STATE_CODE_2011) %>%
    mutate(MB_CATEGORY_2011 = as.factor(MB_CATEGORY_2011),
           GCCSA_CODE_2011  = as.factor(GCCSA_CODE_2011))
pryr::object_size(mb)


#-----------------------------------------------------------------------------
# Allocation of MB in SA1
#-----------------------------------------------------------------------------
asgs.mb.sa1 <- mb %>% select(MB_CODE_2011, MB_CATEGORY_2011, SA1_MAINCODE_2011, SA1_7DIGITCODE_2011)
pryr::object_size(asgs.mb.sa1)



#-----------------------------------------------------------------------------
# Allocation of SA1 in SA2
#-----------------------------------------------------------------------------
asgs.sa1.sa2 <- mb %>%
    select(SA1_MAINCODE_2011, SA1_7DIGITCODE_2011, SA2_MAINCODE_2011, SA2_5DIGITCODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.sa2)



#-----------------------------------------------------------------------------
# Allocation of SA2 in SA3
#-----------------------------------------------------------------------------
asgs.sa2.sa3 <- mb %>%
    select(SA2_MAINCODE_2011, SA2_5DIGITCODE_2011, SA3_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa2.sa3)

#-----------------------------------------------------------------------------
# Allocation of SA3 in SA4
#-----------------------------------------------------------------------------
asgs.sa3.sa4 <- mb %>%
    select(SA3_CODE_2011, SA4_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa3.sa4)


#-----------------------------------------------------------------------------
# Allocation of SA4 in GCCSA
#-----------------------------------------------------------------------------
asgs.sa4.gccsa <- mb %>%
    select(SA4_CODE_2011, GCCSA_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa4.gccsa)


#-----------------------------------------------------------------------------
# Allocation of SA4 in STE
#-----------------------------------------------------------------------------
asgs.sa4.ste <- mb %>%
    select(SA4_CODE_2011, STATE_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa4.ste)


include_files <- c('CED', 'LGA', 'NRMR', 'POA', 'SA1_2011_RA', 'SA1_UCL_SOSR_SOS_2011_AUST', 'SA2_SUA', 'SED', 'SSC', 'STE', 'TR_2011')
corr_files <- setdiff(list.files(corr_dir, pattern="*.csv", full.names = TRUE), mb_files)
pattern <- paste(include_files, collapse="|")
corr_files <- corr_files[grepl(pattern, corr_files)]



#-----------------------------------------------------------------------------
# Allocation of CED in GCCSA
#-----------------------------------------------------------------------------
corr_file <- corr_files[1]
print(corr_file) # CED
corr <- read_csv(corr_file)
asgs.sa1.ced <- corr %>%
    select(SA1_MAINCODE_2011, CED_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.ced)

#-----------------------------------------------------------------------------
# Allocation of MB in LGA
#-----------------------------------------------------------------------------
lga_corr_files <- corr_files[grepl("LGA", corr_files)]
print(lga_corr_files) # CED
corr <- foreach(corr_file=lga_corr_files, .combine=rbind) %do% {
    read_csv(corr_file)
}
asgs.mb.lga <- corr %>%
    select(MB_CODE_2011, LGA_CODE_2011) %>%
    distinct
pryr::object_size(asgs.mb.lga)


#-----------------------------------------------------------------------------
# Allocation of SA1 in NRMR
#-----------------------------------------------------------------------------
corr_file <- corr_files[11]
print(corr_file) # NRMR
corr <- read_csv(corr_file)
asgs.sa1.nrmr <- corr %>%
    select(SA1_MAINCODE_2011, NRMR_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.nrmr)


#-----------------------------------------------------------------------------
# Allocation of SA1 in POA
#-----------------------------------------------------------------------------
corr_file <- corr_files[12]
print(corr_file) # POA
corr <- read_csv(corr_file)
asgs.sa1.poa <- corr %>%
    select(SA1_MAINCODE_2011, POA_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.poa)


#-----------------------------------------------------------------------------
# Allocation of SA1 in RA
#-----------------------------------------------------------------------------
corr_file <- corr_files[13]
print(corr_file) # RA
corr <- read_csv(corr_file)
asgs.sa1.ra <- corr %>%
    select(SA1_MAINCODE_2011, RA_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.ra)


#-----------------------------------------------------------------------------
# Allocation of SA1 in UCL, SOSR, SOS
#-----------------------------------------------------------------------------
corr_file <- corr_files[14]
print(corr_file) # UCL, SOSR, SOS
corr <- read_csv(corr_file)
asgs.sa1.ucl_sosr_sos <- corr %>%
    select(SA1_MAINCODE_2011, UCL_CODE_2011, SOSR_CODE_2011, SOS_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.ucl_sosr_sos)

#-----------------------------------------------------------------------------
# Allocation of SA2 in SUA
#-----------------------------------------------------------------------------
corr_file <- corr_files[15]
print(corr_file)
corr <- read_csv(corr_file)
asgs.sa2.sua <- corr %>%
    select(SA2_MAINCODE_2011, SUA_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa2.sua)


#-----------------------------------------------------------------------------
# Allocation of SA1 in SED
#-----------------------------------------------------------------------------
corr_file <- corr_files[16]
print(corr_file)
corr <- read_csv(corr_file)
asgs.sa1.sed <- corr %>%
    select(SA2_MAINCODE_2011, SED_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.sed)


#-----------------------------------------------------------------------------
# Allocation of SA1 in SED
#-----------------------------------------------------------------------------
corr_file <- corr_files[17]
print(corr_file)
corr <- read_csv(corr_file)
asgs.sa1.ssc <- corr %>%
    select(SA1_MAINCODE_2011, SSC_CODE_2011) %>%
    distinct
pryr::object_size(asgs.sa1.ssc)


#-----------------------------------------------------------------------------
# Allocation of SA1 in SED
#-----------------------------------------------------------------------------
corr_file <- corr_files[19]
print(corr_file)
corr <- read_csv(corr_file)
asgs.sa2.tr <- corr %>%
    select(SA2_MAINCODE_2011, TR_Code_2011) %>%
    distinct %>%
    mutate(TR_Code_2011 = as.factor(TR_Code_2011))
pryr::object_size(asgs.sa2.tr)



#=============================================================================
# Merge SA1 allocations
#=============================================================================
sa1_allocations <- list(asgs.sa1.sa2, asgs.sa1.ssc, asgs.sa1.ced,
                        asgs.sa1.ucl_sosr_sos, asgs.sa1.ra,
                        asgs.sa1.poa, asgs.sa1.nrmr)

asgs.sa1 <- Reduce(dplyr::left_join, sa1_allocations)
pryr::object_size(asgs.sa1)


#=============================================================================
# Merge SA2 allocations
#=============================================================================
sa2_allocations <- list(asgs.sa2.sa3, asgs.sa2.tr, asgs.sa2.sua)

asgs.sa2 <- Reduce(dplyr::left_join, sa2_allocations)
pryr::object_size(asgs.sa2)
dim(asgs.sa2)


#=============================================================================
# Merge MB allocations
#=============================================================================
asgs.mb <- dplyr::left_join(asgs.mb.sa1, asgs.mb.lga)
pryr::object_size(asgs.mb)
dim(asgs.mb)

