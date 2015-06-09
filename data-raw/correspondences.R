
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

#=============================================================================
#
# MESHBLOCKS ALLOCATION
#
#=============================================================================
# Get names of all Meshblock correspondence files
mb_files <- list.files(corr_dir, pattern="MB_*", full.names = TRUE)
mb_files

# Read them all in
mb <- foreach(mb_file=mb_files, .combine=rbind) %do% {
    read_csv(mb_file)
}
pryr::object_size(mb)


#-----------------------------------------------------------------------------
# Allocation of MB in LGA
#-----------------------------------------------------------------------------
lga_corr_files <- list.files(corr_dir, pattern="LGA_*", full.names = TRUE)
print(lga_corr_files)
corr <- foreach(corr_file=lga_corr_files, .combine=rbind) %do% {
    read_csv(corr_file)
}
asgs.mb.lga <- corr %>%
    select(MB_CODE_2011, LGA_CODE_2011, LGA_NAME_2011) %>%
    distinct %>%
    mutate(LGA_NAME_2011 = as.factor(LGA_NAME_2011))
pryr::object_size(asgs.mb.lga)

#-----------------------------------------------------------------------------
# Allocation of MB in SA1
#-----------------------------------------------------------------------------
asgs.mb.sa1 <- mb %>% select(MB_CODE_2011, MB_CATEGORY_2011, SA1_MAINCODE_2011, SA1_7DIGITCODE_2011)
pryr::object_size(asgs.mb.sa1)

#-----------------------------------------------------------------------------
# Merge MB allocations
#   - MB -> SA1
#   - MB -> LGA
#-----------------------------------------------------------------------------
asgs.mb.alloc <- dplyr::left_join(asgs.mb.sa1, asgs.mb.lga)
pryr::object_size(asgs.mb.alloc)

asgs.mb.alloc %<>% rename(MB_CODE11  = MB_CODE_2011,
                          MB_CAT11   = MB_CATEGORY_2011,
                          SA1_MAIN11 = SA1_MAINCODE_2011,
                          SA1_7DIG11 = SA1_7DIGITCODE_2011,
                          LGA_CODE11 = LGA_CODE_2011,
                          LGA_NAME11 = LGA_NAME_2011) %>%
    mutate(MB_CAT11 = as.factor(MB_CAT11))

save(asgs.mb.alloc, file="../data/asgs.mb.alloc.rda", compress='xz')



#=============================================================================
#
# SA1 Allocation
#
#=============================================================================

#-----------------------------------------------------------------------------
# Allocation of SA1 in SA2 (from MeshBlock file)
#-----------------------------------------------------------------------------
asgs.sa1.sa2 <- mb %>%
    select(-MB_CODE_2011, -MB_CATEGORY_2011, -AREA_ALBERS_SQM) %>%
    distinct
pryr::object_size(asgs.sa1.sa2)

#-----------------------------------------------------------------------------
# Allocation of SA1
#-----------------------------------------------------------------------------
sa1 <- list(
    CED  = list(pattern='CED_*', columns=c( 'CED_CODE_2011' ,  'CED_NAME_2011')),
    NRMR = list(pattern='NRMR*', columns=c('NRMR_CODE_2011' , 'NRMR_NAME_2011')),
    POA  = list(pattern='POA_*', columns=c( 'POA_CODE_2011' ,  'POA_NAME_2011')),
    RA   = list(pattern='RA_*' , columns=c(  'RA_CODE_2011' ,   'RA_NAME_2011')),
    UCL  = list(pattern='SOSR*', columns=c( "UCL_CODE_2011" ,  'UCL_NAME_2011',
                                            "SOSR_CODE_2011" , 'SOSR_NAME_2011',
                                            "SOS_CODE_2011" ,  'SOS_NAME_2011')),
    SED  = list(pattern='SED_*', columns=c( 'SED_CODE_2011' ,  'SED_NAME_2011')),
    SSC  = list(pattern='SSC_*', columns=c( 'SSC_CODE_2011' ,  'SSC_NAME_2011'))
)

level <- 'SSC'
sa1_allocations <- foreach(level=names(sa1)) %do% {
    info <- sa1[[level]]
    all_columns <- c('SA1_MAINCODE_2011', info$columns)
    corr_file <- list.files(corr_dir, pattern=info$pattern, full.names = TRUE)
    print(corr_file)
    corr <- read_csv(corr_file)
    tmp <- corr %>%
        select_(.dots = all_columns) %>%
        distinct
    pryr::object_size(tmp)
    tmp
}


#-----------------------------------------------------------------------------
# Allocation of SA2 in TR
#-----------------------------------------------------------------------------
corr_file <- list.files(corr_dir, pattern='TR_', full.names = TRUE)
print(corr_file)
corr <- read_csv(corr_file)
asgs.sa2.tr <- corr %>%
    select(SA2_MAINCODE_2011, TR_Code_2011, TR_Name_2011) %>%
    distinct %>%
    rename(TR_CODE11  = TR_Code_2011,
           TR_NAME11  = TR_Name_2011)
pryr::object_size(asgs.sa2.tr)

#-----------------------------------------------------------------------------
# Allocation of SA2 in SUA
#-----------------------------------------------------------------------------
corr_file <- list.files(corr_dir, pattern='SA2_SUA_', full.names = TRUE)
print(corr_file)
corr <- read_csv(corr_file)
asgs.sa2.sua <- corr %>%
    select(SA2_MAINCODE_2011, SUA_CODE_2011, SUA_NAME_2011) %>%
    distinct %>%
    rename(SUA_CODE11 = SUA_CODE_2011,
           SUA_NAME11 = SUA_NAME_2011)
pryr::object_size(asgs.sa2.sua)



#-----------------------------------------------------------------------------
# Merge SA1 allocations
#-----------------------------------------------------------------------------
sa1_allocations[['SA2']] <- asgs.sa1.sa2
sa1_allocations[['TR' ]] <- asgs.sa2.tr
sa1_allocations[['SUA']] <- asgs.sa2.sua
asgs.sa1.alloc <- Reduce(dplyr::left_join, sa1_allocations)
pryr::object_size(asgs.sa1.alloc)


asgs.sa1.alloc <- asgs.sa1.alloc %>%
    select(SA1_MAINCODE_2011, SA1_7DIGITCODE_2011, SA2_MAINCODE_2011, SA2_5DIGITCODE_2011, everything()) %>%
    rename(SA1_MAIN11 = SA1_MAINCODE_2011,
           SA1_7DIG11 = SA1_7DIGITCODE_2011,
           SA2_MAIN11 = SA2_MAINCODE_2011,
           SA2_5DIG11 = SA2_5DIGITCODE_2011,
           CED_CODE   = CED_CODE_2011,
           CED_NAME   = CED_NAME_2011,
           NRMR_CODE  = NRMR_CODE_2011,
           NRMR_NAME  = NRMR_NAME_2011,
           POA_CODE   = POA_CODE_2011,
           POA_NAME   = POA_NAME_2011,
           RA_CODE11  = RA_CODE_2011,
           RA_NAME11  = RA_NAME_2011,
           UCL_CODE11 = UCL_CODE_2011,
           UCL_NAME11 = UCL_NAME_2011,
           SSR_CODE11 = SOSR_CODE_2011,
           SSR_NAME11 = SOSR_NAME_2011,
           SOS_CODE11 = SOS_CODE_2011,
           SOS_NAME11 = SOS_NAME_2011,
           SED_CODE   = SED_CODE_2011,
           SED_NAME   = SED_NAME_2011,
           SSC_CODE   = SSC_CODE_2011,
           SSC_NAME   = SSC_NAME_2011,
           GCC_CODE11 = GCCSA_CODE_2011,
           GCC_NAME11 = GCCSA_NAME_2011,
           STE_CODE11 = STATE_CODE_2011,
           STE_NAME11 = STATE_NAME_2011,
           SA2_NAME11 = SA2_NAME_2011,
           SA3_CODE11 = SA3_CODE_2011,
           SA3_NAME11 = SA3_NAME_2011,
           SA4_CODE11 = SA4_CODE_2011,
           SA4_NAME11 = SA4_NAME_2011)

# Turn all the NAME columns into factors
name_names <- colnames(asgs.sa1.alloc)[grepl('NAME', colnames(asgs.sa1.alloc))]
name_names <- c(name_names, 'GCC_CODE11')
for (name in name_names) {
    asgs.sa1.alloc[[name]] <- as.factor(asgs.sa1.alloc[[name]])
}

save(asgs.sa1.alloc, file="../data/asgs.sa1.alloc.rda", compress='xz')


