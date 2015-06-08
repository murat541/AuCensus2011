
asgs.volume.info <- list(
    'GCCSA' = list(volume="1270055001"),
    'SA1'   = list(volume="1270055001"),
    'SA2'   = list(volume="1270055001"),
    'SA3'   = list(volume="1270055001"),
    'SA4'   = list(volume="1270055001"),
    'STE'   = list(volume="1270055001"),
    'IARE'  = list(volume="1270055002"),
    'ILOC'  = list(volume="1270055002"),
    'IREG'  = list(volume="1270055002"),
    'ADD'   = list(volume="1270055003"),
    'CED'   = list(volume="1270055003"),
    'LGA'   = list(volume="1270055003"),
    'NRMR'  = list(volume="1270055003"),
    'POA'   = list(volume="1270055003"),
    'SED'   = list(volume="1270055003"),
    'SSC'   = list(volume="1270055003"),
    'TR'    = list(volume="1270055003"),
    'SOS'   = list(volume="1270055004"),
    'SOSR'  = list(volume="1270055004"),
    'SUA'   = list(volume="1270055004"),
    'UCL'   = list(volume="1270055004"),
    'RA'    = list(volume="1270055005")
)

asgs.info <- asgs.volume.info

for (level in names(asgs.info)) {
    sf <- asgs_load_shapefile(level)
    asgs.info[[level]]$columns = colnames(sf@data)
}

save(asgs.info, file="../data/asgs.info.rda", compress='bzip2')
