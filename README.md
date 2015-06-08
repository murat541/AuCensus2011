# AuCensus2011
Tools for parsing 2011 Census DataPacks from the Australian Bureau of Statistics

Install:
```{r}
devtools::install_github("coolbutuseless/AuCensus2011")
```


Geography Home
==============

* [ABS Geography homepage](http://www.abs.gov.au/websitedbs/D3310114.nsf/home/Geography)
* [Index of all the ABS Geography Publications](http://www.abs.gov.au/websitedbs/D3310114.nsf/home/ABS+Geography+Publications]
* [ABS Betaworks visualisation tool for ASGS boundaries) (http://betaworks.abs.gov.au/betaworks/betaworks.nsf/dx/asgs-boundaries-online.htm)
* `ABS2011/Correspondences` [1270.0.55.006 - Australian Statistical Geography Standard (ASGS): Correspondences, July 2011](http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.006July%202011?OpenDocument)  

Australian Statistical Geography Standard
------------------------------------------

Links to core ASGS pages with CSV correnspondes between levels and ESRI Shapefiles.

* `ABS2011/asgs1` [1270.0.55.001 - Australian Statistical Geography Standard (ASGS): Volume 1 - Main Structure and Greater Capital City     Statistical Areas, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.001)
    * Includes MeshBlock shapefiles and correspondences
* `ABS2011/asgs2` [1270.0.55.002 - Australian Statistical Geography Standard (ASGS): Volume 2 - Indigenous Structure, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.002)
* `ABS2011/asgs3` [1270.0.55.003 - Australian Statistical Geography Standard (ASGS): Volume 3 - Non ABS Structures, July 2014 ](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.003)
* `ABS2011/asgs4` [1270.0.55.004 - Australian Statistical Geography Standard (ASGS): Volume 4 - Significant Urban Areas, Urban Centres and Localities, Section of State, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.004)
* `ABS2011/asgs5` [1270.0.55.005 - Australian Statistical Geography Standard (ASGS): Volume 5 - Remoteness Structure, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.005)

Meshblocks
----------
All the ASGS regions are based upon Meshblocks
* `ABS2011/Meshblocks2006` Old 2006 Meshblock boundaries.  Deprecated. Instead, refer to *ASGS Volume 1 - Main Structure* data. [1209.0.55.002 - Mesh Blocks Digital Boundaries, Australia, 2006](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1209.0.55.002/)
* http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/2074.0Main+Features12011
    * The only released meshblock statistics are `Persons usually resident` and `Dwellings` , available in the the file: [CensusCounts_MB_2011_AUST.csv](http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&censuscounts_mb_2011_aust.csv&2074.0&Data%20Cubes&B573366A0A37265ACA257AD0000F1E5A&0&2011&11.12.2012&Latest)