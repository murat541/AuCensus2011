# AuCensus2011
Tools for parsing 2011 Census DataPacks from the Australian Bureau of Statistics

Install:
```{r}
devtools::install_github("coolbutuseless/AuCensus2011")
```

DataPacks
=========
* Need an account to access datapacks. 
* TODO: Walk through account signup
* TODO: Detail which datapacks to download i.e. always choose the the "AUSTRALIA All Regions" with "long header"

Geography Home
==============

* [ABS Geography homepage](http://www.abs.gov.au/websitedbs/D3310114.nsf/home/Geography)
* [Index of all the ABS Geography Publications](http://www.abs.gov.au/websitedbs/D3310114.nsf/home/ABS+Geography+Publications)
* [ABS Betaworks visualisation tool for ASGS boundaries](http://betaworks.abs.gov.au/betaworks/betaworks.nsf/dx/asgs-boundaries-online.htm)
* `ABS2011/Correspondences` [1270.0.55.006 - Australian Statistical Geography Standard (ASGS): Correspondences, July 2011](http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.006July%202011?OpenDocument)  

Australian Statistical Geography Standard
------------------------------------------

Links to core ASGS pages with CSV correnspondes between levels and ESRI Shapefiles.

* `ABS2011/ASGS1` [1270.0.55.001 - Australian Statistical Geography Standard (ASGS): Volume 1 - Main Structure and Greater Capital City     Statistical Areas, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.001)
    * Includes MeshBlock shapefiles and correspondences
* `ABS2011/ASGS2` [1270.0.55.002 - Australian Statistical Geography Standard (ASGS): Volume 2 - Indigenous Structure, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.002)
* [1270.0.55.003 - Australian Statistical Geography Standard (ASGS): Volume 3 - Non ABS Structures, July 2014 ](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.003)
    * Non-ABS structures have been updated every year
    * `ABS2011/ASGS3.July2011`
    * `ABS2011/ASGS3.July2012`
    * `ABS2011/ASGS3.July2013`
    * `ABS2011/ASGS3.July2014`
* `ABS2011/ASGS4` [1270.0.55.004 - Australian Statistical Geography Standard (ASGS): Volume 4 - Significant Urban Areas, Urban Centres and Localities, Section of State, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.004)
* `ABS2011/ASGS5` [1270.0.55.005 - Australian Statistical Geography Standard (ASGS): Volume 5 - Remoteness Structure, July 2011](http://www.abs.gov.au/ausstats/abs@.nsf/mf/1270.0.55.005)

Meshblocks
----------
* `ABS2011/MeshblockStats/censuscounts_mb_2011_aust.csv` The only released statistics at the Meshblock level are `Persons usually resident` and `Dwellings`[2074.0 - Census of Population and Housing: Mesh Block Counts, 2011](http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/2074.0Main+Features12011)