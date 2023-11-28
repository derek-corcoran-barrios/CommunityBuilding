Community generation for plants in Denmark
================
Derek Corcoran
28/11, 2023

- <a href="#1-preparations" id="toc-1-preparations">1 Preparations</a>
- <a href="#2-taxonomic-cleaning" id="toc-2-taxonomic-cleaning">2
  Taxonomic cleaning</a>
  - <a href="#21-working-only-with-plants"
    id="toc-21-working-only-with-plants">2.1 Working only with plants</a>
- <a href="#3-presence-extraction" id="toc-3-presence-extraction">3
  Presence extraction</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

The goal of the CommunityBuilding repository is to thoroughly document
the generation of community data for Denmark. This includes several
processes

1.  Taxonomic cleaning
2.  Presence extraction
3.  Buffer generation
4.  Simple Nature type SDM
5.  Potential community generation
6.  Summary metrics

# 1 Preparations

First we load all necessary packages

``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.2.3
library(terra)
#> Warning: package 'terra' was built under R version 4.2.3
#> terra 1.7.39
library(tidyterra)
#> Warning: package 'tidyterra' was built under R version 4.2.3
#> 
#> Attaching package: 'tidyterra'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(SDMWorkflows)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
#> Please note that rgdal will be retired during October 2023,
#> plan transition to sf/stars/terra functions using GDAL and PROJ
#> at your earliest convenience.
#> See https://r-spatial.org/r/2023/05/15/evolution4.html and https://github.com/r-spatial/evolution
#> rgdal: version: 1.6-7, (SVN revision 1203)
#> Geospatial Data Abstraction Library extensions to R successfully loaded
#> Loaded GDAL runtime: GDAL 3.5.2, released 2022/09/02
#> Path to GDAL shared files: C:/Users/au687614/AppData/Local/R/win-library/4.2/rgdal/gdal
#> GDAL binary built with GEOS: TRUE 
#> Loaded PROJ runtime: Rel. 8.2.1, January 1st, 2022, [PJ_VERSION: 821]
#> Path to PROJ shared files: C:/Users/au687614/AppData/Local/R/win-library/4.2/rgdal/proj
#> PROJ CDN enabled: FALSE
#> Linking to sp version:2.0-0
#> To mute warnings of possible GDAL/OSR exportToProj4() degradation,
#> use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.
#> rgeos version: 0.6-4, (SVN revision 699)
#>  GEOS runtime version: 3.9.3-CAPI-1.14.3 
#>  Please note that rgeos will be retired during October 2023,
#> plan transition to sf or terra functions using GEOS at your earliest convenience.
#> See https://r-spatial.org/r/2023/05/15/evolution4.html for details.
#>  GEOS using OverlayNG
#>  Linking to sp version: 2.0-0 
#>  Polygon checking: TRUE
library(BDRUtils)
library(data.table)
#> Warning: package 'data.table' was built under R version 4.2.3
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:terra':
#> 
#>     shift
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
library(readxl)
#> Warning: package 'readxl' was built under R version 4.2.3
```

# 2 Taxonomic cleaning

We first read the dataset from Arter.dk and clean the taxonomy according
to GBIF nomenclature

``` r
ToClean <- readxl::read_xlsx("2022-09-21.xlsx") |>
  janitor::clean_names() |>
  dplyr::filter(rige == "Plantae") |> 
  dplyr::filter(taxonrang %in% c("Art", "Form", "Superart", "Underart", "Varietet")) |> 
  dplyr::filter(herkomst != "Introduceret" | is.na(herkomst)) |> 
  dplyr::select(videnskabeligt_navn, taxonrang) |> 
  dplyr::distinct()
#> Warning: Expecting logical in AK1030 / R1030C37: got 'Bilag 1'
#> Warning: Expecting logical in K1260 / R1260C11: got 'Stor labyrintedderkop'
#> Warning: Expecting logical in AK1653 / R1653C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK1663 / R1663C37: got 'Bilag 1'
#> Warning: Expecting logical in K2019 / R2019C11: got 'Hvid-el'
#> Warning: Expecting logical in K2796 / R2796C11: got 'Stribet havkat'
#> Warning: Expecting logical in AK2810 / R2810C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK2812 / R2812C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY3235 / R3235C77: got 'Brodmyrer'
#> Warning: Expecting logical in BY3236 / R3236C77: got 'Brodmyrer'
#> Warning: Expecting logical in AK3362 / R3362C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK3364 / R3364C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK3594 / R3594C37: got 'Bilag 1'
#> Warning: Expecting logical in BY3776 / R3776C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY3777 / R3777C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK4450 / R4450C37: got 'Bilag 1'
#> Warning: Expecting logical in AK5294 / R5294C37: got 'Bilag 1'
#> Warning: Expecting logical in BY5893 / R5893C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5894 / R5894C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5895 / R5895C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5896 / R5896C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5897 / R5897C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5898 / R5898C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5899 / R5899C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5900 / R5900C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5901 / R5901C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5902 / R5902C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5903 / R5903C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5904 / R5904C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5905 / R5905C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5906 / R5906C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5907 / R5907C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY5908 / R5908C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in AK6205 / R6205C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK6206 / R6206C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK6208 / R6208C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY6944 / R6944C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6945 / R6945C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6946 / R6946C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6947 / R6947C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6948 / R6948C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6949 / R6949C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6950 / R6950C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6951 / R6951C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6952 / R6952C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6953 / R6953C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6954 / R6954C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY6955 / R6955C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in K7388 / R7388C11: got 'Karl Johan'
#> Warning: Expecting logical in AK7606 / R7606C37: got 'Bilag 1'
#> Warning: Expecting logical in BY7852 / R7852C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY7853 / R7853C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY7854 / R7854C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in AK8020 / R8020C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK8026 / R8026C37: got 'Bilag 1'
#> Warning: Expecting logical in AK8319 / R8319C37: got 'Bilag 1'
#> Warning: Expecting logical in AK8346 / R8346C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK8715 / R8715C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK8716 / R8716C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK8720 / R8720C37: got 'Bilag 1'
#> Warning: Expecting logical in AK8732 / R8732C37: got 'Bilag 1'
#> Warning: Expecting logical in BY9237 / R9237C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY9238 / R9238C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY9239 / R9239C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY9240 / R9240C77: got 'Skælmyrer'
#> Warning: Expecting logical in AK9491 / R9491C37: got 'Bilag 1'
#> Warning: Expecting logical in AK10342 / R10342C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK11221 / R11221C37: got 'Bilag 1'
#> Warning: Expecting logical in AK11231 / R11231C37: got 'Bilag 1'
#> Warning: Expecting logical in AK11620 / R11620C37: got 'Bilag 1'
#> Warning: Expecting logical in AK12233 / R12233C37: got 'Bilag 1'
#> Warning: Expecting logical in AK12234 / R12234C37: got 'Bilag 1'
#> Warning: Expecting logical in AK12369 / R12369C37: got 'Bilag 1'
#> Warning: Expecting logical in AK12370 / R12370C37: got 'Bilag 1'
#> Warning: Expecting logical in AK12373 / R12373C37: got 'Bilag 1'
#> Warning: Expecting logical in CA12415 / R12415C79: got 'Ciini'
#> Warning: Expecting logical in CA12416 / R12416C79: got 'Ciini'
#> Warning: Expecting logical in CA12417 / R12417C79: got 'Ciini'
#> Warning: Expecting logical in CA12418 / R12418C79: got 'Ciini'
#> Warning: Expecting logical in CA12419 / R12419C79: got 'Ciini'
#> Warning: Expecting logical in CA12420 / R12420C79: got 'Ciini'
#> Warning: Expecting logical in CA12421 / R12421C79: got 'Ciini'
#> Warning: Expecting logical in CA12422 / R12422C79: got 'Ciini'
#> Warning: Expecting logical in CA12423 / R12423C79: got 'Ciini'
#> Warning: Expecting logical in CA12424 / R12424C79: got 'Ciini'
#> Warning: Expecting logical in CA12425 / R12425C79: got 'Ciini'
#> Warning: Expecting logical in CA12426 / R12426C79: got 'Ciini'
#> Warning: Expecting logical in CA12427 / R12427C79: got 'Ciini'
#> Warning: Expecting logical in CA12428 / R12428C79: got 'Ciini'
#> Warning: Expecting logical in CA12429 / R12429C79: got 'Ciini'
#> Warning: Expecting logical in CA12430 / R12430C79: got 'Ciini'
#> Warning: Expecting logical in CA12431 / R12431C79: got 'Ciini'
#> Warning: Expecting logical in CA12432 / R12432C79: got 'Ciini'
#> Warning: Expecting logical in CA12433 / R12433C79: got 'Ciini'
#> Warning: Expecting logical in AK12624 / R12624C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY12812 / R12812C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY12813 / R12813C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY13567 / R13567C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY13568 / R13568C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY15162 / R15162C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY15163 / R15163C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK15229 / R15229C37: got 'Bilag 1'
#> Warning: Expecting logical in BY15851 / R15851C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15852 / R15852C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15853 / R15853C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15854 / R15854C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15855 / R15855C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15856 / R15856C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15857 / R15857C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15858 / R15858C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15859 / R15859C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15860 / R15860C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15861 / R15861C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15862 / R15862C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15863 / R15863C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15864 / R15864C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15865 / R15865C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15866 / R15866C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15867 / R15867C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15868 / R15868C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15869 / R15869C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15870 / R15870C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15871 / R15871C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15872 / R15872C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15873 / R15873C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15874 / R15874C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15875 / R15875C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15876 / R15876C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15877 / R15877C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15878 / R15878C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15879 / R15879C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15880 / R15880C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15881 / R15881C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15882 / R15882C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15883 / R15883C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15884 / R15884C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15885 / R15885C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15886 / R15886C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15887 / R15887C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15888 / R15888C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15889 / R15889C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15890 / R15890C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15891 / R15891C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15892 / R15892C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15893 / R15893C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY15894 / R15894C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in AK15975 / R15975C37: got 'Bilag 1'
#> Warning: Expecting logical in AK16192 / R16192C37: got 'Bilag 1'
#> Warning: Expecting logical in AK16195 / R16195C37: got 'Bilag 1'
#> Warning: Expecting logical in AK16197 / R16197C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY16833 / R16833C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16834 / R16834C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16835 / R16835C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16836 / R16836C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16837 / R16837C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16838 / R16838C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16839 / R16839C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY16840 / R16840C77: got 'Dasyheleinae'
#> Warning: Expecting logical in BY18065 / R18065C77: got 'Ichneumoninae'
#> Warning: Expecting logical in BY18066 / R18066C77: got 'Ichneumoninae'
#> Warning: Expecting logical in AK19156 / R19156C37: got 'Bilag 1'
#> Warning: Expecting logical in CA20252 / R20252C79: got 'Ciini'
#> Warning: Expecting logical in CA20253 / R20253C79: got 'Ciini'
#> Warning: Expecting logical in K21355 / R21355C11: got 'Rødkælk'
#> Warning: Expecting logical in AK22945 / R22945C37: got 'Bilag 1'
#> Warning: Expecting logical in BY23337 / R23337C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23338 / R23338C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23339 / R23339C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23340 / R23340C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23341 / R23341C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23342 / R23342C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23343 / R23343C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23344 / R23344C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23345 / R23345C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23346 / R23346C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23347 / R23347C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23348 / R23348C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23349 / R23349C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23350 / R23350C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23351 / R23351C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23352 / R23352C77: got 'Forcipomyiinae'
#> Warning: Expecting logical in BY23364 / R23364C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23365 / R23365C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23366 / R23366C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23367 / R23367C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23368 / R23368C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23369 / R23369C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23370 / R23370C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23371 / R23371C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23372 / R23372C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23373 / R23373C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23374 / R23374C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23375 / R23375C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23376 / R23376C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23377 / R23377C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23378 / R23378C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23379 / R23379C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23380 / R23380C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23381 / R23381C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23382 / R23382C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23383 / R23383C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY23386 / R23386C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY23387 / R23387C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK23554 / R23554C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK23973 / R23973C37: got 'Bilag 1'
#> Warning: Expecting logical in AK23977 / R23977C37: got 'Bilag 1'
#> Warning: Expecting logical in AK24081 / R24081C37: got 'Bilag 1'
#> Warning: Expecting logical in BY24601 / R24601C77: got 'Ectatomminae'
#> Warning: Expecting logical in BY24602 / R24602C77: got 'Ectatomminae'
#> Warning: Expecting logical in AK25052 / R25052C37: got 'Bilag 1'
#> Warning: Expecting logical in AK25446 / R25446C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK25514 / R25514C37: got 'Bilag 1'
#> Warning: Expecting logical in BY25813 / R25813C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY25814 / R25814C77: got 'Toknopmyrer'
#> Warning: Expecting logical in CD26818 / R26818C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26818 / R26818C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26819 / R26819C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26819 / R26819C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26820 / R26820C82: got 'Hieracium sect.
#> Alpestria'
#> Warning: Expecting logical in CE26820 / R26820C83: got 'Bjerg-høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26821 / R26821C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26821 / R26821C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26822 / R26822C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26822 / R26822C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26823 / R26823C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26823 / R26823C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26824 / R26824C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26824 / R26824C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26825 / R26825C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26825 / R26825C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26826 / R26826C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26826 / R26826C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26828 / R26828C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26828 / R26828C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26829 / R26829C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26829 / R26829C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26830 / R26830C82: got 'Hieracium sect.
#> Prenanthoidea'
#> Warning: Expecting logical in CE26830 / R26830C83: got 'Hjertebladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26831 / R26831C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26831 / R26831C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26832 / R26832C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26832 / R26832C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26833 / R26833C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26833 / R26833C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26834 / R26834C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26834 / R26834C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26835 / R26835C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26835 / R26835C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26836 / R26836C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26836 / R26836C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26837 / R26837C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26837 / R26837C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26838 / R26838C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26838 / R26838C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26839 / R26839C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26839 / R26839C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26840 / R26840C82: got 'Hieracium sect.
#> Alpestria'
#> Warning: Expecting logical in CE26840 / R26840C83: got 'Bjerg-høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26841 / R26841C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26841 / R26841C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26842 / R26842C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26842 / R26842C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26843 / R26843C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26843 / R26843C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26844 / R26844C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26844 / R26844C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26845 / R26845C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26845 / R26845C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26846 / R26846C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26846 / R26846C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26847 / R26847C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26847 / R26847C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26848 / R26848C82: got 'Hieracium sect.
#> Alpestria'
#> Warning: Expecting logical in CE26848 / R26848C83: got 'Bjerg-høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26849 / R26849C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26849 / R26849C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26850 / R26850C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26850 / R26850C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26851 / R26851C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26851 / R26851C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26852 / R26852C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26852 / R26852C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26853 / R26853C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26853 / R26853C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26854 / R26854C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26854 / R26854C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26855 / R26855C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26855 / R26855C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26856 / R26856C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26856 / R26856C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26857 / R26857C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26857 / R26857C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26858 / R26858C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26858 / R26858C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26859 / R26859C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26859 / R26859C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26860 / R26860C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26860 / R26860C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26861 / R26861C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26861 / R26861C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26862 / R26862C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26862 / R26862C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26863 / R26863C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26863 / R26863C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26864 / R26864C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26864 / R26864C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26865 / R26865C82: got 'Hieracium sect.
#> Sabauda'
#> Warning: Expecting logical in CE26865 / R26865C83: got 'Bredbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26866 / R26866C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26866 / R26866C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26867 / R26867C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26867 / R26867C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26868 / R26868C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26868 / R26868C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26869 / R26869C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26869 / R26869C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26870 / R26870C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26870 / R26870C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26871 / R26871C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26871 / R26871C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26872 / R26872C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26872 / R26872C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26874 / R26874C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26874 / R26874C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26875 / R26875C82: got 'Hieracium sect.
#> Sabauda'
#> Warning: Expecting logical in CE26875 / R26875C83: got 'Bredbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26876 / R26876C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26876 / R26876C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26877 / R26877C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26877 / R26877C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26878 / R26878C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26878 / R26878C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26879 / R26879C82: got 'Hieracium sect.
#> Prenanthoidea'
#> Warning: Expecting logical in CE26879 / R26879C83: got 'Hjertebladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26880 / R26880C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26880 / R26880C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26881 / R26881C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26881 / R26881C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26882 / R26882C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26882 / R26882C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26883 / R26883C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26883 / R26883C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26884 / R26884C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26884 / R26884C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26885 / R26885C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26885 / R26885C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26886 / R26886C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26886 / R26886C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26887 / R26887C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26887 / R26887C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26888 / R26888C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26888 / R26888C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26889 / R26889C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26889 / R26889C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26890 / R26890C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26890 / R26890C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26891 / R26891C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26891 / R26891C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26899 / R26899C82: got 'Hieracium sect.
#> Alpestria'
#> Warning: Expecting logical in CE26899 / R26899C83: got 'Bjerg-høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26900 / R26900C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26900 / R26900C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26901 / R26901C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26901 / R26901C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26902 / R26902C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26902 / R26902C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26903 / R26903C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26903 / R26903C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26906 / R26906C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26906 / R26906C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26907 / R26907C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26907 / R26907C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26908 / R26908C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26908 / R26908C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26909 / R26909C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26909 / R26909C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26910 / R26910C82: got 'Hieracium sect.
#> Sabauda'
#> Warning: Expecting logical in CE26910 / R26910C83: got 'Bredbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26911 / R26911C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26911 / R26911C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26912 / R26912C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26912 / R26912C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26913 / R26913C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26913 / R26913C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26914 / R26914C82: got 'Hieracium sect.
#> Sabauda'
#> Warning: Expecting logical in CE26914 / R26914C83: got 'Bredbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26915 / R26915C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26915 / R26915C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26916 / R26916C82: got 'Hieracium sect.
#> Tridentata'
#> Warning: Expecting logical in CE26916 / R26916C83: got 'Rank høgeurt (sektion)'
#> Warning: Expecting logical in CD26917 / R26917C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26917 / R26917C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in CD26918 / R26918C82: got 'Hieracium sect.
#> Hieracioides'
#> Warning: Expecting logical in CE26918 / R26918C83: got 'Smalbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26919 / R26919C82: got 'Hieracium sect.
#> Hieracioides'
#> Warning: Expecting logical in CE26919 / R26919C83: got 'Smalbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26920 / R26920C82: got 'Hieracium sect.
#> Hieracioides'
#> Warning: Expecting logical in CE26920 / R26920C83: got 'Smalbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26921 / R26921C82: got 'Hieracium sect.
#> Hieracioides'
#> Warning: Expecting logical in CE26921 / R26921C83: got 'Smalbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26922 / R26922C82: got 'Hieracium sect.
#> Sabauda'
#> Warning: Expecting logical in CE26922 / R26922C83: got 'Bredbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26923 / R26923C82: got 'Hieracium sect.
#> Sabauda'
#> Warning: Expecting logical in CE26923 / R26923C83: got 'Bredbladet høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26924 / R26924C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26924 / R26924C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26925 / R26925C82: got 'Hieracium sect.
#> Vulgata'
#> Warning: Expecting logical in CE26925 / R26925C83: got 'Almindelig høgeurt
#> (sektion)'
#> Warning: Expecting logical in CD26926 / R26926C82: got 'Hieracium sect.
#> Hieracium'
#> Warning: Expecting logical in CE26926 / R26926C83: got 'Skov-høgeurt (sektion)'
#> Warning: Expecting logical in BY27470 / R27470C77: got 'Hybrizontinae'
#> Warning: Expecting logical in BY27471 / R27471C77: got 'Hybrizontinae'
#> Warning: Expecting logical in AK27627 / R27627C37: got 'Bilag 1'
#> Warning: Expecting logical in BY28371 / R28371C77: got 'Brodmyrer'
#> Warning: Expecting logical in BY28372 / R28372C77: got 'Brodmyrer'
#> Warning: Expecting logical in BY28373 / R28373C77: got 'Brodmyrer'
#> Warning: Expecting logical in AK28574 / R28574C37: got 'Bilag 1'
#> Warning: Expecting logical in K29831 / R29831C11: got 'Rødnæb'
#> Warning: Expecting logical in AK30262 / R30262C37: got 'Bilag 1'
#> Warning: Expecting logical in BY30474 / R30474C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30475 / R30475C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30476 / R30476C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30477 / R30477C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30478 / R30478C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30479 / R30479C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30480 / R30480C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30481 / R30481C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30482 / R30482C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30483 / R30483C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30484 / R30484C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30485 / R30485C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY30486 / R30486C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY31395 / R31395C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY31396 / R31396C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY31397 / R31397C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY31398 / R31398C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY31399 / R31399C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK32014 / R32014C37: got 'Bilag 1'
#> Warning: Expecting logical in BY32074 / R32074C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY32075 / R32075C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY32329 / R32329C77: got 'Ichneumoninae'
#> Warning: Expecting logical in BY32330 / R32330C77: got 'Ichneumoninae'
#> Warning: Expecting logical in AK32863 / R32863C37: got 'Bilag 1'
#> Warning: Expecting logical in AK32928 / R32928C37: got 'Bilag 1'
#> Warning: Expecting logical in AK33162 / R33162C37: got 'Bilag 1'
#> Warning: Expecting logical in BY33372 / R33372C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY33373 / R33373C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY33607 / R33607C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY33608 / R33608C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY33609 / R33609C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY33610 / R33610C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY33611 / R33611C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in AK33778 / R33778C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK34517 / R34517C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK34518 / R34518C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK34878 / R34878C37: got 'Bilag 1'
#> Warning: Expecting logical in AK34880 / R34880C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK34881 / R34881C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK35870 / R35870C37: got 'Bilag 1'
#> Warning: Expecting logical in BY36328 / R36328C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY36329 / R36329C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37204 / R37204C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37205 / R37205C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37225 / R37225C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37226 / R37226C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37227 / R37227C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37228 / R37228C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37229 / R37229C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37230 / R37230C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37231 / R37231C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37232 / R37232C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37233 / R37233C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37234 / R37234C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37235 / R37235C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37236 / R37236C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY37237 / R37237C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY38007 / R38007C77: got 'Brodmyrer'
#> Warning: Expecting logical in BY38008 / R38008C77: got 'Brodmyrer'
#> Warning: Expecting logical in AK38696 / R38696C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK38697 / R38697C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY38739 / R38739C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY38740 / R38740C77: got 'Skælmyrer'
#> Warning: Expecting logical in CA38951 / R38951C79: got 'Orophiini'
#> Warning: Expecting logical in CA38952 / R38952C79: got 'Orophiini'
#> Warning: Expecting logical in BY39096 / R39096C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY39097 / R39097C77: got 'Skælmyrer'
#> Warning: Expecting logical in CA39945 / R39945C79: got 'Orectochilini'
#> Warning: Expecting logical in CA39946 / R39946C79: got 'Orectochilini'
#> Warning: Expecting logical in BY40061 / R40061C77: got 'Ichneumoninae'
#> Warning: Expecting logical in BY40062 / R40062C77: got 'Ichneumoninae'
#> Warning: Expecting logical in CA40116 / R40116C79: got 'Ciini'
#> Warning: Expecting logical in CA40117 / R40117C79: got 'Ciini'
#> Warning: Expecting logical in CA40118 / R40118C79: got 'Ciini'
#> Warning: Expecting logical in BY40835 / R40835C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40836 / R40836C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40837 / R40837C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40838 / R40838C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40839 / R40839C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40840 / R40840C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40841 / R40841C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40842 / R40842C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40843 / R40843C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40844 / R40844C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40845 / R40845C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40846 / R40846C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY40847 / R40847C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in AK40957 / R40957C37: got 'Bilag 1'
#> Warning: Expecting logical in BY41479 / R41479C77: got 'Skælmyrer'
#> Warning: Expecting logical in BY41480 / R41480C77: got 'Skælmyrer'
#> Warning: Expecting logical in AK42408 / R42408C37: got 'Bilag 1'
#> Warning: Expecting logical in AK42904 / R42904C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY43075 / R43075C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY43076 / R43076C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY43077 / R43077C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY43078 / R43078C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY43079 / R43079C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY43080 / R43080C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY44321 / R44321C77: got 'Cryptinae'
#> Warning: Expecting logical in AK44849 / R44849C37: got 'Bilag 1'
#> Warning: Expecting logical in AK45507 / R45507C37: got 'Bilag 1'
#> Warning: Expecting logical in AK45511 / R45511C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK45578 / R45578C37: got 'Bilag 1'
#> Warning: Expecting logical in AK45581 / R45581C37: got 'Bilag 1'
#> Warning: Expecting logical in BY46133 / R46133C77: got 'Brodmyrer'
#> Warning: Expecting logical in BY46134 / R46134C77: got 'Brodmyrer'
#> Warning: Expecting logical in AK46335 / R46335C37: got 'Bilag 1'
#> Warning: Expecting logical in BY46650 / R46650C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY46651 / R46651C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in K48647 / R48647C11: got 'Frynse-eg'
#> Warning: Expecting logical in K48652 / R48652C11: got 'Stilk-eg'
#> Warning: Expecting logical in AK48949 / R48949C37: got 'Bilag 1'
#> Warning: Expecting logical in CA49893 / R49893C79: got 'Orophiini'
#> Warning: Expecting logical in CA49894 / R49894C79: got 'Orophiini'
#> Warning: Expecting logical in CA49895 / R49895C79: got 'Orophiini'
#> Warning: Expecting logical in CD50016 / R50016C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50016 / R50016C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50017 / R50017C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50017 / R50017C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50018 / R50018C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50018 / R50018C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50019 / R50019C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50019 / R50019C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50020 / R50020C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50020 / R50020C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50022 / R50022C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50022 / R50022C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50023 / R50023C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50023 / R50023C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50024 / R50024C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50024 / R50024C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50025 / R50025C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50025 / R50025C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50026 / R50026C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50026 / R50026C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50027 / R50027C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50027 / R50027C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50035 / R50035C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50035 / R50035C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50036 / R50036C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50036 / R50036C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50037 / R50037C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50037 / R50037C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50039 / R50039C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50039 / R50039C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50040 / R50040C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CD50041 / R50041C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50041 / R50041C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50042 / R50042C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50042 / R50042C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50043 / R50043C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50043 / R50043C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50044 / R50044C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50044 / R50044C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50045 / R50045C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50045 / R50045C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50046 / R50046C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50046 / R50046C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50047 / R50047C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50047 / R50047C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50048 / R50048C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50048 / R50048C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50049 / R50049C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50049 / R50049C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50050 / R50050C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50050 / R50050C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50051 / R50051C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50051 / R50051C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50052 / R50052C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50052 / R50052C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50053 / R50053C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50053 / R50053C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50054 / R50054C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50054 / R50054C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50055 / R50055C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50055 / R50055C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50056 / R50056C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50056 / R50056C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50057 / R50057C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50057 / R50057C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50058 / R50058C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50058 / R50058C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50059 / R50059C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50059 / R50059C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50060 / R50060C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50060 / R50060C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50061 / R50061C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50061 / R50061C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50063 / R50063C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50063 / R50063C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50064 / R50064C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50064 / R50064C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50065 / R50065C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50065 / R50065C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50066 / R50066C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50066 / R50066C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50067 / R50067C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50067 / R50067C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50068 / R50068C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50068 / R50068C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50069 / R50069C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50069 / R50069C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50070 / R50070C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50070 / R50070C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50071 / R50071C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50071 / R50071C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50072 / R50072C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50072 / R50072C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50073 / R50073C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50073 / R50073C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50074 / R50074C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50074 / R50074C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50075 / R50075C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50075 / R50075C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50079 / R50079C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50079 / R50079C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50080 / R50080C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50080 / R50080C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50081 / R50081C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50081 / R50081C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50082 / R50082C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50082 / R50082C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50083 / R50083C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50083 / R50083C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50084 / R50084C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50084 / R50084C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50085 / R50085C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50085 / R50085C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50086 / R50086C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50086 / R50086C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50087 / R50087C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50087 / R50087C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50088 / R50088C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50088 / R50088C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50089 / R50089C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50089 / R50089C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50090 / R50090C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50090 / R50090C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50091 / R50091C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50091 / R50091C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50092 / R50092C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50092 / R50092C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50093 / R50093C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50093 / R50093C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50094 / R50094C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50094 / R50094C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50095 / R50095C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50095 / R50095C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50096 / R50096C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50096 / R50096C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50097 / R50097C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50097 / R50097C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50098 / R50098C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50098 / R50098C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50099 / R50099C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50099 / R50099C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50100 / R50100C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50100 / R50100C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50101 / R50101C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50101 / R50101C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50102 / R50102C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50102 / R50102C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50103 / R50103C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50103 / R50103C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50104 / R50104C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50104 / R50104C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50105 / R50105C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50105 / R50105C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50106 / R50106C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50106 / R50106C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50107 / R50107C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50107 / R50107C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50109 / R50109C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50109 / R50109C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50110 / R50110C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50110 / R50110C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50112 / R50112C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50112 / R50112C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50113 / R50113C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50113 / R50113C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50115 / R50115C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50115 / R50115C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50116 / R50116C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50116 / R50116C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50117 / R50117C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50117 / R50117C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50118 / R50118C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50118 / R50118C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50120 / R50120C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50120 / R50120C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50121 / R50121C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50121 / R50121C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50122 / R50122C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50122 / R50122C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50123 / R50123C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50123 / R50123C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50124 / R50124C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50124 / R50124C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50125 / R50125C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50125 / R50125C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50126 / R50126C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50126 / R50126C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50127 / R50127C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50127 / R50127C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50128 / R50128C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50128 / R50128C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50129 / R50129C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50129 / R50129C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50130 / R50130C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50130 / R50130C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50131 / R50131C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50131 / R50131C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50132 / R50132C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50132 / R50132C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50133 / R50133C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50133 / R50133C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50135 / R50135C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50135 / R50135C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50136 / R50136C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50136 / R50136C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50137 / R50137C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50137 / R50137C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50140 / R50140C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50140 / R50140C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50141 / R50141C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50141 / R50141C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50142 / R50142C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50142 / R50142C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50143 / R50143C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50143 / R50143C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50144 / R50144C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50144 / R50144C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50146 / R50146C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50146 / R50146C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50147 / R50147C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50147 / R50147C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50148 / R50148C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50148 / R50148C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50149 / R50149C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50149 / R50149C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50150 / R50150C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50150 / R50150C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50151 / R50151C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50151 / R50151C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50152 / R50152C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50152 / R50152C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50153 / R50153C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50153 / R50153C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50154 / R50154C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50154 / R50154C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50155 / R50155C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50155 / R50155C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50156 / R50156C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50156 / R50156C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50157 / R50157C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50157 / R50157C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50158 / R50158C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50158 / R50158C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50159 / R50159C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50159 / R50159C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50160 / R50160C82: got 'Rubus sect. Rubus'
#> Warning: Expecting logical in CE50160 / R50160C83: got 'Brombær (sektion)'
#> Warning: Expecting logical in CD50161 / R50161C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50161 / R50161C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50162 / R50162C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50162 / R50162C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in CD50163 / R50163C82: got 'Rubus sect.
#> Corylifolii'
#> Warning: Expecting logical in CE50163 / R50163C83: got 'Hasselbrombær
#> (sektion)'
#> Warning: Expecting logical in BY51206 / R51206C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY51207 / R51207C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in K51770 / R51770C11: got 'Spytteedderkop'
#> Warning: Expecting logical in BY52044 / R52044C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY52045 / R52045C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY52046 / R52046C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY52047 / R52047C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY52048 / R52048C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY52049 / R52049C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY52659 / R52659C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY52660 / R52660C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY52661 / R52661C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY52662 / R52662C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY52663 / R52663C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK52683 / R52683C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK52851 / R52851C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in CA52949 / R52949C79: got 'Spaeridiini'
#> Warning: Expecting logical in CA52950 / R52950C79: got 'Spaeridiini'
#> Warning: Expecting logical in CA52951 / R52951C79: got 'Spaeridiini'
#> Warning: Expecting logical in CA52953 / R52953C79: got 'Spaeridiini'
#> Warning: Expecting logical in CA52954 / R52954C79: got 'Spaeridiini'
#> Warning: Expecting logical in CA52955 / R52955C79: got 'Spaeridiini'
#> Warning: Expecting logical in CA52956 / R52956C79: got 'Spaeridiini'
#> Warning: Expecting logical in BY53004 / R53004C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY53005 / R53005C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in BY53006 / R53006C77: got 'Ceratopogoninae'
#> Warning: Expecting logical in CD53077 / R53077C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53077 / R53077C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53078 / R53078C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53078 / R53078C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53079 / R53079C82: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CE53079 / R53079C83: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CD53080 / R53080C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53080 / R53080C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53081 / R53081C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53081 / R53081C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53082 / R53082C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53082 / R53082C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53083 / R53083C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53083 / R53083C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53084 / R53084C82: got 'Sphagnum sect. Rigida'
#> Warning: Expecting logical in CE53084 / R53084C83: got 'Sphagnum sect. Rigida'
#> Warning: Expecting logical in CD53085 / R53085C82: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CE53085 / R53085C83: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CD53086 / R53086C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53086 / R53086C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53087 / R53087C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53087 / R53087C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53088 / R53088C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53088 / R53088C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53089 / R53089C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53089 / R53089C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53090 / R53090C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53090 / R53090C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53091 / R53091C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53091 / R53091C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53092 / R53092C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53092 / R53092C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53093 / R53093C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53093 / R53093C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53094 / R53094C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53094 / R53094C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53095 / R53095C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53095 / R53095C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53096 / R53096C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53096 / R53096C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53097 / R53097C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53097 / R53097C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53098 / R53098C82: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CE53098 / R53098C83: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CD53099 / R53099C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53099 / R53099C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53100 / R53100C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53100 / R53100C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53101 / R53101C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53101 / R53101C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53102 / R53102C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53102 / R53102C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53103 / R53103C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53103 / R53103C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53104 / R53104C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53104 / R53104C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53105 / R53105C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53105 / R53105C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53106 / R53106C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53106 / R53106C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53107 / R53107C82: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CE53107 / R53107C83: got 'Sphagnum sect.
#> Sphagnum'
#> Warning: Expecting logical in CD53108 / R53108C82: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CE53108 / R53108C83: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CD53109 / R53109C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53109 / R53109C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53110 / R53110C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53110 / R53110C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53111 / R53111C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53111 / R53111C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53112 / R53112C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53112 / R53112C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53113 / R53113C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53113 / R53113C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53120 / R53120C82: got 'Sphagnum sect.
#> Squarrosa'
#> Warning: Expecting logical in CE53120 / R53120C83: got 'Sphagnum sect.
#> Squarrosa'
#> Warning: Expecting logical in CD53121 / R53121C82: got 'Sphagnum sect. Rigida'
#> Warning: Expecting logical in CE53121 / R53121C83: got 'Sphagnum sect. Rigida'
#> Warning: Expecting logical in CD53122 / R53122C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53122 / R53122C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CD53123 / R53123C82: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CE53123 / R53123C83: got 'Sphagnum sect.
#> Subsecunda'
#> Warning: Expecting logical in CD53124 / R53124C82: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CE53124 / R53124C83: got 'Sphagnum sect.
#> Cuspidata'
#> Warning: Expecting logical in CD53125 / R53125C82: got 'Sphagnum sect.
#> Squarrosa'
#> Warning: Expecting logical in CE53125 / R53125C83: got 'Sphagnum sect.
#> Squarrosa'
#> Warning: Expecting logical in CD53126 / R53126C82: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in CE53126 / R53126C83: got 'Sphagnum sect.
#> Acutifolia'
#> Warning: Expecting logical in BY53151 / R53151C77: got 'Cryptinae'
#> Warning: Expecting logical in BY53152 / R53152C77: got 'Cryptinae'
#> Warning: Expecting logical in BY53739 / R53739C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY53740 / R53740C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY53741 / R53741C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK54032 / R54032C37: got 'Bilag 1'
#> Warning: Expecting logical in AK54033 / R54033C37: got 'Bilag 1'
#> Warning: Expecting logical in AK54038 / R54038C37: got 'Bilag 1'
#> Warning: Expecting logical in BY54424 / R54424C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY54425 / R54425C77: got 'Toknopmyrer'
#> Warning: Expecting logical in CA54597 / R54597C79: got 'Ciini'
#> Warning: Expecting logical in CA54598 / R54598C79: got 'Ciini'
#> Warning: Expecting logical in CA54599 / R54599C79: got 'Ciini'
#> Warning: Expecting logical in AK55209 / R55209C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in BY55400 / R55400C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY55401 / R55401C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY55402 / R55402C77: got 'Duftmyrer'
#> Warning: Expecting logical in CD55412 / R55412C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55412 / R55412C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55413 / R55413C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55413 / R55413C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55414 / R55414C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55414 / R55414C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55415 / R55415C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55415 / R55415C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55416 / R55416C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55416 / R55416C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55417 / R55417C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55417 / R55417C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55419 / R55419C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55419 / R55419C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55420 / R55420C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55420 / R55420C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55421 / R55421C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55421 / R55421C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55422 / R55422C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55422 / R55422C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55424 / R55424C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55424 / R55424C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55425 / R55425C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55425 / R55425C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55426 / R55426C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55426 / R55426C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55427 / R55427C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55427 / R55427C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55428 / R55428C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55428 / R55428C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55429 / R55429C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55429 / R55429C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55430 / R55430C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55430 / R55430C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55431 / R55431C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55431 / R55431C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55432 / R55432C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55432 / R55432C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55433 / R55433C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55433 / R55433C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55434 / R55434C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55434 / R55434C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55435 / R55435C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55435 / R55435C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55436 / R55436C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55436 / R55436C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55437 / R55437C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55437 / R55437C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55438 / R55438C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55438 / R55438C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55439 / R55439C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55439 / R55439C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55440 / R55440C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55440 / R55440C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55441 / R55441C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55441 / R55441C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55442 / R55442C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55442 / R55442C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55443 / R55443C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55443 / R55443C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55444 / R55444C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55444 / R55444C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55445 / R55445C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55445 / R55445C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55446 / R55446C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55446 / R55446C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55447 / R55447C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55447 / R55447C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55448 / R55448C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55448 / R55448C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55450 / R55450C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55450 / R55450C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55451 / R55451C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55451 / R55451C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55452 / R55452C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55452 / R55452C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55453 / R55453C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55453 / R55453C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55454 / R55454C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55454 / R55454C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55455 / R55455C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55455 / R55455C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55456 / R55456C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55456 / R55456C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55457 / R55457C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55457 / R55457C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55458 / R55458C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55458 / R55458C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55459 / R55459C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55459 / R55459C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55460 / R55460C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55460 / R55460C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55461 / R55461C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55461 / R55461C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55462 / R55462C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55462 / R55462C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55463 / R55463C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55463 / R55463C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55464 / R55464C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55464 / R55464C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55465 / R55465C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55465 / R55465C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55467 / R55467C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55467 / R55467C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55468 / R55468C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55468 / R55468C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55469 / R55469C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55469 / R55469C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55470 / R55470C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55470 / R55470C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55471 / R55471C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55471 / R55471C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55473 / R55473C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55473 / R55473C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55474 / R55474C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55474 / R55474C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55475 / R55475C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55475 / R55475C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55476 / R55476C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55476 / R55476C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55477 / R55477C82: got 'Taraxacum sect.
#> Spectabilia'
#> Warning: Expecting logical in CE55477 / R55477C83: got 'Atlantmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55479 / R55479C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55479 / R55479C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55480 / R55480C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55480 / R55480C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55481 / R55481C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55481 / R55481C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55482 / R55482C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55482 / R55482C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55483 / R55483C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55483 / R55483C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55484 / R55484C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55484 / R55484C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55485 / R55485C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55485 / R55485C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55486 / R55486C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55486 / R55486C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55487 / R55487C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55487 / R55487C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55488 / R55488C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55488 / R55488C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55489 / R55489C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55489 / R55489C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55490 / R55490C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55490 / R55490C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55491 / R55491C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55491 / R55491C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55492 / R55492C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55492 / R55492C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55493 / R55493C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55493 / R55493C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55494 / R55494C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55494 / R55494C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55495 / R55495C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55495 / R55495C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55496 / R55496C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55496 / R55496C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55497 / R55497C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55497 / R55497C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55498 / R55498C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55498 / R55498C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55499 / R55499C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55499 / R55499C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55500 / R55500C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55500 / R55500C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55501 / R55501C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55501 / R55501C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55503 / R55503C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55503 / R55503C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55504 / R55504C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55504 / R55504C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55505 / R55505C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55505 / R55505C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55506 / R55506C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55506 / R55506C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55507 / R55507C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55507 / R55507C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55508 / R55508C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55508 / R55508C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55510 / R55510C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55510 / R55510C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55511 / R55511C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55511 / R55511C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55512 / R55512C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55512 / R55512C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55513 / R55513C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55513 / R55513C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55515 / R55515C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55515 / R55515C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55516 / R55516C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55516 / R55516C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55517 / R55517C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55517 / R55517C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55518 / R55518C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55518 / R55518C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55519 / R55519C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55519 / R55519C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55521 / R55521C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55521 / R55521C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55522 / R55522C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55522 / R55522C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55524 / R55524C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55524 / R55524C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55525 / R55525C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55525 / R55525C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55526 / R55526C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55526 / R55526C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55527 / R55527C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55527 / R55527C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55528 / R55528C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55528 / R55528C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55529 / R55529C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55529 / R55529C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55530 / R55530C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55530 / R55530C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55531 / R55531C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55531 / R55531C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55533 / R55533C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55533 / R55533C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55534 / R55534C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55534 / R55534C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55535 / R55535C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55535 / R55535C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55536 / R55536C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55536 / R55536C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55537 / R55537C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55537 / R55537C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55538 / R55538C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55538 / R55538C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55539 / R55539C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55539 / R55539C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55540 / R55540C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55540 / R55540C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55541 / R55541C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55541 / R55541C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55542 / R55542C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55542 / R55542C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55543 / R55543C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55543 / R55543C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55544 / R55544C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55544 / R55544C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55545 / R55545C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55545 / R55545C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55546 / R55546C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55546 / R55546C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55548 / R55548C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55548 / R55548C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55549 / R55549C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55549 / R55549C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55550 / R55550C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55550 / R55550C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55551 / R55551C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55551 / R55551C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55552 / R55552C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55552 / R55552C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55553 / R55553C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55553 / R55553C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55555 / R55555C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55555 / R55555C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55556 / R55556C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55556 / R55556C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55557 / R55557C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55557 / R55557C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55558 / R55558C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55558 / R55558C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55559 / R55559C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55559 / R55559C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55560 / R55560C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55560 / R55560C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55562 / R55562C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55562 / R55562C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55563 / R55563C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55563 / R55563C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55564 / R55564C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55564 / R55564C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55565 / R55565C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55565 / R55565C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55566 / R55566C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55566 / R55566C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55567 / R55567C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55567 / R55567C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55568 / R55568C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55568 / R55568C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55569 / R55569C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55569 / R55569C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55570 / R55570C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55570 / R55570C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55571 / R55571C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55571 / R55571C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55572 / R55572C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55572 / R55572C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55573 / R55573C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55573 / R55573C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55574 / R55574C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55574 / R55574C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55575 / R55575C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55575 / R55575C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55576 / R55576C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55576 / R55576C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55577 / R55577C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55577 / R55577C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55578 / R55578C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55578 / R55578C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55579 / R55579C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55579 / R55579C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55580 / R55580C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55580 / R55580C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55581 / R55581C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55581 / R55581C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55582 / R55582C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55582 / R55582C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55583 / R55583C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55583 / R55583C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55584 / R55584C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55584 / R55584C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55585 / R55585C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55585 / R55585C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55586 / R55586C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55586 / R55586C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55587 / R55587C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55587 / R55587C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55588 / R55588C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55588 / R55588C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55589 / R55589C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55589 / R55589C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55591 / R55591C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55591 / R55591C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55592 / R55592C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55592 / R55592C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55593 / R55593C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55593 / R55593C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55594 / R55594C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55594 / R55594C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55595 / R55595C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55595 / R55595C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55596 / R55596C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55596 / R55596C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55597 / R55597C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55597 / R55597C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55599 / R55599C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55599 / R55599C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55600 / R55600C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55600 / R55600C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55601 / R55601C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55601 / R55601C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55602 / R55602C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55602 / R55602C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55603 / R55603C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55603 / R55603C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55604 / R55604C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55604 / R55604C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55605 / R55605C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55605 / R55605C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55606 / R55606C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55606 / R55606C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55607 / R55607C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55607 / R55607C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55608 / R55608C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55608 / R55608C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55609 / R55609C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55609 / R55609C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55610 / R55610C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55610 / R55610C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55611 / R55611C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55611 / R55611C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55612 / R55612C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55612 / R55612C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55613 / R55613C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55613 / R55613C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55614 / R55614C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55614 / R55614C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55615 / R55615C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55615 / R55615C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55616 / R55616C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55616 / R55616C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55617 / R55617C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55617 / R55617C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55618 / R55618C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55618 / R55618C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55619 / R55619C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55619 / R55619C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55620 / R55620C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55620 / R55620C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55621 / R55621C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55621 / R55621C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55623 / R55623C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55623 / R55623C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55624 / R55624C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55624 / R55624C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55625 / R55625C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55625 / R55625C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55626 / R55626C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55626 / R55626C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55627 / R55627C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55627 / R55627C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55628 / R55628C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55628 / R55628C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55629 / R55629C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55629 / R55629C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55630 / R55630C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55630 / R55630C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55631 / R55631C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55631 / R55631C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55632 / R55632C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55632 / R55632C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55633 / R55633C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55633 / R55633C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55634 / R55634C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55634 / R55634C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55635 / R55635C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55635 / R55635C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55636 / R55636C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55636 / R55636C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55637 / R55637C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55637 / R55637C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55638 / R55638C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55638 / R55638C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55639 / R55639C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55639 / R55639C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55640 / R55640C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55640 / R55640C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55641 / R55641C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55641 / R55641C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55642 / R55642C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55642 / R55642C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55643 / R55643C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55643 / R55643C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55644 / R55644C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55644 / R55644C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55645 / R55645C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55645 / R55645C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55646 / R55646C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55646 / R55646C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55647 / R55647C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55647 / R55647C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55648 / R55648C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55648 / R55648C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55650 / R55650C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55650 / R55650C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55651 / R55651C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55651 / R55651C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55653 / R55653C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55653 / R55653C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55654 / R55654C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55654 / R55654C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55655 / R55655C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55655 / R55655C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55656 / R55656C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55656 / R55656C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55657 / R55657C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55657 / R55657C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55658 / R55658C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55658 / R55658C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55659 / R55659C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55659 / R55659C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55660 / R55660C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55660 / R55660C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55661 / R55661C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55661 / R55661C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55662 / R55662C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55662 / R55662C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55663 / R55663C82: got 'Taraxacum sect.
#> Celtica'
#> Warning: Expecting logical in CE55663 / R55663C83: got 'Kærmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55664 / R55664C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55664 / R55664C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55665 / R55665C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55665 / R55665C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55666 / R55666C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55666 / R55666C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55668 / R55668C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55668 / R55668C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55669 / R55669C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55669 / R55669C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55670 / R55670C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55670 / R55670C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55671 / R55671C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55671 / R55671C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55672 / R55672C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55672 / R55672C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55674 / R55674C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55674 / R55674C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55675 / R55675C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55675 / R55675C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55676 / R55676C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55676 / R55676C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55677 / R55677C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55677 / R55677C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55678 / R55678C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55678 / R55678C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55679 / R55679C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55679 / R55679C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55680 / R55680C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55680 / R55680C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55681 / R55681C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55681 / R55681C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55682 / R55682C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55682 / R55682C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55683 / R55683C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55683 / R55683C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55684 / R55684C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55684 / R55684C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55685 / R55685C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55685 / R55685C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55686 / R55686C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55686 / R55686C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55687 / R55687C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55687 / R55687C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55688 / R55688C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55688 / R55688C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55690 / R55690C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55690 / R55690C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55691 / R55691C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55691 / R55691C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55692 / R55692C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55692 / R55692C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55693 / R55693C82: got 'Taraxacum sect.
#> Spectabilia'
#> Warning: Expecting logical in CE55693 / R55693C83: got 'Atlantmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55694 / R55694C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55694 / R55694C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55695 / R55695C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55695 / R55695C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55696 / R55696C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55696 / R55696C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55697 / R55697C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55697 / R55697C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55698 / R55698C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55698 / R55698C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55699 / R55699C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55699 / R55699C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55700 / R55700C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55700 / R55700C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55701 / R55701C82: got 'Taraxacum sect.
#> Celtica'
#> Warning: Expecting logical in CE55701 / R55701C83: got 'Kærmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55702 / R55702C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55702 / R55702C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55703 / R55703C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55703 / R55703C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55704 / R55704C82: got 'Taraxacum sect.
#> Obliqua'
#> Warning: Expecting logical in CE55704 / R55704C83: got 'Dværgmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55705 / R55705C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55705 / R55705C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55706 / R55706C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55706 / R55706C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55707 / R55707C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55707 / R55707C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55708 / R55708C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55708 / R55708C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55709 / R55709C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55709 / R55709C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55710 / R55710C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55710 / R55710C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55711 / R55711C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55711 / R55711C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55712 / R55712C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55712 / R55712C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55713 / R55713C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55713 / R55713C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55714 / R55714C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55714 / R55714C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55715 / R55715C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55715 / R55715C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55716 / R55716C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55716 / R55716C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55717 / R55717C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55717 / R55717C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55718 / R55718C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55718 / R55718C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55719 / R55719C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55719 / R55719C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55720 / R55720C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55720 / R55720C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55721 / R55721C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55721 / R55721C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55722 / R55722C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55722 / R55722C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55723 / R55723C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55723 / R55723C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55724 / R55724C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55724 / R55724C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55725 / R55725C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55725 / R55725C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55726 / R55726C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55726 / R55726C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55727 / R55727C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55727 / R55727C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55728 / R55728C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55728 / R55728C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55729 / R55729C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55729 / R55729C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55730 / R55730C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55730 / R55730C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55731 / R55731C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55731 / R55731C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55732 / R55732C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55732 / R55732C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55733 / R55733C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55733 / R55733C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55734 / R55734C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55734 / R55734C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55735 / R55735C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55735 / R55735C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55736 / R55736C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55736 / R55736C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55737 / R55737C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55737 / R55737C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55738 / R55738C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55738 / R55738C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55739 / R55739C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55739 / R55739C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55741 / R55741C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55741 / R55741C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55743 / R55743C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55743 / R55743C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55744 / R55744C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55744 / R55744C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55745 / R55745C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55745 / R55745C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55746 / R55746C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55746 / R55746C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55747 / R55747C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55747 / R55747C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55748 / R55748C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55748 / R55748C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55749 / R55749C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55749 / R55749C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55750 / R55750C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55750 / R55750C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55761 / R55761C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55761 / R55761C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55762 / R55762C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55762 / R55762C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55763 / R55763C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55763 / R55763C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55764 / R55764C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55764 / R55764C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55765 / R55765C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55765 / R55765C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55766 / R55766C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55766 / R55766C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55767 / R55767C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55767 / R55767C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55768 / R55768C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55768 / R55768C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55769 / R55769C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55769 / R55769C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55770 / R55770C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55770 / R55770C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55771 / R55771C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55771 / R55771C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55773 / R55773C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55773 / R55773C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55774 / R55774C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55774 / R55774C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55775 / R55775C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55775 / R55775C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55776 / R55776C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55776 / R55776C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55778 / R55778C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55778 / R55778C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55779 / R55779C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55779 / R55779C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55780 / R55780C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55780 / R55780C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55781 / R55781C82: got 'Taraxacum sect.
#> Palustria'
#> Warning: Expecting logical in CE55781 / R55781C83: got 'Kalkmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55782 / R55782C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55782 / R55782C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55784 / R55784C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55784 / R55784C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55785 / R55785C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55785 / R55785C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55786 / R55786C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55786 / R55786C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55787 / R55787C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55787 / R55787C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55788 / R55788C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55788 / R55788C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55789 / R55789C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55789 / R55789C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55791 / R55791C82: got 'Taraxacum sect. Hamata'
#> Warning: Expecting logical in CE55791 / R55791C83: got 'Krogmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55792 / R55792C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55792 / R55792C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55793 / R55793C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55793 / R55793C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55794 / R55794C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55794 / R55794C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55795 / R55795C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55795 / R55795C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55796 / R55796C82: got 'Taraxacum sect.
#> Naevosa'
#> Warning: Expecting logical in CE55796 / R55796C83: got 'Pletmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55797 / R55797C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55797 / R55797C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55798 / R55798C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55798 / R55798C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55799 / R55799C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55799 / R55799C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55800 / R55800C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55800 / R55800C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55802 / R55802C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55802 / R55802C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55803 / R55803C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55803 / R55803C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55805 / R55805C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55805 / R55805C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55806 / R55806C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55806 / R55806C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55807 / R55807C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55807 / R55807C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55808 / R55808C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55808 / R55808C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55809 / R55809C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55809 / R55809C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55810 / R55810C82: got 'Taraxacum sect.
#> Macrodonta'
#> Warning: Expecting logical in CE55810 / R55810C83: got 'Islandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55812 / R55812C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55812 / R55812C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55813 / R55813C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55813 / R55813C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55814 / R55814C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55814 / R55814C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55815 / R55815C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55815 / R55815C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55816 / R55816C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55816 / R55816C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55817 / R55817C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55817 / R55817C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55819 / R55819C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55819 / R55819C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55820 / R55820C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55820 / R55820C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55821 / R55821C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55821 / R55821C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55822 / R55822C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55822 / R55822C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55823 / R55823C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55823 / R55823C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55824 / R55824C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55824 / R55824C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55825 / R55825C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55825 / R55825C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55826 / R55826C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55826 / R55826C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55827 / R55827C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55827 / R55827C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55828 / R55828C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55828 / R55828C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55829 / R55829C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55829 / R55829C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55830 / R55830C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55830 / R55830C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55831 / R55831C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55831 / R55831C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55832 / R55832C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55832 / R55832C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55834 / R55834C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55834 / R55834C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55835 / R55835C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55835 / R55835C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55836 / R55836C82: got 'Taraxacum sect. Borea'
#> Warning: Expecting logical in CE55836 / R55836C83: got 'Nordmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55837 / R55837C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55837 / R55837C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55838 / R55838C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55838 / R55838C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55839 / R55839C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55839 / R55839C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55840 / R55840C82: got 'Taraxacum sect.
#> Erythrosperma'
#> Warning: Expecting logical in CE55840 / R55840C83: got 'Sandmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55841 / R55841C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55841 / R55841C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55842 / R55842C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55842 / R55842C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in CD55843 / R55843C82: got 'Taraxacum sect.
#> Taraxacum'
#> Warning: Expecting logical in CE55843 / R55843C83: got 'Vejmælkebøtte
#> (sektion)'
#> Warning: Expecting logical in BY55905 / R55905C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY55906 / R55906C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY55907 / R55907C77: got 'Duftmyrer'
#> Warning: Expecting logical in BY56029 / R56029C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56030 / R56030C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56031 / R56031C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56032 / R56032C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56033 / R56033C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56315 / R56315C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56316 / R56316C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56317 / R56317C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56318 / R56318C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56319 / R56319C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56320 / R56320C77: got 'Toknopmyrer'
#> Warning: Expecting logical in BY56321 / R56321C77: got 'Toknopmyrer'
#> Warning: Expecting logical in AK56424 / R56424C37: got 'Bilag 1'
#> Warning: Expecting logical in AK58328 / R58328C37: got 'Bilag 1'
#> Warning: Expecting logical in AK58330 / R58330C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in AK58333 / R58333C37: got 'Artikel 4 stk. 2'
#> Warning: Expecting logical in K59063 / R59063C11: got 'Storbladet elm'
#> Warning: Expecting logical in K59067 / R59067C11: got 'Park-elm'
#> Warning: Expecting logical in AK59222 / R59222C37: got 'Artikel 4 stk. 2'
```

We start with 4,899 taxa, we the use the SDMWorkflows package in order
to clean the taxa

``` r
Clean_Taxa <- SDMWorkflows::Clean_Taxa(ToClean$videnskabeligt_navn)
```

After this we reduce our number to 4,379

## 2.1 Working only with plants

After this cleaning we will only work with plants, for that we filter
for that:

``` r
Clean_Plants <- Clean_Taxa |>
    dplyr::filter(kingdom == "Plantae") |>
    dplyr::pull(species) |>
    unique()
```

With this we finally end up with 4,189 species.

# 3 Presence extraction

After this we extract all the presences we can retrieve from GBIF, only
for Denmark, between 1999 and 2023

``` r


SDMWorkflows::GetOccs(Species = unique(species),
                        WriteFile = FALSE,
                        Log = FALSE,
                        country = "DK",
                        limit = 100000,
                        year='1999,2023')
```
