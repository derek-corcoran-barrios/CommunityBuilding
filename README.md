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
library(ggplot2)
library(terra)
library(tidyterra)
library(SDMWorkflows)
library(BDRUtils)
library(data.table)
library(readxl)
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
SDMWorkflows::GetOccs(Species = unique(Clean_Species$species), 
                      WriteFile = TRUE, 
                      Log = TRUE,
                      country = "DK",
                      limit = 100000,
                      year='1999,2023')
```
