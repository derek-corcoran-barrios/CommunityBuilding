library(data.table)
library(dplyr)
library(terra)

result <- result3[, .(richness = sum(value)), by = .(cell, Habitat)]

Test <- terra::rast("O:/Nat_Sustain-tmp/DenmarkPlantPresences/Denmark/ForestDryPoor/abies_alba.tif")

values(Test) <- 0

ResultByHab <- result |> group_split(Habitat)

HabNames <- ResultByHab |> 
  purrr::map(~dplyr::pull(.x, Habitat)) |> 
  purrr::map(unique) |> 
  purrr::reduce(c)



Habs <- list()

for(i in 1:length(ResultByHab)){
  Template <- Test
  values(Template)[ResultByHab[[i]]$cell] <- ResultByHab[[i]]$richness
  Habs[[i]] <- Template
}

Habs <- Habs |> purrr::reduce(c) |> magrittr::set_names(HabNames)

DK <- geodata::gadm("Denmark", level = 0, path = getwd()) |> 
  terra::project(terra::crs(Habs))


Habs <- Habs |> terra::mask(DK)


plot(Habs, colNA = "black")


NewMask <- terra::rast("O:/Nat_Sustain-tmp/DenmarkPlantPresences/Vilhemsborg/ForestWetRich/chondrus_crispus.tif")

Habs_VilhelmsBorg <- Habs |>  terra::crop(NewMask)


Habs_VilhelmsBorg <- terra::project(Habs_VilhelmsBorg, "epsg:4326")

Habs_VilhelmsBorg[Habs_VilhelmsBorg == 0] <- NA

library(leaflet)
library(leafem)

pal <- colorNumeric(c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", 
                               "#d53e4f"), values(Habs_VilhelmsBorg),
                    na.color = "transparent")



l <- leaflet() |>  
  addProviderTiles("Esri.WorldImagery") 

for(i in 1:nlyr(Habs_VilhelmsBorg)){
  l <- l |> addRasterImage(Habs_VilhelmsBorg[[i]], colors = pal, opacity = 0.7, group = names(Habs_VilhelmsBorg[[i]]))
}
  
  
l |> 
  addLegend(pal = pal, values = values(Habs_VilhelmsBorg), title = "Species richness") |> 
  addLayersControl(
    baseGroups = names(Habs_VilhelmsBorg),
    options = layersControlOptions(collapsed = T)
  )

MaxHabitat <- as.data.frame(Habs_VilhelmsBorg, cells = T) |> 
  tidyr::pivot_longer(-cell, names_to = "Habitat", values_to = "Richness") |> 
  dplyr::group_by(cell) |> 
  slice_max(order_by = Richness, n = 1, with_ties = F) 



Template <- Habs_VilhelmsBorg[[1]]
values(Template) <- 0
values(Template)[MaxHabitat$cell] <- MaxHabitat$Richness

MaxHabitatRast <- Template

Template <- Habs_VilhelmsBorg[[1]]
values(Template) <- NA
values(Template)[MaxHabitat$cell] <- MaxHabitat$Habitat

MaxHabitatRastHab <- Template

BDRUtils::write_cog(MaxHabitatRast, "MaxHabitatRast.tif")

BDRUtils::write_cog(MaxHabitatRastHab, "MaxHabitatRastHab.tif")


palFac <- colorFactor(c('#7fc97f','#beaed4','#fdc086','#ffff99'), levels(MaxHabitatRastHab)[[1]]$value,
                               na.color = "transparent")



l <- leaflet() |>  
  addProviderTiles("Esri.WorldImagery") |> 
  addRasterImage(MaxHabitatRastHab, colors = palFac, opacity = 0.7, group = "Best habitat") |> 
  addLegend(pal = palFac, 
            values = levels(MaxHabitatRastHab)[[1]]$value,
            title = "Land Cover",
            labFormat  = labelFormat(
              transform = function(x) {
                levels(MaxHabitatRastHab)[[1]]$label[which(levels(MaxHabitatRastHab)[[1]]$value == x)]
              }))

library(leafem)

pal <- colorNumeric(c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", 
                               "#d53e4f"), values(MaxHabitatRast),
                               na.color = "transparent")



l <- leaflet() |>  
  addProviderTiles("Esri.WorldImagery") |> 
  addRasterImage(MaxHabitatRast, colors = pal, opacity = 0.7) |> 
  addLegend(pal = pal, values = values(Habs_VilhelmsBorg), title = "Species richness") |> 
  addLayersControl(
    baseGroups = names(Habs_VilhelmsBorg),
    options = layersControlOptions(collapsed = T)
  )
    
    