# setup.R

library(here)
library(fs)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(ggalluvial)
library(ggforce)
library(scales)
library(janitor)
library(glue)
# library(cshapes) # Package for CShapes 2.0, a GIS dataset of country borders (1886-today)
library(gt)
library(units)
library(ggridges) # for geom_density_ridges()
library(ggtext) # for  theme(plot.title = element_textbox_simple())

library(sf)
# library(geojsonsf)
library(osmdata)
library(terra)
library(lwgeom)
library(tidyterra)
library(elevatr)
library(USAboundaries)
library(tidycensus)
options(tigris_use_cache = TRUE)
sf::sf_use_s2(TRUE)
# library(nhdplusTools) # Tools for traversing and working with National Hydrography Dataset Plus (NHDPlus) data.

library(broom) # tidy() model output
library(lme4) # lmer() mixed effect models
library(rsq) # r squared for mixed effect models
# library(marginaleffects) # TODO: not used yet - DELETE?

options(scipen = 9)

###### Defaults and constants
# plot_fill <- "#fffff8" # Use same in _quarto.yml and/or css
# plot_fill <- "#ffffff" # white

theme_set(
  theme_light() +
    theme(
      # plot.background = element_rect(fill = plot_fill, color = NA),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      # legend.background = element_rect(fill = plot_fill, color = NA),
      axis.ticks = element_blank(),
      plot.title = element_text(size = rel(2.0)),
      strip.text = element_text(size = rel(1.25))
    )
)

update_geom_defaults("bar", list(fill = "dodgerblue", alpha = 0.6))
update_geom_defaults("col", list(fill = "dodgerblue", alpha = 0.6))

my_caption_self_only <- "Daniel Moul"
my_caption <- "Data: National Inventory of Dams; Plot: Daniel Moul"
my_caption_asce <- "Data: American Society of Civil Engineers (ASCE); Plot: Daniel Moul"
my_caption_nid_census <- "Data: National Inventory of Dams, US Census Bureau; Plot: Daniel Moul"
my_caption_nid_census_nws <- glue("Data: National Inventory of Dams, US Census Bureau,",
                                  "\nNWS National Operational Hydrologic Remote Sensing Center; Plot: Daniel Moul")
my_caption_nid_nws <- "Data: National Inventory of Dams, NWS National Operational Hydrologic Remote Sensing Center; Plot: Daniel Moul"
my_caption_nid_opentopography <- "Data: National Inventory of Dams, OpenTopography.org; Plot: Daniel Moul"
my_caption_opentopography <- "Data: OpenTopography.org; Plot: Daniel Moul"
my_caption_nid_mapzen <- "Data: National Inventory of Dams, AWS Open Data Terrain Tiles\nmanaged by Mapzen; Plot: Daniel Moul"
my_caption_mapzen <- "Data: AWS Open Data Terrain Tiles managed by Mapzen; Plot: Daniel Moul"
my_caption_census_mapzen <- "Data: US Census Bureau, AWS Open Data Terrain Tiles managed by Mapzen; Plot: Daniel Moul"
my_caption_nid_census_mapzen <- "Data: National Inventory of Dams, US Census Bureau,\nAWS Open Data Terrain Tiles managed by Mapzen; Plot: Daniel Moul"
my_caption_nid_nws_mapzen <- glue("Data: National Inventory of Dams, NWS National Operational Hydrologic Remote Sensing Center,",
                                  "\nAWS Open Data Terrain Tiles managed by Mapzen; Plot: Daniel Moul")
my_caption_nid_nws_oms <- glue("Data: National Inventory of Dams, NWS National Operational Hydrologic Remote Sensing Center,",
                                  "\nOpenstreetmap.org; Plot: Daniel Moul")
my_caption_nid_nws_wbd <- glue("Data: National Inventory of Dams, NWS National Operational Hydrologic Remote Sensing Center,",
                                   "\nUSGS Watershed Boundary Database; Plot: Daniel Moul")
my_caption_nid_nws_wbd_oms <- glue("Data: National Inventory of Dams, NWS National Operational Hydrologic Remote Sensing Center,",
                                  "\nUSGS Watershed Boundary Dataset, Openstreetmap.org; Plot: Daniel Moul")

