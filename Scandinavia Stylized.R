library(rayshader)
library(rayvista)
library(elevatr)
library(tidyverse)
library(sf)
library(rgl)
library(giscoR)
library(stars)
library(MetBrewer)
library(colorspace)
library(rnaturalearth)
library(rnaturalearthdata)
library(car)
library(sysfonts)
sf_use_s2(FALSE)
extrafont::
  # 2. RAYVISTA - AREA
  #--------------------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

country_sf <- giscoR::gisco_get_countries(
  country = c("FI", "SE", "NO"),
  resolution = "1") %>%
  sf::st_transform(crs = crsLONGLAT)


bbox <- sf::st_bbox(country_sf)%>%
  sf::st_as_sfc()

country_sf <- sf::st_cast(country_sf, "MULTIPOLYGON")

country_sf_borders <- country_sf

country_sf_borders$geometry <- st_cast(st_geometry(country_sf$geometry), "MULTILINESTRING")%>%
  sf::st_transform(crs = crsLONGLAT)

country_sf_labels <- ne_download(scale = 10, type = "admin_0_countries", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)


country_sf_labels <- as.data.frame(country_sf_labels)%>%
  select(-geometry)

country_sf_labels$geometry <- st_as_sf(country_sf_labels, coords = c('LABEL_X', 'LABEL_Y'))%>%
  st_geometry()

country_sf_labels <- sf::st_as_sf(country_sf_labels)

county_sf <- ne_download(scale = 10, type = "admin_1_states_provinces_lines", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)
county_sf_lines <- ne_download(scale = 10, type = "admin_1_states_provinces_lines", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

county_sf_labels <- ne_download(scale = 10, type = "admin_1_label_points", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

ports_10 <- ne_download(scale = 50, type = "ports", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)
roads_10 <- ne_download(scale = 10, type = "roads", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)
railroads_10 <- ne_download(scale = 10, type = "railroads", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)
rivers_10 <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")%>%
  filter(featurecla == "River")%>%
  st_intersection(bbox)
lakes_10 <- ne_download(scale = 50, type = "lakes", category = "physical", returnclass = "sf")%>%
  st_intersection(bbox)


table(pop_places_10_df$FEATURECLA )

pop_places_10_df <- ne_download(scale = 50, type = "populated_places", category = "cultural", returnclass = "sf")%>%
  filter(POP_MAX >= 1000000)

pop_places_10 <- st_as_sf(pop_places_10_df, coords = c("LONGITUDE","LATITUDE"))%>%
  st_intersection(bbox)

gridlines <- st_as_sf(st_read("data/Grid Lines/World_Latitude_and_Longitude_Grids.shp"))%>%
  st_intersection(bbox)



# 3. RAYVISTA - COUNTRY
#----------------------

country_elevation <- elevatr::get_elev_raster(
  location = country_sf,
  z = 6,
  clip = "location"
)

names(country_elevation) <- "elevation"
plot(country_elevation)

# 3. RAYVISTA - COUNTRY
#----------------------
library(raster)
library(ambient)
library(rayrender)

c2 <- c("#fdf5e6", "#f6e0b5", "#eea990", "#aa6f73","#a39193", "#66545e","#2a1b16")
swatchplot(c2)
texture <- grDevices::colorRampPalette(c2, bias = 1.5)(400)
swatchplot(texture)



country_elevation_ref <- country_elevation
country_elevation_ref[country_elevation_ref$elevation > 9000] <- 0
country_elevation_ref[country_elevation_ref$elevation < 0] <- country_elevation_ref[country_elevation_ref$elevation < 0]/10
lakes_10$geometry <-st_cast(st_geometry(lakes_10$geometry), "MULTIPOLYGON") 
rivers_10_labels <- rivers_10
rivers_10_labels$geometry<- st_centroid(rivers_10_labels$geometry)
gridlines_obj <- generate_line_overlay(gridlines
                                       , extent = sf::st_bbox(country_sf)
                                       , heightmap = raster_to_matrix(country_elevation_ref)
                                       , linewidth = 1)

lakes_10$geometry <-st_cast(st_geometry(lakes_10$geometry), "MULTIPOLYGON") 
rivers_10_labels <- rivers_10
rivers_10_labels$geometry<- st_centroid(rivers_10_labels$geometry)
snow_palette = "white"
snow_hs = height_shade(raster_to_matrix(country_elevation_ref), texture = snow_palette)

par(family = "Lucida Console")

# plot that 3d thing!
raster_to_matrix(country_elevation_ref)%>%
  height_shade(texture = texture )%>%
  add_shadow(ray_shade(raster_to_matrix(country_elevation_ref),zscale=70)) %>%
  add_overlay(generate_polygon_overlay(lakes_10
                                       , attr(country_elevation_ref,"extent")
                                       , heightmap = raster_to_matrix(country_elevation_ref)
                                       , palette = "#005b96"
                                       , linewidth=NA)
              , rescale_original = TRUE
              , alphalayer = 0.5)  %>%
  add_overlay(generate_altitude_overlay(snow_hs, raster_to_matrix(country_elevation_ref)
                                        , 4000
                                        , 9000, lower=FALSE))  %>%
  add_overlay(generate_line_overlay(rivers_10
                                    , attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#005b96"
                                    , linewidth = 4)
              , rescale_original = TRUE
              , alphalayer = 0.5)  %>%
  add_overlay(generate_line_overlay(roads_10
                                    , attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#4b3832"
                                    , linewidth = 1
                                    , lty = 1)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_line_overlay(railroads_10
                                    , attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#000413"
                                    , linewidth = 1
                                    , lty = 5)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_line_overlay(county_sf_lines
                                    , attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#232325"
                                    , lty = 2
                                    , linewidth = 3)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_line_overlay(country_sf_borders,
                                    attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#232325"
                                    , linewidth = 5)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_label_overlay(pop_places_10%>%filter(POP_MAX >= 2000000)
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "#350641"
                                     , text_size = 2
                                     , point_size = 1
                                     , point_color = "#350641"
                                     , data_label_column = "NAME"
                                     , font = 3
                                     , pch = 0)
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(pop_places_10%>%filter(POP_MAX < 2000000)
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "#443332"
                                     , text_size = 1
                                     , point_size = 0.5
                                     , point_color = "#443332"
                                     , font = 3
                                     , data_label_column = "NAME"
                                     , pch = 1)
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(rivers_10_labels
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "#5a3734"
                                     , text_size = 1
                                     , font = 3
                                     , data_label_column = "name")
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(county_sf_labels
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "#2b3c45"
                                     , text_size = 0.5
                                     , font = 3
                                     , data_label_column = "name")
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(country_sf_labels%>%filter(NAME != "Åland")
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "#2c3136"
                                     , text_size = 10
                                     , font = 3
                                     , seed = 2509
                                     , data_label_column = "NAME")
              , rescale_original = TRUE)%>%
  plot_3d(heightmap = raster_to_matrix(country_elevation_ref)
          , zscale = 70
          , solid = F
          , shadowdepth = -257.1
          , theta = 0
          , phi = 80
          , fov = 10
          , water = TRUE
          , watercolor = "#005b96"
          , zoom = 0.65
          , multicore=TRUE
          , software_render = TRUE
          , background = "white"
  )

#Render Floating Overlays
# render_floating_overlay(gridlines_obj
#                         , heightmap = raster_to_matrix(country_elevation_ref)
#                         , alpha = 0.3
#                         , altitude = 0)


## Render Camera ----
render_depth(preview_focus=TRUE
             , heightmap = raster_to_matrix(country_elevation_ref)
             , zscale= 10
             , focallength=10
             , aberration = 1
             , vignette = TRUE)
render_camera(theta = 0
              , phi = 80
              , fov = 10
              , zoom = 0.78)






outfile <- "images/test_plot_scandinavia_stylized.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time),"/n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1),target = outfile)
  }
  render_highquality(
    filename = outfile
    , interactive = FALSE
    , lightaltitude = c(30,100)
    , lightcolor = c(c2[3], "#ffffff")
    , lightintensity = c(500, 150)
    , lightdirection = 240
    , samples = 700
    , width = 4000
    , height = 4000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff),"/n")
}

library(magick)
library(magickGUI)
library(extrafont) 
library(glue)

info_text_region <-glue("
Scandinavia is a subregion in Northern Europe, with strong historical, cultural, 
and linguistic ties between its constituent peoples. Scandinavia most commonly refers 
to Denmark, Norway, and Sweden. It can sometimes also refer more narrowly to the 
Scandinavian Peninsula (which excludes Denmark but includes a part of northern Finland). 
In English usage, Scandinavia is sometimes used as a synonym for Nordic countries. 
Iceland and the Faroe Islands are sometimes included in Scandinavia for their 
ethnolinguistic relations with Sweden, Norway and Denmark. While Finland differs 
from other Nordic countries in this respect, some authors call it Scandinavian 
due to its economic and other similarities.")%>%
  str_wrap(70)

info_text_region_2 <-glue("
The geography of the region is varied, from the Norwegian fjords in the west and 
Scandinavian mountains covering parts of Norway and Sweden, to the low and flat 
areas of Denmark in the south, as well as archipelagos and lakes in the east. 
Most of the population in the region live in the more temperate southern regions, 
with the northern parts having long, cold, winters.")%>%
  str_wrap(80)

info_text_history <- glue("
The region became notable during the Viking Age, when Scandinavian peoples participated 
in large-scale raiding, conquest, colonization and trading mostly throughout Europe. 
They also used their longships for exploration, becoming the first Europeans to 
reach North America. These exploits saw the establishment of the North Sea Empire 
which comprised large parts of Scandinavia and Great Britain, though it was relatively 
short-lived. Scandinavia was eventually Christianized, and the coming centuries 
saw various unions of Scandinavian nations, most notably the Kalmar Union of Denmark, 
Norway and Sweden, which lasted for over 100 years until the Swedish king Gustav I 
led Sweden to independence. It also saw numerous wars between the nations, which 
shaped the modern borders. The most recent union was the union between Sweden and 
Norway, which ended in 1905.

In modern times the region has prospered, with the economies of the countries being 
amongst the strongest in Europe. Sweden, Denmark, Norway, Iceland, and Finland 
all maintain welfare systems considered to be generous, with the economic and 
social policies of the countries being dubbed the "Nordic model".")%>%
  str_wrap(90)

info_text_geo <- glue("The geography of Scandinavia is extremely varied. N
otable are the Norwegian fjords, the Scandinavian Mountains covering much of Norway 
and parts of Sweden, the flat, low areas in Denmark and the archipelagos of Finland, 
Norway and Sweden. Finland and Sweden have many lakes and moraines, legacies of 
the ice age, which ended about ten millennia ago."
, "\n"
, "The southern regions of Scandinavia, which are also the most populous regions, 
have a temperate climate. Scandinavia extends north of the Arctic Circle, but 
has relatively mild weather for its latitude due to the Gulf Stream. Many of the 
Scandinavian mountains have an alpine tundra climate.")%>%
  str_wrap(90)
info_text_culture <- glue("
Nordic countries have historically had some of the most socially progressive cultures 
in the world, and culture is one of the main bases of cooperation between them. 
The policies of the Nordic countries with respect to cultural life, mass media 
and religion have many shared values and features in common. However, some differences 
may be seen, for instance in cultural institutions that arise from historical circumstances. 
In both Denmark and Sweden, there are cultural institutions with roots in the traditions 
of the royal courts. In these countries, national institutions formed the foundation 
of cultural life at an early stage while in Norway cultural institutions began to form later.")%>%
  str_wrap(90)
info_text_history <- glue("
The Viking Age (793–1066 CE) was the period during the Middle Ages when Norsemen 
known as Vikings undertook large-scale raiding, colonising, conquest, and trading 
throughout Europe and reached North America. It followed the Migration Period and 
the Germanic Iron Age. The Viking Age applies not only to their homeland of Scandinavia 
but also to any place significantly settled by Scandinavians during the period. 
The Scandinavians of the Viking Age are often referred to as Vikings as well as 
Norsemen, although few of them were Vikings in the sense of being engaged in piracy."
, "\n"
, "Voyaging by sea from their homelands in Denmark, Norway, and Sweden, the Norse people 
settled in the British Isles, Ireland, the Faroe Islands, Iceland, Greenland, 
Normandy, and the Baltic coast and along the Dnieper and Volga trade routes in 
eastern Europe, where they were also known as Varangians. They also briefly settled 
in Newfoundland, becoming the first Europeans to reach North America. The Norse-Gaels, 
Normans, Rus' people, Faroese, and Icelanders emerged from these Norse colonies. 
The Vikings founded several kingdoms and earldoms in Europe: the Kingdom of the 
Isles (Suðreyjar), Orkney (Norðreyjar), York (Jórvík) and the Danelaw (Danalǫg), 
Dublin (Dyflin), Normandy, and Kievan Rus' (Garðaríki). The Norse homelands were 
also unified into larger kingdoms during the Viking Age, and the short-lived North 
Sea Empire included large swathes of Scandinavia and Britain. In 1021, the Vikings 
achieved the feat of reaching North America - the date of which was not specified 
until a millennium later")%>%
  str_wrap(120)

library(magick)
library(magickGUI)
library(extrafont)
fonts()
map_stylized <- image_read("~/R Projects/3D Atlas/images/test_plot_scandinavia_stylized.png")
map_stylized_annotated <-  map_stylized%>%
  image_annotate(gravity = "center"
                 , location = "+700+1000"
                 , text = "Scandinavia"
                 , weight = 200
                 , size = 200
                 , font = "Baskerville Old Face"
                 , kerning = 20
                 , decoration = "underline"
                 , color = "grey30")%>%
  image_annotate(gravity = "southwest"
                 , location = "+350+750"
                 , text = "Mountain Range "
                 , weight = 200
                 , size = 50
                 , font = "Baskerville Old Face"
                 , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+750"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#66545e")%>%
  ### Elevated Area ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+650"
               , text = "Elevated Area  "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+650"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#aa6f73")%>%
  ### Valleys ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+550"
               , text = "Valleys        "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+550"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#f6e0b5")%>%
  ### Bodies Of Water ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+450"
               , text = "Bodies Of Water"
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+450"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#005b96")%>%
  ### Snow ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+350"
               , text = "Snow"
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+350"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "white")%>%
  ## Lower Part ----
### Admin. Border ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+350"
               , text = "Admin. Border  "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+350"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#2b3c45")%>%
  ### Citys ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+450"
               , text = "Citys          "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+450"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#350641")%>%
  ### Stateborder ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+550"
               , text = "Stateborder    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+550"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#2c3136")%>%
  # Caption ----
image_annotate(gravity = "southeast"
               #, location = "+700-1200"
               , text = "Elevation Map Created by Lasse Rodeck"
               , weight = 200
               , size = 30
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  # info texts ----
image_annotate(gravity = "northwest"
               , location = "+250+150"
               , text = info_text_region
               , weight = 200
               , size = 40
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+450+750"
                 , text = info_text_region_2
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+1700+3300"
                 , text = info_text_history
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+2200+500"
                 , text = info_text_culture
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+1700+150"
                 , text = info_text_geo
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")



image_write(map_stylized_annotated, "~/R Projects/3D Atlas/images/test_plot_scandinavia_stylized_ann.png")

