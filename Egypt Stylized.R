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
  country = "EG",
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
  z = 7,
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
                                    , color = "#2a1b16"
                                    , lty = 2
                                    , linewidth = 3)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_line_overlay(country_sf_borders,
                                    attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#2a1b16"
                                    , linewidth = 5)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_label_overlay(pop_places_10%>%filter(POP_MAX >= 2000000)
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "grey26"
                                     , text_size = 2
                                     , point_size = 1
                                     , point_color = "grey26"
                                     , data_label_column = "NAME"
                                     , pch = 0)
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(pop_places_10%>%filter(POP_MAX < 2000000)
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "grey26"
                                     , text_size = 1
                                     , point_size = 0.5
                                     , point_color = "grey26"
                                     , data_label_column = "NAME"
                                     , pch = 1)
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(rivers_10_labels
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "grey20"
                                     , text_size = 1
                                     , data_label_column = "name")
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(county_sf_labels
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "#c9b5aa"
                                     , text_size = 0.5
                                     , data_label_column = "name")
              , rescale_original = TRUE)%>%
  plot_3d(heightmap = raster_to_matrix(country_elevation_ref)
          , zscale = 70
          , solid = F
          , shadowdepth = -44.2
          , theta = 0
          , phi = 80
          , fov = 10
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
              , zoom = 0.65)



outfile <- "images/test_plot_egypt_stylized.png"

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
map_stylized <- image_read(outfile)

info_text_region <-glue("
The region lies near the intersection of geological plates, 
with both heavy seismic and volcanic activities.The Sunda Plate is the main plate 
of the region, featuring almost all Southeast Asian countries except Myanmar, northern Thailand, 
northern Laos, northern Vietnam, and northern Luzon of the Philippines, while the 
Sunda Plate only includes western Indonesia to as far east as the Indonesian province of Bali. 
The mountain ranges in Myanmar, Thailand, Peninsular Malaysia and the Indonesian 
islands of Sumatra, Java, Bali, Lesser Sunda Islands, and Timor are part of the 
Alpide belt, while the islands of the Philippines and Indonesia as well as East 
Timor are part of the Pacific Ring of Fire. Both seismic belts meet in Indonesia, 
causing the region to have relatively high occurrences of earthquakes and volcanic 
eruptions, particularly in the Philippines and Indonesia.")%>%
  str_wrap(60)

info_text_region_2 <-glue("
It covers about 4,500,000 km2 (1,700,000 sq mi), which is 8% of Eurasia and 3% of 
Earth's total land area. Its total population is more than 675 million, about 8.5% 
of the world's population. It is the third most populous geographical region in 
Asia after South Asia and East Asia.[8] The region is culturally and ethnically 
diverse, with hundreds of languages spoken by different ethnic groups. Ten countries 
in the region are members of the Association of Southeast Asian Nations (ASEAN), 
a regional organisation established for economic, political, military, educational, 
and cultural integration among its members. 
 
Southeast Asia is one of the most culturally diverse regions of the world. There 
are many different languages and ethnicities in the region. Historically, Southeast 
Asia was significantly influenced by Indian, Chinese, Muslim, and colonial cultures, 
which became core components of the region's cultural and political institutions. 
Most modern Southeast Asian countries were colonized by European powers. European 
colonisation exploited natural resources and labour from the lands they conquered, 
and attempted to spread European institutions to the region.[11] Several Southeast 
Asian countries were also briefly occupied by the Japanese Empire during World War II. 
The aftermath of World War II saw most of the region decolonised. Today, Southeast 
Asia is predominantly governed by independent states.")%>%
  str_wrap(80)

info_text_history <- glue("Southeast Asia boasts a rich and intricate history shaped by a tapestry of cultures, empires, and vibrant maritime trade routes. Its historical narrative unfolds across various epochs, each leaving an indelible mark on the region.
In antiquity, Southeast Asia witnessed the rise of advanced civilizations, notably the Khmer Empire in Cambodia and the Srivijaya Empire in Sumatra, which thrived from the 7th to the 14th centuries. During this period, Indian and Chinese influences permeated the region, influencing language, religion, and artistic expressions.
The maritime trade routes were vital in shaping Southeast Asian history, with empires like Srivijaya and Majapahit controlling these routes and spreading Hinduism and Buddhism. The Malacca Sultanate, in the 15th century, emerged as a pivotal trading center, connecting the East and West.
The 16th century marked the onset of European colonization, with powers such as the Portuguese, Dutch, Spanish, and British establishing a presence in various parts of Southeast Asia. The Dutch East India Company and British East India Company played instrumental roles in this colonial enterprise.
The 19th century witnessed the consolidation of European control over the region. However, the aftermath of World War II saw a surge in nationalist movements, leading to the independence of countries like Indonesia, the Philippines, Vietnam, and Malaysia.
Post-independence, Southeast Asia faced challenges, including the First Indochina War and the Vietnam War, as countries sought to define their political identities. In 1967, the region took a significant step towards cooperation and stability with the establishment of the Association of Southeast Asian Nations (ASEAN), comprising Indonesia, Malaysia, the Philippines, Singapore, and Thailand.
In recent decades, Southeast Asia has experienced rapid economic development and urbanization. Nations like Singapore and Malaysia have emerged as economic powerhouses, contributing to the region's global significance. Despite progress, challenges such as political instability, ethnic conflicts, and environmental issues persist in certain countries.
Culturally, Southeast Asia is celebrated for its diversity, encompassing indigenous traditions, Hindu-Buddhist heritage, Islamic influences, and the assimilation of global cultural elements. The region's history is a testament to its resilience and adaptability, reflecting a dynamic interplay of local and external forces across the centuries.")%>%
  str_wrap(100)

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


image_write(map_stylized_annotated, "~/R Projects/3D Atlas/images/test_plot_se_asia_stylized_ann.png")
