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
  country = c("IN","NP", "BD", "PK", "BT", "LK"),
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




render_highquality(
  filename = "images/test_plot_india_stylized.png"
  , interactive = FALSE
  , lightaltitude = c(30,100)
  , lightcolor = c(c2[3], "#ffffff")
  , lightintensity = c(600, 350)
  , lightdirection = 240
  , width = 400
  , height = 400
  , smooth_line = TRUE
)



outfile <- "images/test_plot_india_stylized.png"

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
fonts()
india_stylized <- image_read("~/R Projects/3D Atlas/images/test_plot_india_stylized.png")

info_text_region <-glue("
India, officially the Republic of India (ISO: Bhārat Gaṇarājya), is a country in 
South Asia. It is the seventh-largest country by area; the most populous country 
as of June 2023; and from the time of its independence in 1947, the world's most 
populous democracy. Bounded by the Indian Ocean on the south, the Arabian 
Sea on the southwest, and the Bay of Bengal on the southeast, it shares land borders 
with Pakistan to the west; China, Nepal, and Bhutan to the north; and Bangladesh 
and Myanmar to the east. In the Indian Ocean, India is in the vicinity of Sri Lanka 
and the Maldives; its Andaman and Nicobar Islands share a maritime border with 
Thailand, Myanmar, and Indonesia.")%>%
  str_wrap(40)

info_text_history <-glue("
India's history is a tapestry woven with ancient civilizations, diverse cultures, 
and profound philosophical contributions. The Indus Valley Civilization, one of 
the world's oldest urban cultures, flourished around 3300–1300 BCE. The Vedic 
period introduced sacred texts like the Rigveda, shaping the foundations of Hinduism.

The Maurya and Gupta Empires (c. 4th century BCE to 6th century CE) witnessed 
political unity, flourishing arts, and the spread of Buddhism. The medieval 
period saw the rise of powerful dynasties such as the Cholas and the Delhi Sultanate, 
alongside the establishment of the Mughal Empire, known for architectural marvels 
like the Taj Mahal.

Colonialism took hold with the arrival of European powers, particularly the British 
East India Company. The struggle for independence gained momentum in the 20th century, 
led by figures like Mahatma Gandhi. India achieved independence in 1947, partitioned 
into India and Pakistan.

Post-independence, India adopted a democratic constitution, becoming a secular republic. 
The country has faced challenges, including regional conflicts and economic disparities, 
but has also made significant strides in technology, science, and space exploration. 
Today, India stands as one of the world's most populous and culturally diverse nations, 
blending tradition and modernity in a dynamic tapestry of history and progress.")%>%
  str_wrap(130)

info_text_geo <- glue("India accounts for the bulk of the Indian subcontinent, 
lying atop the Indian tectonic plate, a part of the Indo-Australian Plate. 
India's defining geological processes began 75 million years ago when the Indian Plate, 
then part of the southern supercontinent Gondwana, began a north-eastward drift 
caused by seafloor spreading to its south-west, and later, south and south-east. 
Simultaneously, the vast Tethyan oceanic crust, to its northeast, began to subduct 
under the Eurasian Plate. These dual processes, driven by convection in the 
Earth's mantle, both created the Indian Ocean and caused the Indian continental crust 
eventually to under-thrust Eurasia and to uplift the Himalayas. Immediately south 
of the emerging Himalayas, plate movement created a vast crescent-shaped trough 
that rapidly filled with river-borne sediment and now constitutes the Indo-Gangetic Plain. 
The original Indian plate makes its first appearance above the sediment in the 
ancient Aravalli range, which extends from the Delhi Ridge in a southwesterly direction. 
To the west lies the Thar Desert, the eastern spread of which is checked by the Aravallis.")%>%
  str_wrap(50)

info_text_culture <- glue("
Indian cultural history spans more than 4,500 years. During the Vedic period (c. 1700 BCE – c. 500 BCE), 
the foundations of Hindu philosophy, mythology, theology and literature were laid, 
and many beliefs and practices which still exist today, such as dhárma, kárma, yóga, 
and mokṣa, were established. India is notable for its religious diversity, with 
Hinduism, Buddhism, Sikhism, Islam, Christianity, and Jainism among the nation's 
major religions. The predominant religion, Hinduism, has been shaped by various 
historical schools of thought, including those of the Upanishads, the Yoga Sutras, 
the Bhakti movement, and by Buddhist philosophy.")%>%
  str_wrap(90)


info_text_culture_2 <- glue("
The foundation of a typical Indian meal is a cereal cooked in a plain fashion and 0
complemented with flavourful savoury dishes. The cooked cereal could be steamed rice; 
chapati, a thin unleavened bread made from wheat flour, or occasionally cornmeal, 
and griddle-cooked dry; the idli, a steamed breakfast cake, or dosa, a griddled 
pancake, both leavened and made from a batter of rice- and gram meal. 
The savoury dishes might include lentils, pulses and vegetables commonly spiced 
with ginger and garlic, but also with a combination of spices that may include coriander, 
cumin, turmeric, cinnamon, cardamon and others as informed by culinary conventions. 
They might also include poultry, fish, or meat dishes. In some instances, the ingredients 
might be mixed during the process of cooking.")%>%
  str_wrap(60)

map_stylized_annotated <- india_stylized%>%
  image_annotate(gravity = "center"
                 , location = "+700+1000"
                 , text = "India"
                 , weight = 200
                 , size = 200
                 , font = "Lucida Console"
                 , kerning = 20
                 , decoration = "underline"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  # Legend ----
## Lower Part ----
### Mountain Range ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+750"
               , text = "Mountain Range "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
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
               , color = "grey30"
               , strokecolor = "grey30")%>%
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
               , color = "grey30"
               , strokecolor = "grey30")%>%
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
               , color = "grey30"
               , strokecolor = "grey30")%>%
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
               , color = "grey30"
               , strokecolor = "grey30")%>%
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
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+350"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#2a1b16"
               , strokecolor = "grey30")%>%
  ### Citys ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+450"
               , text = "Citys          "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+450"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "grey26")%>%
  ### Stateborder ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+550"
               , text = "Stateborder    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+550"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#2a1b16"
               , strokecolor = "grey30")%>%
  # Caption ----
image_annotate(gravity = "southeast"
               #, location = "+700-1200"
               , text = "Elevation Map Created by Lasse Rodeck"
               , weight = 200
               , size = 30
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  # info texts ----
image_annotate(gravity = "northwest"
               , location = "+250+150"
               , text = info_text_region
               #, weight = 200
               , size = 40
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+1650+250"
                 , text = info_text_history
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+150+1850"
                 , text = info_text_geo
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+2450+750"
                 , text = info_text_culture
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+2750+2200"
                 , text = info_text_culture_2
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")



image_write(map_stylized_annotated, "~/R Projects/3D Atlas/images/test_plot_india_stylized_ann.png")

