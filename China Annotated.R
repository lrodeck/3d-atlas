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
  country = c("CN"),
  resolution = "1") %>%
  sf::st_transform(crs = crsLONGLAT)


bbox <- sf::st_bbox(country_sf)%>%
  sf::st_as_sfc()

country_sf <- sf::st_cast(country_sf, "MULTIPOLYGON")

country_sf_borders <- country_sf

country_sf_borders$geometry <- st_cast(st_geometry(country_sf$geometry), "MULTILINESTRING")%>%
  sf::st_transform(crs = crsLONGLAT)

county_sf <- ne_download(scale = 10, type = "admin_1_states_provinces_lines", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

county_sf_lines <- ne_download(scale = 10, type = "admin_1_states_provinces_lines", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

county_sf_labels <- ne_download(scale = 10, type = "admin_1_label_points", category = "cultural", returnclass = "sf")%>%
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
# gridlines_obj <- generate_line_overlay(gridlines
#                                        , extent = sf::st_bbox(country_sf)
#                                        , heightmap = raster_to_matrix(country_elevation_ref)
#                                        , linewidth = 1)

lakes_10$geometry <-st_cast(st_geometry(lakes_10$geometry), "MULTIPOLYGON") 
rivers_10_labels <- rivers_10
rivers_10_labels$geometry<- st_centroid(rivers_10_labels$geometry)
snow_palette = "white"
snow_hs = height_shade(raster_to_matrix(country_elevation_ref), texture = snow_palette)

par(family = "Baskerville Old Face")

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
                                     , text_size = 0.6
                                     , point_size = 1
                                     , point_color = "grey26"
                                     , data_label_column = "NAME"
                                     , pch = 0)
              , rescale_original = TRUE)%>%
  add_overlay(generate_label_overlay(pop_places_10%>%filter(POP_MAX < 2000000)
                                     , attr(country_elevation_ref,"extent")
                                     , heightmap = raster_to_matrix(country_elevation_ref)
                                     , color = "grey26"
                                     , text_size = 0.3
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
             , zscale= 70
             , focallength=250
             , aberration = 1
             , vignette = TRUE)
render_camera(theta = 0
              , phi = 80
              , fov = 50
              , zoom = 0.7)




outfile <- "images/test_plot_china_stylized.png"

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
China, officially the People's Republic of China (PRC), 
is a country in East Asia. It is the world's second-most-populous country, with a 
population exceeding 1.4 billion. China spans the equivalent of five time zones 
and borders fourteen countries by land, tied with Russia as having the most of 
any country in the world. With an area of nearly 9.6 million square kilometers 
(3,700,000 sq mi), it is the world's third-lar1gest country by total land area. 
The country is divided into 22 provinces,[n] five autonomous regions, four municipalities, 
and two semi-autonomous special administrative regions. The national capital is 
Beijing, and the most populous city and largest financial center is Shanghai.")%>%
  str_wrap(70)

info_text_region_2 <-glue("

China, inhabited since the Paleolithic era, saw the emergence of early dynastic 
states like the Shang and Zhou along the Yellow River by the late second millennium 
BCE. From 221 BCE, over two millennia, imperial dynasties like the Han, Tang, Ming, 
and Qing governed, fostering achievements like gunpowder, the Silk Road, and the 
Great Wall. China's influence on East Asia, including language, traditions, and 
philosophy, was profound during this period.

Today, China is a one-party socialist republic led by the CCP, holding a permanent 
seat on the UN Security Council and playing key roles in international organizations 
like the Asian Infrastructure Investment Bank and the G20. Despite economic strength, 
China faces criticism for issues like democracy, transparency, and human rights, 
ranking poorly in press freedom and ethnic equality. As the world's largest economy 
by GDP at purchasing power parity, a major manufacturer, and exporter, China wields 
significant global influence, also maintaining the world's largest standing army 
and the second-largest defense budget.")%>%
  str_wrap(80)

info_text_history <- glue("
China's history spans millennia and is marked by distinct periods that have shaped 
its cultural, political, and economic landscape. In ancient times, the Xia, Shang, 
and Zhou dynasties laid the foundation for Chinese civilization. The Qin Dynasty's 
unification, the Han Dynasty's cultural advancements, and the Silk Road trade routes 
characterized early imperial China.

Following a period of disunity and foreign invasions, the Ming Dynasty restored Han 
Chinese rule, while the Qing Dynasty, established by the Manchus, marked the last 
imperial era. Modernization efforts in the 19th century led to the downfall of the 
Qing Dynasty, giving rise to the Republic of China in 1912.

The Chinese Civil War between the Nationalists and Communists ended with the 
establishment of the People's Republic of China in 1949 under Communist leadership. 
The mid-20th century saw the tumultuous Cultural Revolution, followed by Deng Xiaoping's 
economic reforms in the late 20th century, propelling China into a phase of rapid 
economic growth and global integration. Contemporary China is characterized by 
economic prowess, technological advancements, and significant geopolitical 
influence. The country's history, marked by periods of unity, fragmentation, and 
cultural evolution, continues to shape its identity and global standing today.")%>%
  str_wrap(90)

info_text_culture <- glue("Since ancient times, Chinese culture has been heavily 
influenced by Confucianism. Chinese culture, in turn, has heavily influenced East 
Asia and Southeast Asia. For much of the country's dynastic era, opportunities 
for social advancement could be provided by high performance in the prestigious 
imperial examinations, which have their origins in the Han dynasty. The literary 
emphasis of the exams affected the general perception of cultural refinement in 
China, such as the belief that calligraphy, poetry and painting were higher forms 
of art than dancing or drama. Chinese culture has long emphasized a sense of deep 
history and a largely inward-looking national perspective. Examinations and a 
culture of merit remain greatly valued in China today.")%>%
  str_wrap(40)

info_text_geo <- glue("China's landscape is vast and diverse, ranging from the 
                      Gobi and Taklamakan Deserts in the arid north to the subtropical 
                      forests in the wetter south. The Himalaya, Karakoram, Pamir 
                      and Tian Shan mountain ranges separate China from much of 
                      South and Central Asia. The Yangtze and Yellow Rivers, the 
                      third- and sixth-longest in the world, respectively, run from 
                      the Tibetan Plateau to the densely populated eastern seaboard. 
                      China's coastline along the Pacific Ocean is 14,500 km (9,000 mi) 
                      long and is bounded by the Bohai, Yellow, East China and South 
                      China seas. China connects through the Kazakh border to the 
                      Eurasian Steppe which has been an artery of communication 
                      between East and West since the Neolithic through the Steppe 
                      Route – the ancestor of the terrestrial Silk Road(s).")%>%
  str_wrap(90)

map_stylized_annotated <-  map_stylized%>%
  # Title ----
image_annotate(gravity = "center"
               , location = "+700+1500"
               , text = "China"
               , weight = 200
               , size = 200
               , font = "Baskerville Old Face"
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
               , strokecolor = "grey30"
               )%>%
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
               , weight = 200
               , size = 40
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+2600+250"
                 , text = info_text_region_2
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+1100+600"
                 , text = info_text_history
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+3200+1800"
                 , text = info_text_culture
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+150+2700"
                 , text = info_text_geo
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")



image_write(map_stylized_annotated, "~/R Projects/3D Atlas/images/test_plot_china_stylized_ann.png")
