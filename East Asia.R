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
sf_use_s2(FALSE)
# 1. Data
#--------------------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"


xmin <- 104.589844
ymin <- 30.675715
xmax <- 145.634766
ymax <- 57.610107

bbox <- sf::st_sfc(
  sf::st_polygon(
    list(
      cbind(
        c(xmin, xmax, xmax, xmin, xmin),
        c(ymin, ymin, ymax, ymax, ymin)
      )
    )), crs = crsLONGLAT) %>%
  sf::st_as_sf()

# plot(bbox)
# 
# country_sf <- giscoR::gisco_get_countries(
#   region = "Asia",
#   resolution = "1") %>%
#   sf::st_transform(crs = crsLONGLAT)%>%
#   st_intersection(bbox)
# 
# country_sf_borders <- ne_download(scale = 50, type = "admin_0_boundary_lines_land", category = "cultural", returnclass = "sf")%>%
#   sf::st_transform(crs = crsLONGLAT)%>%
#   st_intersection(bbox)
# 
# country_sf_borders <- country_sf
# 
# country_sf_borders$geometry <- st_cast(st_geometry(country_sf$geometry), "MULTILINESTRING")%>%
#   sf::st_transform(crs = crsLONGLAT)

rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")%>%
  filter(featurecla == "River")%>%
  st_intersection(bbox)

landforms <- st_as_sf(st_read("data/Landforms/ne_10m_geography_regions_polys.shp"))%>%
  filter(FEATURECLA %in% c("Coast", "Desert"))%>%
  mutate(fill = case_when(FEATURECLA == "Coast" ~ 100
                          ,FEATURECLA == "Desert" ~ 200)
  )%>%
  st_intersection(bbox)

lakes <- ne_download(scale = 50, type = "lakes", category = "physical", returnclass = "sf")%>%
  st_intersection(bbox)

pop_places_10_df <- ne_download(scale = 50, type = "populated_places", category = "cultural", returnclass = "sf")%>%
  filter(POP_MAX >= 500000)

pop_places_10 <- st_as_sf(pop_places_10_df, coords = c("LONGITUDE","LATITUDE"))%>%
  st_intersection(bbox)

urban_areas_10 <- ne_download(scale = 10, type = "urban_areas_landscan", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

roads_10 <- ne_download(scale = 10, type = "roads", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

railroads_10 <- ne_download(scale = 10, type = "railroads", category = "cultural", returnclass = "sf")%>%
  st_intersection(bbox)

boarders <- rnaturalearth::ne_download(scale = 10, type = "admin_0_boundary_lines_land", category = "cultural", returnclass = "sf")

boarders_ru <- boarders%>%
  filter(ADM0_RIGHT == "Russia" & ADM0_LEFT == "North Korea")%>%
  sf::st_transform(crs = crsLONGLAT)

boarders_reg <-  boarders%>%
  st_intersection(bbox)

# 2. Elevation Data
#----------------------


country_elevation <- elevatr::get_elev_raster(
  locations = bbox,
  z = 6,
  clip = "location"
)

# desert_elevation <- elevatr::get_elev_raster(
#   locations = landforms,
#   z = 7,
#   clip = "location"
# )



names(country_elevation) <- "elevation"

plot(country_elevation)



# 3. Plotting and Rendering
#----------------------
library(raster)
library(ambient)

c1 <- met.brewer("Hiroshige")

c2 <- c("#fdf5e6", "#f6e0b5", "#eea990", "#aa6f73","#a39193", "#66545e","#2a1b16")
swatchplot(c2)
texture <- grDevices::colorRampPalette(c2, bias = 0.9)(500)
swatchplot(texture)

country_elevation_ref <- country_elevation
country_elevation_ref[country_elevation_ref$elevation > 5000] <- 0
country_elevation_ref[country_elevation_ref$elevation < 0] <- country_elevation_ref[country_elevation_ref$elevation < 0]/10
hist(country_elevation)
hist(country_elevation_ref)

plot(country_elevation_ref)

lakes$geometry <-st_cast(st_geometry(lakes$geometry), "MULTIPOLYGON") 
rivers_labels <- rivers
rivers_labels$geometry<- st_centroid(rivers_labels$geometry)
snow_palette = "white"
snow_hs = height_shade(raster_to_matrix(country_elevation_ref), texture = snow_palette)


# desert_hs = height_shade(raster_to_matrix(desert_elevation)
#                          , texture = (grDevices::colorRampPalette(c("#fbe1c0", "#af9573", "#765f2a")))(256)
#                          )



# plot that 3d thing!
raster_to_matrix(country_elevation_ref)%>%
  height_shade(texture = texture)%>%
  add_overlay(generate_line_overlay(rivers,
                                    attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#93b8e2"
                                    , linewidth = 10)
              , rescale_original = TRUE)  %>%
  # add_overlay(generate_line_overlay(boarders_ru,
  #                                   attr(country_elevation_ref,"extent")
  #                                   , heightmap = raster_to_matrix(country_elevation_ref)
  #                                   , color = "#ff8b94"
  #                                   , linewidth = 20)
  #             , rescale_original = TRUE)  %>%
  add_overlay(generate_polygon_overlay(lakes,
                                       attr(country_elevation_ref,"extent")
                                       , heightmap = raster_to_matrix(country_elevation_ref)
                                       , palette = "#93b8e2"
                                       , linecolor = NA
                                       , linewidth = 10)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_altitude_overlay(snow_hs, raster_to_matrix(country_elevation_ref)
                                        , 2000
                                        , 3000, lower=FALSE))  %>%
  add_overlay(generate_line_overlay(boarders_reg,
                                    attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#2a1b16"
                                    , linewidth = 5)
              , rescale_original = TRUE)  %>%
  add_water(detect_water(raster_to_matrix(country_elevation_ref)),color="imhof3")%>%
  add_overlay(generate_polygon_overlay(urban_areas_10
                                       , attr(country_elevation_ref,"extent")
                                       , heightmap = raster_to_matrix(country_elevation_ref)
                                       , palette = "#5d807f"
                                       , linewidth=NA
                                       
                                       )
              , rescale_original = TRUE
              , alphalayer=0.7
              )  %>%
  add_overlay(generate_line_overlay(roads_10
                                    , attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#4b4345"
                                    , linewidth = 2
  )
  , rescale_original = TRUE)  %>%
  add_overlay(generate_line_overlay(railroads_10
                                    , attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "grey2"
                                    , linewidth = 2
  )
  , rescale_original = TRUE)  %>%
  add_shadow(ambient_shade(raster_to_matrix(country_elevation_ref)), 0) %>%
  plot_3d(heightmap = raster_to_matrix(country_elevation_ref)
          , zscale = 50
          , solidcolor = "#a39193"
          , solid = T
          , soliddepth = -965.700000
          , lineantialias = T
          , solidlinecolor = "black"
          , waterlinecolor = "white"
          , shadowdepth = -965.700000
          , water=TRUE
          , watercolor="#93b8e2"
  )




## Render Camera ----

render_camera(theta = 0
              , phi = 40
              , fov = 100
              , zoom = 0.45)


render_depth(focallength = 800, filename = "images/depth_map_east_asia.png", width = 7000, height = 7000)
              

outfile <- "images/final_plot_east_asia_2.png"

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
    , lightcolor = c("#f6e0b5", "#ffffff")
    , lightintensity = c(500, 150)
    , lightdirection = 240
    , samples = 700
    , width = 7000
    , height = 7000
    , focal_distance = 700
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff),"/n")
}


