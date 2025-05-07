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
library(osmdata)

# 2. RAYVISTA - AREA
#--------------------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"


xmin <- 68
ymin <- 7
xmax <- 98
ymax <- 35

bbox <- sf::st_sfc(
  sf::st_polygon(
    list(
      cbind(
        c(xmin, xmax, xmax, xmin, xmin),
        c(ymin, ymin, ymax, ymax, ymin)
      )
    )), crs = crsLONGLAT) %>%
  sf::st_as_sf()

plot(bbox)

rivers <- st_as_sf(st_read("data/Rivers/HydroRIVERS_v10.shp"))
landforms <- st_as_sf(st_read("data/Landforms/ne_10m_geography_regions_polys.shp"))%>%
  filter(FEATURECLA %in% c("Coast", "Desert"))%>%
  mutate(fill = case_when(FEATURECLA == "Coast" ~ 100
                          ,FEATURECLA == "Desert" ~ 200)
  )

lakes <- st_as_sf(st_read("data/Lakes/HydroLAKES_polys_v10.shp"))



# 3. RAYVISTA - COUNTRY
#----------------------


country_elevation <- elevatr::get_elev_raster(
  locations = bbox,
  z = 6,
  clip = "location"
)

names(country_elevation) <- "elevation"

plot(country_elevation)



# 3. RAYVISTA - COUNTRY
#----------------------
library(raster)
library(ambient)

c1 <- met.brewer("Hiroshige")

c3 <- c("#EFF9FF","#E1E9EA", "#B3765A", "#B8474C")
c2 <- c("#FAD5A5", "#90BF7A" , "#FAD5A5","#AA883C","#462A11")
swatchplot(c2)
swatchplot(grDevices::colorRampPalette(c2)(5))
texture <- grDevices::colorRampPalette(c2, bias = 1)(400)
swatchplot(texture)

country_elevation_ref <- country_elevation
country_elevation_ref[country_elevation_ref$elevation > 5000] <- 0
country_elevation_ref[country_elevation_ref$elevation < 0] <- country_elevation_ref[country_elevation_ref$elevation < 0]/10
hist(country_elevation)
hist(country_elevation_ref)

plot(country_elevation_ref)

# plot that 3d thing!
raster_to_matrix(country_elevation_ref)%>%
  height_shade(texture = texture)%>%
  add_overlay(generate_line_overlay(rivers,
                                    attr(country_elevation_ref,"extent")
                                    , heightmap = raster_to_matrix(country_elevation_ref)
                                    , color = "#517272"
                                    , linewidth = 10)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_polygon_overlay(lakes,
                                       attr(country_elevation_ref,"extent")
                                       , heightmap = raster_to_matrix(country_elevation_ref)
                                       , palette = "#517272"
                                       , linecolor = NA
                                       , linewidth = 10)
              , rescale_original = TRUE)  %>%
  add_overlay(generate_polygon_overlay(landforms
                                       , attr(country_elevation_ref,"extent")
                                       , heightmap = raster_to_matrix(country_elevation_ref)
                                       , palette = "#fbe1c0"
                                       , linecolor = "#fbe1c0")
              , rescale_original = TRUE)  %>%
  add_water(detect_water(raster_to_matrix(country_elevation_ref)),color="imhof3")%>%
  plot_3d(heightmap = raster_to_matrix(country_elevation_ref)
          , zscale = 100
          , solid = F
          , shadowdepth = -561.7
          , water=TRUE
          , watercolor="#517272"
  )

## Render Camera ----

render_camera(theta = 0
              , phi = 80
              , fov = 10
              , zoom = 0.7)
## Render Movie ----
# render_movie()

outfile <- "images/final_plot_east_asia.png"

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
    , lightcolor = c(c1[2], "#ffffff")
    , lightintensity = c(600, 350)
    , lightdirection = 240
    , samples = 700
    , width = 2000
    , height = 4500
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff),"/n")
}


