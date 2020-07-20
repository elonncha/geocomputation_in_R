librarySetUp = function () {
  library(leaflet)
  library(sf)
  library(tmap)
  library(raster)
  library(spData)
  library(RColorBrewer)
  library(units)
  library(tidyverse)
}

librarySetUp()
tmaptools::palette_explorer()
# a taste of leaflet
popup = c("Robin", "Jakub", "Jannes")
leaflet() %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
  addMarkers(lng = c(-3, 23, 11),
             lat = c(52, 53, 49), 
             popup = popup)



##### Chapter 2 Geographic Data in R
# Vector Data in sf
tmap_mode('view')
data(world)
head(world)
worldpopmap = tm_shape(world) + 
                tm_polygons(col = 'continent', border.col = 'white', palette = brewer.pal(n=6, 'Set1'), legend.hist = TRUE) + 
                tm_text('name_long', size = 0.6, clustering = FALSE)
# create a sf column (basic geometry-geometry column sfc-geodatabase)
polygonlist = list()
for (i in seq(1,50,1)) {
  polygonlist[i] = world$geom[i]
}
geomc = st_sfc(polygonlist, crs = st_crs(world)) # from shape to sfc
newsf = st_sf(data.frame(x=seq(1,50,1), geomc)) # combine sfc with data
# Coordinate Reference System
st_crs(world)

# Raster Data
set.seed(1)
newr = raster(system.file("raster/srtm.tif", package = "spDataLarge"))
testr = raster(xmn = -100, xmx = 100, ymn = -100, ymx = 100, nrows = 200, ncol = 200, vals = rnorm(40000))
spplot(testr)

rstack = stack(testr, testr)



##### Chapter 4 Spatial Data Operations
# nz data set
data(nz)
head(nz)
data(nz_height)
head(nz_height)
canterbury = nz %>% filter(Name == 'Canterbury')

hpinCan = nz_height %>% filter(st_within(., canterbury, sparse = FALSE))
nz_height2 = nz %>%
  st_join(., nz_height, join = st_contains) %>% 
  group_by(Name) %>%
  summarise(evelation = mean(elevation))

# spatial subsetting
us = world %>% filter(name_long == 'United States')
ctUS = world %>% filter(st_touches(world, us, sparse = FALSE))

# spatial joining
set.seed(1)
bb_world = st_bbox(world)
x = runif(20, min = bb_world[1], max = bb_world[3])
y = runif(20, min = bb_world[2], max = bb_world[4])
rdf = data.frame(x,y)

rpoint = st_as_sf(rdf, coords = c('x','y'))
rpoint = st_set_crs(rpoint, 4326)

tm_shape(world) + tm_polygons() + tm_shape(rpoint) + tm_dots()

cont = world %>% group_by(continent) %>% summarise_if(is.numeric, mean, rm.na = TRUE)
tm_shape(cont) + tm_polygons()

# raster data operation
#subsetting (drop = FALSE)
spplot(testr[values(testr) >= 1, drop = FALSE])

#reclassify
rcl = matrix(c(-99,-1,-1,-1,1,0,1,99,99), ncol = 3, byrow = TRUE)
recl = reclassify(testr, rcl)
spplot(recl)

#focal (moving window averaging)
r_focal = focal(testr, w = matrix(1,nrow = 5, ncol = 5), fun = mean)
spplot(r_focal)


##### Chapter 5 Geometry
tm_shape(nz) + tm_polygons() + tm_shape(st_point_on_surface(nz)) + tm_dots()

## sf-geojson
shp = st_transform(nz, CRS("+proj=longlat +datum=WGS84"))
geojson = sf_geojson(shp)
write(geojson,'geo.geojson')