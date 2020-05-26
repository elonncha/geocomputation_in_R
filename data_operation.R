
githubinstall::githubinstall()
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
#> Coordinate Reference System
st_crs(world)

# Raster Data
set.seed(1)
newr = raster(system.file("raster/srtm.tif", package = "spDataLarge"))
testr = raster(xmn = -100, xmx = 100, ymn = -100, ymx = 100, nrows = 200, ncol = 200, vals = rnorm(40000, mean = 0, sd = 1))
spplot(testr)

##### Chapter 3 Attribute Data Operations
# 