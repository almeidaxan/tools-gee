require(pacman)
p_load(data.table)
p_load(leaflet)
p_load(magrittr)
p_load(rPython)
p_load(raster)
p_load(SDMTools)
p_load(shiny)
p_load(shinyBS)
p_load(shinyjs)

proj_ll <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj_utm <- function(shape) {
	p <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +units=m"
	paste0(p, " +zone=", zoneCalc(extent(shape)[1]))
}

# calculates UTM zone based on long coordinate
zoneCalc <- function(long) {
	(floor((long + 180)/6) %% 60) + 1
}