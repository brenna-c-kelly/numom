
library(sf)
library(tmap)
library(viridis)

# data
geo <- read.csv("geocoded_addresses.csv")
names(geo) <- tolower(names(geo))

# touch-up
geo <- geo[-which(geo$latitude == 0), ] # out of state, not labeled as "other"
geo <- geo %>%
  filter(!grepl("Other", full_address)) # out-of-state, labeled as "other
geo <- subset(geo, select = -c(x)) # removing index

# convert to sf
geo <- st_as_sf(geo, coords = c("longitude", "latitude"),
                crs = st_crs(grid), agr = "constant")
# tm_shape(grid) +
#   tm_polygons() +
#   tm_shape(geo_visit) +
#   tm_dots()

# schwartz grid
grid <- st_read("/Users/brenna/Documents/School/Research/aq-pregnancy/data/shapefiles/NO2Grid(Polygons).shp") # populated ids are too limited for mom data

# to retain visit information, old address data is combined with geocoded addresses
addresses <- read.csv("addresses.csv")
geo_visit <- merge(geo, addresses, by = "full_address", all = TRUE) %>%
  filter(!grepl("Other", full_address))

# spatial join to get the grid id for each point
geo_grid <- st_join(geo_visit, grid, join = st_nearest_feature)
# st_nearest is used because 9 points weren't properly contained within the grids (despite matching st_crs)

geo_grid_sh <- st_drop_geometry(geo_grid) # don't want point geometry
# tidying up
geo_grid_sh <- geo_grid_sh[, c("studyid.x", "publicid.x", "visit",
                               "full_address", "grid_id")]
names(geo_grid_sh) <- c("studyid", "publicid", "visit",
                        "full_address", "grid_id")

# merge grid-coded data with the grid shapefile (for plotting, mostly)
gridded_addresses <- merge(grid, geo_grid_sh, by = "grid_id", all.x = FALSE, all.y = TRUE)

# sanity check: map the points and grids for these counties
cities <- geo[which(geo$county %in% c("Davis County, Weber County",
                                      "Salt Lake County", "Utah County")), c("city")] %>%
  st_drop_geometry()
cities <- unique(cities) # list of cities in the counties (counties aren't listed in address)

# filtering only points in counties
geo_grid_map <- geo_grid_sh %>%
  filter(grepl(paste(cities, collapse="|"),
               full_address))

# count of points belonging to each grid cell
grid_count <- as.data.frame(table(geo_grid_map$grid_id))
names(grid_count) <- c("grid_id", "count")

# merging with grid shapefile
grid_count <- merge(grid, grid_count, by = "grid_id", all.x = FALSE, all.y = TRUE)

# filtering only points in counties
geo_grid_pts <- geo_grid %>%
  filter(grepl(paste(cities, collapse="|"),
               full_address))

# mapping
grids <- tm_shape(grid_count) +
  tm_polygons(col = "count", lwd = 0, palette = "-viridis", style = "cont") +
  tm_layout(main.title = "Individuals in each grid cell", 
            main.title.position = "left",
            main.title.size = 1)
points <- tm_shape(geo_grid_pts) +
  tm_dots(col = "purple4", alpha = 0.25, size = 0.1) +
  tm_layout(main.title = "Geocoded locations", 
            main.title.position = "left",
            main.title.size = 1)
# tmap_arrange(points, grids)


# saving the gridded address data with grid shapefile
gridded_addresses <- gridded_addresses[, c("grid_id", "studyid",
                                           "visit", "publicid")]
st_write(gridded_addresses, "gridded_addresses.shp")

# saving the gridded address data without shapefile
gridded_addresses <- st_drop_geometry(gridded_addresses)
write.csv(gridded_addresses, "gridded_addresses.csv")


