library(sf)
library(spData)
library(readr)
setwd("F://gdelt")
protest <- read_csv("cnhkmo_protest_data.csv")
protest <- protest %>% filter(GLOBALEVENTID %in% unique(GLOBALEVENTID)) #remove duplicates
#cn_geojson <- st_read("CN province shapes//gadm36_CHN_1.json")
cn_shp <- st_read("CN province shapes//gadm36_CHN_1.shp")

lonlat_to_state <- function(pointsDF,
                            states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

protest$region <- lonlat_to_state(data.frame(long = protest$ActionGeo_Long,
                                             lat = protest$ActionGeo_Lat),
                                  states = cn_shp,
                                  name_col = "NAME_1")
protest$region <- ifelse(protest$ActionGeo_CountryCode == "HK", "Hong Kong", protest$region)
protest$region <- ifelse(protest$ActionGeo_CountryCode == "MC", "Macau", protest$region)

View(protest)

protest <- protest[,-1]

protest <- protest %>% relocate(where(is.numeric), .after=last_col())

write.csv(protest, "cnhkmo_protest_data_RG.csv")

