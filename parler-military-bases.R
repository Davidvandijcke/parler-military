# load packages

packages_load <- c("data.table", "dplyr", "ggplot2", "lubridate",  'stringr', # not all these are necessary but dont remember which arent
                    'foreach', 'zoo', 'sf', "purrr", 'tidyverse', 'geosphere', 'maps', 'extrafont', 'sp', "here")
{
  sink("/dev/null") # load packages but suppress output
  lapply(c(packages_load),  function(x) {if (!require(x, character.only=T)) {install.packages(x, dependencies=TRUE)}
    
  lapply(packages_load, library, character.only = TRUE) })

  sink()
}

# set cd to program directory
dir <- here::here()
setwd(dir)
  

intersectParler <- function(cbg_stacked, parler, split = 100, id = "geoid") { 
  
  # """ function to intersect parler data with shapefile """
  # ARGUMENTS
  # * cbg_stacked: shapefile to intersect with
  # * parler: parler data
  # * split: how many batches to split data in (intersecting all at once maxes out my 24GB RAM)
  # * id: id that identifies the geography of interest in cbg_stacked
  
  # long = x, lat = y
  split <- split
  pnts_full <- parler[,c("Longitude", "Latitude")]
  pnts_full <- split(pnts_full, rep(1:split, each = dim(pnts_full)[1]/split ))
  
  pnts_out <- data.table(Longitude = numeric(), Latitude = numeric(), id = character())
  for (i in 1:split) {
    
    pnts <- pnts_full[[i]]
    pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts),
                                         function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326)))
    pnts_trans <- st_transform(pnts_sf, 2163)
    
    intersected <- apply(st_intersects(cbg_stacked, pnts_trans, sparse = FALSE), 2,
                         function(col) {
                           cbg_stacked[which(col), ][[id]]
                         })
    pnts$id <- unlist(lapply(intersected, 
                             function(x) if(identical(x,character(0))) NA else x))
      
      
    if (length(intersected) == 0)
      pnts$id <- NA
    pnts_out <- rbind(pnts_out, pnts)
    
  }
  return(pnts_out)
}


# get parler data
parler <- fread(file.path(dir, 'data', 'parler-videos-geocoded.csv'))


# get military bases shapefile
military <- read_sf(file.path(dir, 'data', 'tl_2019_us_mil')) %>% 
  st_transform(2163) %>%
  set_names(colnames(.) %>% str_to_lower())

# intersect
mil_out <- intersectParler(military, parler, split = 50, id = "areaid") 

parler_mil <- cbind(parler[,c("Timestamp", "ID")], mil_out)

# write to file
fwrite(parler_mil, file.path(dir, 'data', 'parler-military-intersected.csv.gz'))

# only the ones from military bases
parler_mil_true <- parler_mil[!is.na(id)]

fwrite(parler_mil_true, file.path(dir, 'data', 'parler-from-military.csv'))

