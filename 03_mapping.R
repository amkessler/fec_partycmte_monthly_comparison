library(tidyverse)
library(janitor)
library(tidycensus)
library(tigris)
library(sf)
library(leaflet)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(widgetframe)
library(sp)
library(raster)
library(mapview)
library(scales)
library(viridis)
options(tigris_class = "sf")

#bring in comparison table created in previous step
zipcompare <- readRDS("zipcompare.rds")

head(zipcompare)
names(zipcompare)

zipcompare$GEOID <- zipcompare$zip_code

#remove any negative values
zipcompare <- zipcompare %>% 
  filter(demtotal >= 0,
         goptotal >= 0)

zipcompare <- zipcompare %>% 
  filter(!str_detect(GEOID, "^99"),
         !str_detect(GEOID, "^96"),
         !str_detect(GEOID, "^006"),
         !str_detect(GEOID, "^007"),
         !str_detect(GEOID, "^009")
           )




#### get zip code lat/long points from census gazeteer file
zip_points <- read_csv("zip_points.csv", 
                       col_types = cols(ALAND = col_skip(), 
                                        ALAND_SQMI = col_skip(), AWATER = col_skip(), 
                                        AWATER_SQMI = col_skip()))

colnames(zip_points) <- c("GEOID", "lat", "lon")

#join data
zip_map <- inner_join(zipcompare, zip_points)

zip_map$winner <- as.factor(zip_map$winner)


#add dollar formatting
zip_map$demdisplay <- dollar(zip_map$demtotal)
zip_map$gopdisplay <- dollar(zip_map$goptotal)



#### MAPPING POINTS ##### ---------------------------------------

factpal <- colorFactor(c("blue","red"), zip_map$winner)

#labels
labs1 <- lapply(seq(nrow(zip_map)), function(i) {
  paste0( '<p>', 'Zip code: ', '<strong>', zip_map[i, "GEOID"], '</strong></p>',
          '<p></p>', 
          "Democrats: ", zip_map[i, "demdisplay"],
          '<p></p>', 
          "Republicans: ", zip_map[i, "gopdisplay"]
  ) 
})

m1 <- leaflet(zip_map) %>% 
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, weight = .4,
             stroke = FALSE, fillOpacity = .25,
             radius = ~sqrt(advantage) * 300, 
             fillColor = ~factpal(winner),
             label = lapply(labs1, HTML)
  ) %>%
  addControl("RNC/NRCC vs. DNC/DCCC - Feb. individual contributions by zip code", position = "topright") 
# %>% 
#   setView(-96, 37.8, zoom=4) 

m1

#save to frameable file for CMS
htmlwidgets::saveWidget(frameableWidget(m1),'feb_dvsr_byzip_points.html')

# save as rds object
saveRDS(m1, "savedmap_zipcompare.rds")
