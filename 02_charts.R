library(tidyverse)
library(plotly)
library(widgetframe)
library(scales)
library(zoo)
library(lubridate)
options(scipen = 999)

#bring in saved table from step 01
zipcompare <- readRDS("zipcompare.rds")


#first the dems ####

#pull out top 10 zips for the dems
dem10 <- zipcompare %>% 
  arrange(desc(demtotal)) %>% 
  head(10)

dem10

#reorder factor to allow for descending bars
dem10 <- dem10 %>%
  mutate(zipname = fct_reorder(zipname, demtotal)) 

#chart it out
d <- ggplot(dem10, aes(zipname, demtotal)) + geom_col(fill = "darkblue") + coord_flip() +
  theme_minimal()

d

#add extra elements to the chart and convert to ggplotly
d2 <- d + labs(title="Top DNC/DCCC zip codes",
               # subtitle = "A subtitle",
               caption = "Source: FEC",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=dollar) 

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu

#save as embeddable format
# htmlwidgets::saveWidget(frameableWidget(dd), 'demtopzip_plt.html')
htmlwidgets::saveWidget(frameableWidget(dd_nomenu), 'demtopzip_plt_nm.html')

#save as RDS object
saveRDS(dd_nomenu, "demtopzip_plt_nm.rds")


#now the republicans ################

#pull out top 10 zips for the gop
gop10 <- zipcompare %>% 
  arrange(desc(goptotal)) %>% 
  head(10)

gop10

#reorder factor to allow for descending bars
gop10 <- gop10 %>%
  mutate(zipname = fct_reorder(zipname, goptotal)) 

#chart it out
p <- ggplot(gop10, aes(zipname, goptotal)) + geom_col(fill = "darkred") + coord_flip() +
  theme_minimal()

p

#add titles and other extras
p2 <- p + labs(title="Top RNC/NRCC zip codes",
               # subtitle = "A subtitle",
               caption = "Source: FEC",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=dollar) 

p2

pp <- ggplotly(p2) 

pp

pp_nomenu <- pp %>% config(displayModeBar = FALSE)
pp_nomenu

#save as embeddable format
# htmlwidgets::saveWidget(frameableWidget(pp), 'goptopzip_plt.html')
htmlwidgets::saveWidget(frameableWidget(pp_nomenu), 'goptopzip_plt_nm.html')

#save as RDS object
saveRDS(pp_nomenu, "goptopzip_plt_nm.rds")

