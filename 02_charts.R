library(tidyverse)
library(plotly)
library(widgetframe)
library(scales)
library(zoo)
library(lubridate)
options(scipen = 999)

#bring in data
dem10 <- read_csv("top zips dems with city names.csv", 
                                          col_types = cols(zip_code = col_character()))

gop10 <- read_csv("top zips gop with city names.csv")


dem10 <- dem10 %>% 
  mutate(
    zipname = paste0(zip_code, " (", name, ")")
  )

gop10 <- gop10 %>% 
  mutate(
    zipname = paste0(zip_code, " (", name, ")")
  )


#first the dems

df <- dem10
df <- arrange(df, total)
df$zipname <- factor(df$zipname, levels = df$zipname)

d <- ggplot(df, aes(zipname, total)) + geom_col(fill = "darkblue") + coord_flip() +
  theme_minimal()

d


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



#now the republicans ##############################

df2 <- gop10
df2 <- arrange(df2, total)
df2$zipname <- factor(df2$zipname, levels = df2$zipname)

p <- ggplot(df2, aes(zipname, total)) + geom_col(fill = "darkred") + coord_flip() +
     theme_minimal()

p


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
htmlwidgets::saveWidget(frameableWidget(pp), 'goptopzip_plt.html')
htmlwidgets::saveWidget(frameableWidget(pp_nomenu), 'goptopzip_plt_nm.html')

