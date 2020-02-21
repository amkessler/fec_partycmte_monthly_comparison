library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
options(scipen = 999)


# REPUBLICANS ####
#point to the files from the relevant reporting period directory you're interested in

rnc <- read_excel("feb2020reports/rnc.xlsx")
nrcc <- read_excel("feb2020reports/nrcc.xlsx")

gop_combined <- rbind(rnc, nrcc)

gop_combined %>% 
  count(flag_orgind)

# gop_combined$date <- ymd(gop_combined$date)
gop_combined$month <- month(gop_combined$date)

gop_combined %>% 
  select(date) %>% 
  arrange(date)

gop_combined %>% 
  count(month)

gop_combined %>% 
  filter(month == 1)


#individual contribs top zips
topzips_gop <- gop_combined %>% 
  filter(flag_orgind == "IND") %>% 
  group_by(zip) %>% 
  summarise(sumtotal = sum(amount)) %>% 
  arrange(desc(sumtotal))

head(topzips_gop, 10)



# DEMOCRATS ####
#point to the files from the relevant reporting period directory you're interested in

dnc <- read_excel("feb2020reports/dnc.xlsx")

dccc <- read_excel("feb2020reports/dccc.xlsx", 
                   col_types = c("text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "date", "numeric", 
                                "numeric", "text", "text", "numeric", 
                                "text", "numeric"))


dem_combined <- rbind(dnc, dccc)

dem_combined %>% 
  count(flag_orgind)

# dem_combined$date <- ymd(dem_combined$date)
dem_combined$month <- month(dem_combined$date)

dem_combined %>% 
  select(date) %>% 
  arrange(date)

dem_combined %>% 
  count(month)

dem_combined %>% 
  filter(month == 1)

#individual contribs top zips
topzips_dem <- dem_combined %>% 
  filter(flag_orgind == "IND") %>% 
  group_by(zip) %>% 
  summarise(sumtotal = sum(amount)) %>% 
  arrange(desc(sumtotal))

head(topzips_dem, 10)

#write to file
write_csv(topzips_gop, "topzips_gop.csv")
write_csv(topzips_dem, "topzips_dem.csv")


### compare D and R in each zip

colnames(topzips_dem) <- c("zip_code", "demtotal")
colnames(topzips_gop) <- c("zip_code", "goptotal")

#join
zipcompare <- full_join(topzips_dem, topzips_gop)

#change nas to 0
zipcompare <- zipcompare %>% 
  replace(., is.na(.), 0)

#party with more
zipcompare <- zipcompare %>% 
  mutate(
    winner = ifelse(demtotal>goptotal, "D", "R"),
    advantage = abs(demtotal-goptotal)  
  )

saveRDS(zipcompare, "zipcompare.rds")
