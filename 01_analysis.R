library(tidyverse)
library(janitor)
library(lubridate)
options(scipen = 999)


#republicans ####

rnc <- read_csv("rnc.csv")
nrcc <- read_csv("nrcc.csv")

gop_combined <- rbind(rnc, nrcc)

gop_combined %>% 
  count(entity_type)

gop_combined$date <- ymd(gop_combined$date)
gop_combined$month <- month(gop_combined$date)

gop_combined %>% 
  select(date) %>% 
  arrange(date)

gop_combined %>% 
  count(month)

gop_combined %>% 
  filter(month != 9)


#individual contribs top zips
topzips_gop <- gop_combined %>% 
  filter(entity_type == "IND") %>% 
  group_by(zip_code) %>% 
  summarise(sumtotal = sum(amount)) %>% 
  arrange(desc(sumtotal))

head(topzips_gop, 10)



#democrats ####

dnc <- read_csv("dnc.csv")
dccc <- read_csv("dccc.csv")


dnc$date <- ymd(dnc$date)
dnc$month <- month(dnc$date)

dnc %>% 
  select(date) %>% 
  arrange(desc(date))

dnc %>% 
  count(month)


dccc$date <- mdy(dccc$date)
dccc$month <- month(dccc$date)

dccc %>% 
  select(date) %>% 
  arrange(date)

dccc %>% 
  count(month)



dnc_byzip <- dnc %>% 
  filter(entity_type == "IND") %>% 
  group_by(zip_code) %>% 
  summarise(sumtotal = sum(amount)) %>% 
  arrange(desc(sumtotal))


dccc_by_zip <- dccc %>% 
  filter(flag_orgind == "IND") %>% 
  group_by(zip) %>% 
  summarise(sumtotal = sum(amount)) %>% 
  arrange(desc(sumtotal))

colnames(dccc_by_zip) <- c("zip_code", "sumtotal")

dd <- rbind(dnc_byzip, dccc_by_zip)

topzips_dem <- dd %>% 
  group_by(zip_code) %>% 
  summarise(sumtotal = sum(sumtotal)) %>% 
  arrange(desc(sumtotal))

head(topzips_dem, 10)

write.csv(topzips_gop, "topzips_gop.csv")
write.csv(topzips_dem, "topzips_dem.csv")


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
