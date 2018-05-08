#############################################
###Data wrangling code for final project##### 

library(installr) # install+load installr
library(tidyverse)
library(sf)
#library(rgeos)
#library(maptools)
library(ggplot2)
library(viridis)
#library(stats)
library(dplyr)
library(tidyr)
library(sf)
library(tidycensus)

Sys.getenv("CENSUS_API_KEY")
#census_api_key("7ec628d12f68cb9fce1d74c77ababf2078dbc766", overwrite = FALSE, install = TRUE)

#---------------------------Color palettes all here---------------------
# color palette for the maps
pal_blue5 <- c("#85c4c9", "#68abb8", "#4f90a6", "#3b738f", 
               "#2a5674")

pal_green5 = c("#d8f2ed", "#9fc4be", "#6b9993", "#3f736d", 
                      "#154f4a")

pal_ltgn5 <- c("#6dbc90", "#4da284", "#36877a", "#266b6e", 
               "#1d4f60")

pal_dkbl5 <- c("#7bbcb0", "#559c9e", "#3a7c89", "#235d72", 
               "#123f5a")

pal_blue6 = c("#154f4a", "#376b66", "#588782", "#7ea8a2", 
              "#a9ccc6", "#d8f2ed")

pal_blue11 = c("#d8f2ed",
               "#bfded8", "#a9ccc6", "#93bab4", "#7ea8a2",
               "#6b9993", "#588782", "#477872", "#376b66", 
               "#265c56", "#154f4a")

# Create color palette for communities on map
paletteContinuousC <- colorNumeric(palette = pal_blue11, domain = Community_dataset$Rate)
paletteContinuousN <- colorNumeric(palette = pal_blue11, domain = Neighborhood_dataset$Rate)

# ---------------------------All the map themes here--------------------
ggmapTheme<-function(base_size = 12) {
  theme(text = element_text(size = 12,face = "italic"),
        plot.title = element_text(size = 17,face = "bold",colour = "black", hjust = 0),
        plot.subtitle = element_text(size = 12, face = "italic", colour = "dark grey", hjust = 0),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
        panel.background = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major =  element_line(colour="white",size = rel(0.5)),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0,0,0,0), "lines")
  )
}

graphTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(size = 12,face="italic", colour = "#477371", hjust = 0),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("#477371", size = 0.1),
    panel.border = element_rect(colour = "#477371", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    axis.text = element_text(size=8),
    axis.title = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

plotTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(size = 12,face="italic", colour = "#477371", hjust = 0),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("#477371", size = 0.1),
    panel.border = element_rect(colour = "#477371", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.y = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    axis.ticks.y = element_line(color="grey70"),
    axis.text.x =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank())
}

mapTheme<-function(base_size = 12) {
  theme(text = element_text(size = 12,face = "italic"),
        plot.title = element_text(size = 17,face = "bold",colour = "black", hjust = 0),
        plot.subtitle = element_text(size = 12, face = "italic", colour = "dark grey", hjust = 0),
        plot.caption = element_text(size = 10, face = "italic", colour = "grey"),
        panel.background = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major =  element_line(colour="white",size = rel(0.5)),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0,0,0,0), "lines")
  )
}

# ---------------------------------Base map---------------------------------
parks <- st_read("forbasemap/Minneparks.shp")
roads <- st_read("forbasemap/primeroads.shp")
streets <- st_read("forbasemap/streets.shp")
water <- st_read("forbasemap/waterbody.shp")
minneapolis <- st_read("Shapefiles/CityBoundary_proj.shp")
boundary <- st_read("forbasemap/Boundary.shp")

baseMap <- ggplot() + 
  geom_sf(data=boundary, aes(), fill = "gray70", colour="darkgray", size = 0.5, alpha = 0.2) +
  geom_sf(data=streets, aes(), fill=NA, colour='white', size = 0.3,alpha=0.8) + 
  geom_sf(data=water, aes(), fill="white", colour = NA) +
  geom_sf(data=roads, aes(), fill=NA, colour='gray90', alpha = 0.8) +
  geom_sf(data=parks, aes(), fill="white", colour = NA, alpha = 0.9) +
  geom_sf(data=minneapolis, aes(), fill=NA, colour="darkgray", size = 0.3) +
  geom_sf(data=boundary, aes(), fill = NA, colour="darkgray", size = 0.5, alpha = 0.2) +
  ggmapTheme()

baseMap

# -------------------------historical data---------------------------------
###Read in files from Brown site that standardizes census tracts across time
# site = https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx
Std_70 <- read.csv("Standardized_1970.csv")
Std_70 <- filter(Std_70, state == "MN", county == "Hennepin County")
head(Std_70)
Std_70_select <- select(Std_70,TRTID10,tract,POP70,HU70,VAC70,OWN70,RENT70)
head(Std_70_select)
tractshp$GEOID <- as.numeric(tractshp$GEOID)
Min70 <- left_join(tractshp, Std_70_select, by=c("GEOID" = "TRTID10"))
head(Min70)


Std_80 <- read.csv("Standardized_1980.csv")
Std_80 <- filter(Std_80, state == "MN", county == "Hennepin County")
head(Std_80)
Std_80_select <- select(Std_80,TRTID10,tract,POP80,HU80,VAC80,MHMVAL80,MRENT80,OWN80,RENT80)
head(Std_80_select)
Min80 <- left_join(tractshp, Std_80_select, by=c("GEOID" = "TRTID10"))
head(Min80)

Std_90 <- read.csv("Standardized_1990.csv")
Std_90 <- filter(Std_90, state == "MN", county == "Hennepin County")
head(Std_90)
Std_90_select <- select(Std_90,TRTID10,tract,POP90,HU90,VAC90,MHMVAL90,MRENT90, OWN90,RENT90)
head(Std_90_select)
Min90 <- left_join(tractshp, Std_90_select, by=c("GEOID" = "TRTID10"))
head(Min90)

Std_00 <- read.csv("Standardized_2000.csv")
Std_00 <- filter(Std_00, state == "MN", county == "Hennepin County")
head(Std_00)
Std_00_select <- select(Std_00,TRTID10,tract,POP00,HU00,VAC00,FAMILY00,FHH00, OWN00,RENT00)
head(Std_00_select)
Min00 <- left_join(tractshp, Std_00_select, by=c("GEOID" = "TRTID10"))
head(Min00)

Std_10 <- read.csv("Standardized_2010.csv")
Std_10 <- filter(Std_10, state == "MN", county == "Hennepin County")
head(Std_10)
Std_10_select <- select(Std_10,tractid,tract,pop10,hu10,vac10,family10,fhh10, own10,rent10)
head(Std_10_select)
Min10 <- left_join(tractshp, Std_10_select, by=c("GEOID" = "tractid"))
head(Min10)
v2010 <- get_acs(geography = "tract", variables = "B06011_001", state = "MN", county = "Hennepin", year = 2012)
head(v2010)
v2010 <- select(v2010, GEOID, estimate)
v2010$GEOID <- as.numeric(v2010$GEOID)
Min10 <- left_join(Min10, v2010, by=c("GEOID" = "GEOID"))
Std_10_select <- left_join(Std_10_select, v2010, by=c("tractid" = "GEOID"))
Std_10_select <- rename(Std_10_select, MEDINC = estimate)
head(Std_10_select)

### Join all variables together into one dataset. separate column for each variable in each yr.
TractInfo <- left_join(Std_70_select, Std_80_select, by=c("TRTID10" = "TRTID10"))
TractInfo <- left_join(TractInfo, Std_90_select, by=c("TRTID10" = "TRTID10"))
TractInfo <- left_join(TractInfo, Std_00_select, by=c("TRTID10" = "TRTID10"))
TractInfo <- left_join(TractInfo, Std_10_select, by=c("TRTID10" = "tractid"))
Tract16$GEOID <- as.numeric(Tract16$GEOID)
TractInfo <- left_join(TractInfo, Tract16, by=c("TRTID10" = "GEOID"))

write.csv(TractInfo, "HistoricalCensus.csv")

# LOAD IN HISTORICAL CENSUS TRACT DATA ------------------------------------
data <- read.csv("HistoricalCensus.csv")
data_subset <- dplyr::select(data, TRTID10, POP70, VAC70, OWN70, RENT70, 
                      POP80, VAC80, OWN80, RENT80, 
                      POP90, VAC90, OWN90, RENT90,
                      POP00, VAC00, OWN00, RENT00, 
                      pop10, vac10, own10, rent10)

# Load in shapefiles  -----------------------------------------------------
TractsMN <- st_read("CensusTracts_2016_clip_proj.shp")
Communities <- read_sf("Communities_proj.shp")
Neighborhoods <- read_sf("Neighborhoods_proj_w_comm.shp")
head(TractsMN)

#-----------------------------------Map of Population change-----------------------------#

# 1970 Population map by census tract
datpop70 <- dplyr::select(data_subset, TRTID10, POP70) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmappop70 <- right_join(TractsMN,datpop70,by=c("GEOID" = "TRTID10"))

datmappop70$POP70Buckets <- factor(cut(datmappop70$POP70, c(100,1000,2500,3500,7000,12500)),labels = c("labels"))
na.omit(datmappop70)

baseMap +
  geom_sf(data = datmappop70, aes(fill = datmappop70$POP70Buckets), color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_green5, 
                     labels=c("100-800",
                              "800-2000", 
                              "2000-4000",
                              "4000-7000",
                              "7000-13000"),
                     name = "Population \nby Census Tract")+
  labs(title= "1970 Population by Census Tract in Minneapolis",
       subtitle= "Choropleth of Population by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1980 Population map by census tract
datpop80 <- dplyr::select(data_subset, TRTID10, POP80) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmappop80 <- right_join(TractsMN,datpop80,by=c("GEOID" = "TRTID10"))

datmappop80$POP80Buckets <- factor(cut(datmappop80$POP80, c(100,1000,2500,3500,7000,12500)),labels = c("labels"))
na.omit(datmappop80)

baseMap +
  geom_sf(data = datmappop80, aes(fill = datmappop80$POP80Buckets), color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_green5, 
                    labels=c("100-800",
                             "800-2000", 
                             "2000-4000",
                             "4000-7000",
                             "7000-13000"),
                    name = "Population \nby Census Tract")+
  labs(title= "1980 Population by Census Tract in Minneapolis",
       subtitle= "Choropleth of Population by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1990 Population map by census tract
datpop90 <- dplyr::select(data_subset, TRTID10, POP90) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmappop90 <- right_join(TractsMN,datpop90,by=c("GEOID" = "TRTID10"))

datmappop90$POP90Buckets <- factor(cut(datmappop90$POP90, c(100,1000,2500,3500,7000,12500)),labels = c("labels"))
na.omit(datmappop80)

baseMap +
  geom_sf(data = datmappop90, aes(fill = datmappop90$POP90Buckets), color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_green5, 
                    labels=c("100-800",
                             "800-2000", 
                             "2000-4000",
                             "4000-7000",
                             "7000-13000"),
                    name = "Population \nby Census Tract")+
  labs(title= "1990 Population by Census Tract in Minneapolis",
       subtitle= "Choropleth of Population by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2000 Population map by census tract
datpop00 <- dplyr::select(data_subset, TRTID10, POP00) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmappop00 <- right_join(TractsMN,datpop00,by=c("GEOID" = "TRTID10"))

datmappop00$POP00Buckets <- factor(cut(datmappop00$POP00, c(100,1000,2500,3500,7000,12500)),labels = c("labels"))
na.omit(datmappop00)

baseMap +
  geom_sf(data = datmappop00, aes(fill = datmappop00$POP00Buckets), color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_green5, 
                    labels=c("100-800",
                             "800-2000", 
                             "2000-4000",
                             "4000-7000",
                             "7000-13000"),
                    name = "Population \nby Census Tract")+
  labs(title= "2000 Population by Census Tract in Minneapolis",
       subtitle= "Choropleth of Population by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2010 Population map by census tract
datpop10 <- dplyr::select(data_subset, TRTID10, pop10) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmappop10 <- right_join(TractsMN,datpop10,by=c("GEOID" = "TRTID10"))

datmappop10$POP10Buckets <- factor(cut(datmappop10$pop10, c(100,1000,2500,3500,7000,12500)),labels = c("labels"))
na.omit(datmappop10)

baseMap +
  geom_sf(data = datmappop10, aes(fill = datmappop10$POP10Buckets), color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_green5, 
                    labels=c("100-800",
                             "800-2000", 
                             "2000-4000",
                             "4000-7000",
                             "7000-13000"),
                    name = "Population \nby Census Tract")+
  labs(title= "2010 Population by Census Tract in Minneapolis",
       subtitle= "Choropleth of Population by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

#-----------------------------------Map of Vacancy change-----------------------------#
# 1970 Vacancy map by census tract
datvac70 <- dplyr::select(data_subset, TRTID10, VAC70) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapvac70 <- right_join(TractsMN,
                          datvac70,
                          by=c("GEOID" = "TRTID10"))

datmapvac70$vac70Buckets <- factor(cut(datmapvac70$VAC70, 
                                       c(0,20,50,90,150,500)),
                                   labels = c("labels"))
na.omit(datmapvac70)

baseMap +
  geom_sf(data = datmapvac70, 
          aes(fill = datmapvac70$vac70Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_blue5, 
                    labels=c("0-20",
                             "20-50", 
                             "50-90",
                             "90-150",
                             "150-500"),
                    name = "Vacancy \nby Census Tract")+
  labs(title= "1970 Vacancy by Census Tract in Minneapolis",
       subtitle= "Choropleth of vacancy by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1980 Vacancy map by census tract
datvac80 <- dplyr::select(data_subset, TRTID10, VAC80) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapvac80 <- right_join(TractsMN,
                          datvac80,
                          by=c("GEOID" = "TRTID10"))

datmapvac80$vac80Buckets <- factor(cut(datmapvac80$VAC80, 
                                       c(0,20,50,90,150,500)),
                                   labels = c("labels"))
na.omit(datmapvac80)

baseMap +
  geom_sf(data = datmapvac80, 
          aes(fill = datmapvac80$vac80Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_blue5, 
                    labels=c("0-20",
                             "20-50", 
                             "50-90",
                             "90-150",
                             "150-500"),
                    name = "Vacancy \nby Census Tract")+
  labs(title= "1980 Vacancy by Census Tract in Minneapolis",
       subtitle= "Choropleth of vacancy by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1990 Vacancy map by census tract
datvac90 <- dplyr::select(data_subset, TRTID10, VAC90) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapvac90 <- right_join(TractsMN,
                          datvac90,
                          by=c("GEOID" = "TRTID10"))

datmapvac90$vac90Buckets <- factor(cut(datmapvac90$VAC90, 
                                       c(0,20,50,90,150,500)),
                                   labels = c("labels"))
na.omit(datmapvac90)

baseMap +
  geom_sf(data = datmapvac90, 
          aes(fill = datmapvac90$vac90Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_blue5, 
                    labels=c("0-20",
                             "20-50", 
                             "50-90",
                             "90-150",
                             "150-500"),
                    name = "Vacancy \nby Census Tract")+
  labs(title= "1990 Vacancy by Census Tract in Minneapolis",
       subtitle= "Choropleth of vacancy by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2000 Vacancy map by census tract
datvac00 <- dplyr::select(data_subset, TRTID10, VAC00) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapvac00 <- right_join(TractsMN,
                          datvac00,
                          by=c("GEOID" = "TRTID10"))

datmapvac00$vac00Buckets <- factor(cut(datmapvac00$VAC00, 
                                       c(0,20,50,90,150,500)),
                                   labels = c("labels"))
na.omit(datmapvac00)

baseMap +
  geom_sf(data = datmapvac00, 
          aes(fill = datmapvac00$vac00Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_blue5, 
                    labels=c("0-20",
                             "20-50", 
                             "50-90",
                             "90-150",
                             "150-500"),
                    name = "Vacancy \nby Census Tract")+
  labs(title= "2000 Vacancy by Census Tract in Minneapolis",
       subtitle= "Choropleth of vacancy by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2010 Vacancy map by census tract
datvac10 <- dplyr::select(data_subset, TRTID10, vac10) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapvac10 <- right_join(TractsMN,
                          datvac10,
                          by=c("GEOID" = "TRTID10"))

datmapvac10$vac10Buckets <- factor(cut(datmapvac10$vac10, 
                                       c(0,20,50,90,150,500)),
                                   labels = c("labels"))
na.omit(datmapvac10)

baseMap +
  geom_sf(data = datmapvac10, 
          aes(fill = datmapvac10$vac10Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_blue5, 
                    labels=c("0-20",
                             "20-50", 
                             "50-90",
                             "90-150",
                             "150-500"),
                    name = "Vacancy \nby Census Tract")+
  labs(title= "2010 Vacancy by Census Tract in Minneapolis",
       subtitle= "Choropleth of vacancy by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

#-----------------------------------Map of Rental change-----------------------------#
# 1970 Rental map by census tract
datrent70 <- dplyr::select(data_subset, TRTID10, RENT70) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmaprent70 <- right_join(TractsMN,
                          datrent70,
                          by=c("GEOID" = "TRTID10"))

datmaprent70$rent70Buckets <- factor(cut(datmaprent70$RENT70, 
                                       c(0,250,650,1000,1500,3000)),
                                   labels = c("labels"))
na.omit(datmaprent70)

baseMap +
  geom_sf(data = datmaprent70, 
          aes(fill = datmaprent70$rent70Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_ltgn5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Rental House \nby Census Tract")+
  labs(title= "1970 Number of Rental House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Rental House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1980 Rental map by census tract
datrent80 <- dplyr::select(data_subset, TRTID10, RENT80) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmaprent80 <- right_join(TractsMN,
                           datrent80,
                           by=c("GEOID" = "TRTID10"))

datmaprent80$rent80Buckets <- factor(cut(datmaprent80$RENT80, 
                                         c(0,250,650,1000,1500,3000)),
                                     labels = c("labels"))
na.omit(datmaprent80)

baseMap +
  geom_sf(data = datmaprent80, 
          aes(fill = datmaprent80$rent80Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_ltgn5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Rental House \nby Census Tract")+
  labs(title= "1980 Number of Rental House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Rental House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1990 Rental map by census tract
datrent90 <- dplyr::select(data_subset, TRTID10, RENT90) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmaprent90 <- right_join(TractsMN,
                           datrent90,
                           by=c("GEOID" = "TRTID10"))

datmaprent90$rent90Buckets <- factor(cut(datmaprent90$RENT90, 
                                         c(0,250,650,1000,1500,3000)),
                                     labels = c("labels"))
na.omit(datmaprent90)

baseMap +
  geom_sf(data = datmaprent90, 
          aes(fill = datmaprent90$rent90Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_ltgn5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Rental House \nby Census Tract")+
  labs(title= "1990 Number of Rental House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Rental House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2000 Rental map by census tract
datrent00 <- dplyr::select(data_subset, TRTID10, RENT00) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmaprent00 <- right_join(TractsMN,
                           datrent00,
                           by=c("GEOID" = "TRTID10"))

datmaprent00$rent00Buckets <- factor(cut(datmaprent00$RENT00, 
                                         c(0,250,650,1000,1500,3000)),
                                     labels = c("labels"))
na.omit(datmaprent00)

baseMap +
  geom_sf(data = datmaprent00, 
          aes(fill = datmaprent00$rent00Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_ltgn5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Rental House \nby Census Tract")+
  labs(title= "2000 Number of Rental House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Rental House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2010 Rental map by census tract
datrent10 <- dplyr::select(data_subset, TRTID10, rent10) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmaprent10 <- right_join(TractsMN,
                           datrent10,
                           by=c("GEOID" = "TRTID10"))

datmaprent10$rent10Buckets <- factor(cut(datmaprent10$rent10, 
                                         c(0,250,650,1000,1500,3000)),
                                     labels = c("labels"))
na.omit(datmaprent10)

baseMap +
  geom_sf(data = datmaprent10, 
          aes(fill = datmaprent10$rent10Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_ltgn5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Rental House \nby Census Tract")+
  labs(title= "2010 Number of Rental House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Rental House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

#-----------------------------------Map of Ownership change-----------------------------#
# 1970 ownership map by census tract
datown70 <- dplyr::select(data_subset, TRTID10, OWN70) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapown70 <- right_join(TractsMN,
                          datown70,
                           by=c("GEOID" = "TRTID10"))

datmapown70$own70Buckets <- factor(cut(datmapown70$OWN70, 
                                         c(0,125,225,450,850,3800)),
                                     labels = c("labels"))
na.omit(datmapown70)

baseMap +
  geom_sf(data = datmapown70, 
          aes(fill = datmapown70$own70Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_dkbl5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Owner House \nby Census Tract")+
  labs(title= "1970 Number of Owner House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Owner House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1980 ownership map by census tract
datown80 <- dplyr::select(data_subset, TRTID10, OWN80) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapown80 <- right_join(TractsMN,
                          datown80,
                          by=c("GEOID" = "TRTID10"))

datmapown80$own80Buckets <- factor(cut(datmapown80$OWN80, 
                                       c(0,125,225,450,850,3800)),
                                   labels = c("labels"))
na.omit(datmapown80)

baseMap +
  geom_sf(data = datmapown80, 
          aes(fill = datmapown80$own80Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_dkbl5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Owner House \nby Census Tract")+
  labs(title= "1980 Number of Owner House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Owner House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 1990 ownership map by census tract
datown90 <- dplyr::select(data_subset, TRTID10, OWN90) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapown90 <- right_join(TractsMN,
                          datown90,
                          by=c("GEOID" = "TRTID10"))

datmapown90$own90Buckets <- factor(cut(datmapown90$OWN90, 
                                       c(0,125,225,450,850,3800)),
                                   labels = c("labels"))
na.omit(datmapown90)

baseMap +
  geom_sf(data = datmapown90, 
          aes(fill = datmapown90$own90Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_dkbl5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Owner House \nby Census Tract")+
  labs(title= "1990 Number of Owner House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Owner House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2000 ownership map by census tract
datown00 <- dplyr::select(data_subset, TRTID10, OWN00) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapown00 <- right_join(TractsMN,
                          datown00,
                          by=c("GEOID" = "TRTID10"))

datmapown00$own00Buckets <- factor(cut(datmapown00$OWN00, 
                                       c(0,125,225,450,850,3800)),
                                   labels = c("labels"))
na.omit(datmapown00)

baseMap +
  geom_sf(data = datmapown00, 
          aes(fill = datmapown00$own00Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_dkbl5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Owner House \nby Census Tract")+
  labs(title= "2000 Number of Owner House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Owner House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()

# 2010 ownership map by census tract
datown10 <- dplyr::select(data_subset, TRTID10, own10) %>%
  as.data.frame() %>%
  mutate(TRTID10=as.factor(TRTID10))

datmapown10 <- right_join(TractsMN,
                          datown10,
                          by=c("GEOID" = "TRTID10"))

datmapown10$own10Buckets <- factor(cut(datmapown10$own10, 
                                       c(0,125,225,450,850,3800)),
                                   labels = c("labels"))
na.omit(datmapown10)

baseMap +
  geom_sf(data = datmapown10, 
          aes(fill = datmapown10$own10Buckets), 
          color = 'white', alpha = 0.8) +
  scale_fill_manual(values = pal_dkbl5, 
                    labels=c("0-250",
                             "250-650", 
                             "650-1000",
                             "1000-1500",
                             "1500-3000"),
                    name = "Owner House \nby Census Tract")+
  labs(title= "2010 Number of Owner House by Census Tract in Minneapolis",
       subtitle= "Choropleth of Owner House by Census Tract",
       caption = "Data Source:\n Spatial Structures in Social Sciences, Brown University") +
  ggmapTheme()


