###############################################
### Modeling code for model behind the app##### 

library(jsonlite)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(devtools)
library(tidyverse)
library(rasterVis)
library(spatstat)
library(esri2sf)
library(geosphere)
library(dplyr)
library(ggplot2)
library(FNN)
library(spdep)
library(maptools)
library(tidycensus)
library(caTools)
library(Boruta)
library(xgboost)
library(rpart)
library(caret)
library(plotROC)
library(Metrics)
library(QuantPsyc)
library(ggplot2)
library(gridExtra)
library(heuristica)
library(InformationValue)
library(Ckmeans.1d.dp)
library(aod)
library(rms)
library(gmodels)
library(nnet)
library(DAAG)
library(ROCR)
library(corrplot)

options(scipen = 999)

#---------------------------Map themes all here---------------------
# themes for the maps

plotTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
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

mapTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "#ffffff")
  )
}

#---------------------------Color themes all here---------------------
# color palette for the maps

paletteChoropleth <- c("#CFF09E","#A8DBA8","#79BD9A","#3B8686", "#0B486B")

paletteLead <- c("#477371","#FFFF00")
paletteCorrect <- c("#71F229","#E81434")
paletteClassified <- c("#FFCC07","#2C4A54")
paletteConfuse <- c("#65BC7A","#490A3D","#F56991","#FA6900")
palette2 <- c("#F56991","#FA6900","#490A3D")

#---------------------------Data import  all here---------------------
# data for the modeling
minneaParcels <- sf::st_read("Shapefiles/forR_test.shp")
minneaHOLC <- st_read("Shapefiles/HOLC_Minneapolis.shp")
tractshp <- read_sf("Shapefiles/CensusTracts_2016_clip_proj.shp")

#---------------------------Data wrangling starts here----------------
# data for the modeling
Minnea1 <- st_join(minneaParcels %>% st_transform(4326), minneaHOLC)

Minnea2 <- dplyr::select(Minnea1, PID, BUILD_YR, SALE_DATE, SALE_PRICE,TOT_PENALT, 
                         EARLIEST_D, PR_TYP_NM1, OWNER_PCT1,QUAL_IMPR1,Lead_Hazar, PID_1, holc_grade)

newPID <- as.character(Minnea2$PID_1)
inter <- as.data.frame(newPID)
testedParcels_PID <- na.exclude(inter$newPID)
new <- as.data.frame(testedParcels_PID)

Minnea3 <- as.data.frame(Minnea2)
finalTested <- left_join(new, Minnea3, by=c("testedParcels_PID" = "PID_1"))
Parcels <- st_as_sf(finalTested)

minneapolis <- st_read("Shapefiles/CityBoundary_proj.shp")
minneapolis_roads <-st_read("Shapefiles/Streets_proj_clip.shp")
minneapolis_water <- st_read("Shapefiles/water_proj_clip.shp")
Complaints <- st_read("Shapefiles/311_Public_2017_proj.shp") %>% st_transform(26851)
ResComplaints <- Complaints %>%
  filter(TYPENAME == "Residential Conditions Complaint Tenant" | TYPENAME == "Residential Conditions Complaint")
land <- st_read("Shapefiles/AssessorLandCharacteristics2017_proj.shp")
parcels_land_building <- sf::st_read("Shapefiles/parcels_land_building.shp") %>% st_transform(26851)
building <- st_read("Shapefiles/AssessorBuildingCharacteristics_2017_proj.shp")  %>% st_transform(26851)
parcels_all_lead <- left_join(as.data.frame(parcels_land_building), as.data.frame(Parcels), by="PID") 

baseMap <- ggplot() + 
  #geom_sf(data=minneaGrid, aes(), fill=NA, colour="darkgreen") +
  geom_sf(data=minneapolis_roads %>% filter(SPEED_LIM > 40), aes(), fill=NA, colour='grey70') + 
  geom_sf(data=minneapolis_water, aes(), fill="skyblue", colour = NA) +
  geom_sf(data=minneapolis, aes(), fill=NA, colour="black", size = 1.5) +
  labs(title= "Minneapolis") +
  mapTheme()

# Take out the lead data
lead <- Parcels %>% filter(Lead_Hazar == 1)

##Sale Data - all 
saleData <- read_csv("SpreadsheetData/saleData.csv")
saleData <- rename(saleData, PID = apn)

#Foreclosure
saleDataForeclosure <- saleData %>% filter(saleData$saleType == "Forclse/Divrce/Bnkrpt") %>% group_by(PID)

SDF1 <- saleDataForeclosure %>% 
  group_by(PID) %>% 
  dplyr::summarize(count=n())

SDF <- aggregate(saleType ~ PID, data = saleDataForeclosure, c)
SDF <- as.data.frame(SDF)
SDF$apn <- as.character(SDF$PID)

Minnea3$PID <- as.character(Minnea3$PID)
Foreclosure <- left_join(SDF, Minnea3, by=c("apn" = "PID"))
foreclosureSites <- Foreclosure %>% st_as_sf() %>% st_transform(26851)
foreclosureSites <- foreclosureSites %>% filter(SALE_PRICE != "N/A")

#Sale Data # see the structure of dataset
unlist(lapply(saleData, class))# see the structure of dataset

saleData_date <- separate(saleData, saleDate, c("saleyear", "salemonth", "saleday"))

monthly_cpi <-
  read.csv("SpreadsheetData/CPIAUCSL.csv", header = TRUE)
monthly_cpi$cpi_year <- lubridate::year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% dplyr::summarize(cpi = mean(CPIAUCSL))
yearly_cpi$adj_factor <- (yearly_cpi$cpi[yearly_cpi$cpi_year == 2017])/yearly_cpi$cpi

yearly_cpi <- yearly_cpi %>% dplyr::filter(cpi_year > 1982) %>% dplyr::filter(cpi_year < 2018)

# change data types
newsaleData <-
  saleData_date %>%
  mutate(PID = PID,
         saleyear = as.numeric(saleyear),
         salemonth = as.numeric(salemonth),
         saleday = as.numeric(saleday),
         salePrice = as.numeric(salePrice),
         saleType = as.factor(saleType)) %>%
  dplyr::select(PID,saleyear, salemonth,salePrice, saleType) %>%
  as.data.frame()

newsaleData1 <- left_join(newsaleData, yearly_cpi, by=c("saleyear"="cpi_year"))

newsaleData1$realprice <- (newsaleData1$salePrice * newsaleData1$adj_factor)

# Total number of sales times
salecount1 <- newsaleData1 %>%  
  group_by(PID) %>%
  dplyr::summarize(salecountTotal=n()) %>%
  as.data.frame() 

# Highest historical price
saleprice_max <- newsaleData1 %>%  
  group_by(PID) %>%
  dplyr::summarize(price_max=max(realprice), price_min=min(realprice)) %>% 
  dplyr::mutate(pricechange=price_max-price_min) %>% 
  as.data.frame() 

# latest sale year
yearsold <- newsaleData1 %>%  
  group_by(PID) %>%
  dplyr::summarize(yearsold=max(saleyear)) %>%
  as.data.frame()

# number of years since last trascation
yearssincesold <- yearsold %>%  
  mutate(yearsincesold=(2018-yearsold))%>%
  as.data.frame()  

MNSalesData <- left_join(yearssincesold, saleprice_max, by=c("PID" = "PID"))
MNSalesData <- left_join(MNSalesData, salecount1, by=c("PID" = "PID"))

#Valuation
Valuation <- read.csv("SpreadsheetData/valuationData.csv", header = F) 
colnames(Valuation) <- c("apn", "year", "homesteadPct", "buildingValue",
                         "landValue", "machineryValue", "totalValue", "taxableValue")
Valuation <- dplyr::select(Valuation, apn, year, totalValue)

#  valuation - change data type
Valuation2 <- Valuation %>% 
  dplyr::mutate(year = as.numeric(year),
                totalValue = as.numeric(totalValue)) %>%
  dplyr::select(apn,year, totalValue) %>%
  as.data.frame()

# create a combine field to join with the correct PID
#newsaleData$combField <- paste0(newsaleData$salePrice,newsaleData$saleType)
Valuation2 <- rename(Valuation2,PID =apn)

# Total number of valuation time
Valuation_tojoin <- Valuation2 %>%  
  group_by(PID) %>%
  dplyr::summarize(totalValue = max(totalValue)) %>%
  as.data.frame()
class(Valuation_tojoin$PID) # (Valuation_tojoin) [Maureen]


#Permit data
permits <- read.csv("SpreadsheetData/permits.csv", header = FALSE)
permits <- dplyr::rename(permits, apn = V1, permitNumber = V2, issueDate = V3, completeDate = V4, permitType = V5, permitTypeDescription = V6, decision = V7, status = V8, value = V9)

#new column for whether parcel had relevant permit for "residential building", "building", "wrecking/moving"
sort(table(permits$permitTypeDescription))
permits <- mutate(permits, PermitLead = ifelse(permitTypeDescription == "Residential Building Permit", 1, 
                                               ifelse(permitTypeDescription == "BUILDING", 1,
                                                      ifelse(permitTypeDescription =="WRECKING/MOVING", 1,
                                                             ifelse(permitTypeDescription == "Wrecking Permit", 1, 0)))))
permits <- dplyr::select(permits, apn, PermitLead)

#count of total relevant permits per parcel
permitCount <- permits %>%  
  group_by(apn) %>% 
  dplyr::summarize(PermitCountTotal=n())
permitCount2 <- permits %>% 
  group_by(apn) %>%
  dplyr::summarise(PermitCountLead= sum(PermitLead == "1"))      #[data prep and joining here]


#Land and Building Characteristics
parcels_land_building <- sf::st_read("Shapefiles/parcels_land_building.shp") %>% st_transform(26851)
building <- st_read("Shapefiles/AssessorBuildingCharacteristics_2017_proj.shp")  %>% st_transform(26851)
#land <- st_read("AssessorLandCharacteristics2017_proj.shp") %>% st_transform(26851)
parcels_all_lead <- left_join(as.data.frame(parcels_land_building), as.data.frame(Parcels), by="PID")

parcels_all_lead$Lead_Hazar[is.na(parcels_all_lead$Lead_Hazar)] <- 2

parcels_all_lead$Lead_Hazar <- as.factor(parcels_all_lead$Lead_Hazar)
VacantLand <- land %>% filter(LandUse == "VACANT LAND")


minnea <- readOGR("Shapefiles/CityBoundary_proj.shp")
grid <- raster(extent(minnea))
res(grid) <- 500
proj4string(grid)<- proj4string(minnea)
gridpolygon <- rasterToPolygons(grid)
test_grid <- raster::intersect(minnea, gridpolygon)
plot(test_grid)
minneaGrid <- st_as_sf(test_grid)

minneaWindow <- owin(c((2798377/5280),(2833409/5280)), c((1018085/5280),(1076634/5280)))

leadX <- (st_coordinates(lead)[,1])/5280
leadY <- (st_coordinates(lead)[,2])/5280
leadDens <- cbind(leadX, leadY)
leadDens1 <- as.data.frame(leadDens)
lead.points.sf <- st_as_sf(x = leadDens1, coords = 1:2)

vacantX <- (st_coordinates(VacantLand)[,1])/5280
vacantY <- (st_coordinates(VacantLand)[,2])/5280
vacantDens <- cbind(vacantX, vacantY)
vacantDens1 <- as.data.frame(vacantDens)
vacant.points.sf <- st_as_sf(x = vacantDens1, coords = 1:2)

foreclosureX <- (st_coordinates(foreclosureSites)[,1])/5280
foreclosureY <- (st_coordinates(foreclosureSites)[,2])/5280
foreclosureDens <- cbind(foreclosureX, foreclosureY)
foreclosureDens1 <- as.data.frame(foreclosureDens)
foreclosure.points.sf <- st_as_sf(x = foreclosureDens1, coords = 1:2)

resCompX <- (st_coordinates(ResComplaints)[,1])/5280
resCompY <- (st_coordinates(ResComplaints)[,2])/5280
resCompDens <- cbind(resCompX, resCompY)
resCompDens1 <- as.data.frame(resCompDens)
resComp.points.sf <- st_as_sf(x=resCompDens1, coords = 1:2)

vacancy <- as.ppp(vacantDens,minneaWindow)
lead.ppp <- as.ppp(leadDens, minneaWindow)
foreclosure.ppp <- as.ppp(foreclosureDens, minneaWindow)
resComp.ppp <- as.ppp(resCompDens, minneaWindow)
densityRasterLead <- raster(density(lead.ppp))
densityRasterVacancy <- raster(density(vacancy))
densityRasterForeclosure <- raster(density(foreclosure.ppp))
densityRasterResComp <- raster(density(resComp.ppp))

lead_spdf <- as(densityRasterLead, "SpatialPixelsDataFrame")
lead_df <- as.data.frame(lead_spdf)
colnames(lead_df) <- c("value", "x", "y")
lead_df$x <- (lead_df$x * 5280)
lead_df$y <- (lead_df$y *5280)

coordinates(lead_df) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(lead_df) <- TRUE
# coerce to raster
leadRaster <- raster(lead_df)

new <- raster::mask(leadRaster, minnea)
lead_spdf1 <- as(new, "SpatialPixelsDataFrame")
lead_df1 <- as.data.frame(lead_spdf1)
colnames(lead_df1) <- c("value", "x", "y")

vacancy_spdf <- as(densityRasterVacancy, "SpatialPixelsDataFrame")
vacancy_df <- as.data.frame(vacancy_spdf)
colnames(vacancy_df) <- c("value", "x", "y")
vacancy_df$x <- (vacancy_df$x * 5280)
vacancy_df$y <- (vacancy_df$y *5280)

coordinates(vacancy_df) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(vacancy_df) <- TRUE
# coerce to raster
vacancyRaster <- raster(vacancy_df)

new1 <- raster::mask(vacancyRaster, minnea)
vacancy_spdf1 <- as(new1, "SpatialPixelsDataFrame")
vacancy_df1 <- as.data.frame(vacancy_spdf1)
colnames(vacancy_df1) <- c("value", "x", "y")

foreclosure_spdf <- as(densityRasterForeclosure, "SpatialPixelsDataFrame")
foreclosure_df <- as.data.frame(foreclosure_spdf)
colnames(foreclosure_df) <- c("value", "x", "y")
foreclosure_df$x <- (foreclosure_df$x * 5280)
foreclosure_df$y <- (foreclosure_df$y *5280)

coordinates(foreclosure_df) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(foreclosure_df) <- TRUE
# coerce to raster
foreclosureRaster <- raster(foreclosure_df)

new2 <- raster::mask(foreclosureRaster, minnea)
foreclosure_spdf1 <- as(new2, "SpatialPixelsDataFrame")
foreclosure_df1 <- as.data.frame(foreclosure_spdf1)
colnames(foreclosure_df1) <- c("value", "x", "y")

resComp_spdf <- as(densityRasterResComp, "SpatialPixelsDataFrame")
resComp_df <- as.data.frame(resComp_spdf)
colnames(resComp_df) <- c("value", "x", "y")
resComp_df$x <- (resComp_df$x * 5280)
resComp_df$y <- (resComp_df$y *5280)

coordinates(resComp_df) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(resComp_df) <- TRUE
# coerce to raster
resCompRaster <- raster(resComp_df)

new3 <- raster::mask(resCompRaster, minnea)
resComp_spdf1 <- as(new3, "SpatialPixelsDataFrame")
resComp_df1 <- as.data.frame(resComp_spdf1)
colnames(resComp_df1) <- c("value", "x", "y")

vacantLand <- VacantLand %>%
  dplyr::select(geometry) %>%
  st_transform(26851) %>%
  mutate(type = "Vacant Land") %>%
  st_as_sf()

leadPoints <- parcels_all_lead %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  as.data.frame() %>% 
  dplyr::select(geometry = geometry.x) %>%
  mutate(type = "Lead") %>%
  st_as_sf() %>%
  st_transform(26851)

allPlaces <- rbind(vacantLand, leadPoints)

allPlaces <- cbind(as.data.frame(st_coordinates(allPlaces)), data.frame(allPlaces))

leadPointsXY <-
  allPlaces %>%
  filter(type == "Lead") %>%
  dplyr::select(X,Y) %>%
  as.matrix()   

vacantLandXY <-
  allPlaces %>%
  filter(type == "Vacant Land") %>%
  dplyr::select(X,Y) %>%
  as.matrix() 

Vacancynn1 <- get.knnx(vacantLandXY,leadPointsXY,k=1)
Vacancynn3 <- get.knnx(vacantLandXY,leadPointsXY,k=3)
Vacancynn5 <- get.knnx(vacantLandXY,leadPointsXY,k=5)

leadOneVacant <-
  as.data.frame(Vacancynn1$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(VacantLandUse, VacantLand_Distance, V1) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_vacantOne = mean(VacantLand_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadThreeVacant <-
  as.data.frame(Vacancynn3$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(VacantLandUse, VacantLand_Distance, V1:V3) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_vacantThree = mean(VacantLand_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadFiveVacant <-
  as.data.frame(Vacancynn5$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(VacantLandUse, VacantLand_Distance, V1:V5) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_vacantFive = mean(VacantLand_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

minnea_vacancyOne <- as.data.frame(cbind(parcels_all_lead, d_vacant = leadOneVacant$d_vacantOne, KNN = "Nearest Vacant Lot"))
minnea_vacancyThree <- as.data.frame(cbind(parcels_all_lead,d_vacant = leadThreeVacant$d_vacantThree, KNN = "Three Nearest Vacant Lots"))
minnea_vacancyFive <- as.data.frame(cbind(parcels_all_lead,d_vacant = leadFiveVacant$d_vacantFive, KNN = "Five Nearest Vacant Lots"))
minnea_vacancy_distance <- rbind(minnea_vacancyOne, minnea_vacancyThree, minnea_vacancyFive)

vacancy_Distance_Facet <- minnea_vacancy_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  dplyr::summarize(Mean = mean(d_vacant)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = Mean, group=KNN, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity") +
  geom_text(inherit.aes = TRUE, aes(label=paste(round(Mean),"feet"), vjust=-.5)) +
  scale_fill_manual(values=palette2)+
  facet_wrap(~KNN) +
  labs(title="Nearest Vacant Lot", y="Mean Distance", fill="Lead Presence") +
  theme(panel.background = element_blank(),
        axis.ticks.y = element_line(color="grey70"),
        axis.text.x =  element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey70"))

ResCompPoints <- ResComplaints %>%
  dplyr::select(geometry) %>%
  st_transform(26851) %>%
  mutate(type = "Complaint")

allPlaces1 <- rbind(ResCompPoints, leadPoints)
allPlaces1 <- cbind(as.data.frame(st_coordinates(allPlaces1)), data.frame(allPlaces1))
ResCompPointsXY <-
  allPlaces1 %>%
  filter(type == "Complaint") %>%
  dplyr::select(X,Y) %>%
  as.matrix() 

ResCompnn1 <- get.knnx(ResCompPointsXY,leadPointsXY,k=1)
ResCompnn3 <- get.knnx(ResCompPointsXY,leadPointsXY,k=3)
ResCompnn5 <- get.knnx(ResCompPointsXY,leadPointsXY,k=5)

leadOneComp <-
  as.data.frame(ResCompnn1$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(ResComp, ResComp_Distance, V1) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_ResCompOne = mean(ResComp_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadThreeComp <-
  as.data.frame(ResCompnn3$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(ResComp, ResComp_Distance, V1:V3) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_ResCompThree = mean(ResComp_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadFiveComp <-
  as.data.frame(ResCompnn5$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(ResComp, ResComp_Distance, V1:V5) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_ResCompFive = mean(ResComp_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

minnea_compOne <- as.data.frame(cbind(parcels_all_lead, d_ResComp = leadOneComp$d_ResCompOne, KNN = "Nearest Call"))
minnea_compThree <- as.data.frame(cbind(parcels_all_lead,d_ResComp = leadThreeComp$d_ResCompThree, KNN = "Nearest Three Calls"))
minnea_compFive <- as.data.frame(cbind(parcels_all_lead,d_ResComp = leadFiveComp$d_ResCompFive, KNN = "Nearest Five Calls"))
minnea_comp_distance <- rbind(minnea_compOne, minnea_compThree, minnea_compFive)

ResComp_Distance_Facet <- minnea_comp_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  dplyr::summarize(Mean = mean(d_ResComp)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = Mean, group=KNN, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity") +
  geom_text(inherit.aes = TRUE, aes(label=paste(round(Mean),"feet"), vjust=-.5)) +
  scale_fill_manual(values=palette2)+
  facet_wrap(~KNN) +
  labs(title="Nearest 311 Call about Residential Conditions Complaint", y="Mean Distance", fill="Lead Presence") +
  theme(panel.background = element_blank(),
        axis.ticks.y = element_line(color="grey70"),
        axis.text.x =  element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey70"))

foreclosurePoints <- foreclosureSites %>%   
  dplyr::select(geometry) %>%
  st_transform(26851) %>%
  mutate(type = "Foreclosure")

allPlaces2 <- rbind(foreclosurePoints, leadPoints)
allPlaces2 <- cbind(as.data.frame(st_coordinates(allPlaces2)), data.frame(allPlaces2))
ForeclosurePointsXY <-
  allPlaces2 %>%
  filter(type == "Foreclosure") %>%
  dplyr::select(X,Y) %>%
  as.matrix() 

Foreclosurenn1 <- get.knnx(ForeclosurePointsXY,leadPointsXY,k=1)              #[Evan - troubleshooting]
Foreclosurenn3 <- get.knnx(ForeclosurePointsXY,leadPointsXY,k=3)
Foreclosurenn5 <- get.knnx(ForeclosurePointsXY,leadPointsXY,k=5)

leadOneForeclosure <-
  as.data.frame(Foreclosurenn1$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(Foreclosure, Foreclosure_Distance, V1) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_ForeclosureOne = mean(Foreclosure_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadThreeForeclosure <-
  as.data.frame(Foreclosurenn3$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(Foreclosure, Foreclosure_Distance, V1:V3) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_ForeclosureThree = mean(Foreclosure_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadFiveForeclosure <-
  as.data.frame(Foreclosurenn5$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  tidyr::gather(Foreclosure, Foreclosure_Distance, V1:V5) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_ForeclosureFive = mean(Foreclosure_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

minnea_foreclosureOne <- as.data.frame(cbind(parcels_all_lead,d_Foreclosure = leadOneForeclosure$d_ForeclosureOne, KNN = "Nearest Foreclosure"))
minnea_foreclosureThree <- as.data.frame(cbind(parcels_all_lead,d_Foreclosure = leadThreeForeclosure$d_ForeclosureThree, KNN = "Three Nearest Foreclosures"))
minnea_foreclosureFive <- as.data.frame(cbind(parcels_all_lead,d_Foreclosure = leadFiveForeclosure$d_ForeclosureFive, KNN = "Five Nearest Foreclosures"))
minnea_foreclosure_distance <- rbind(minnea_foreclosureOne, minnea_foreclosureThree, minnea_foreclosureFive)

foreclosure_Distance_Facet <- minnea_foreclosure_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  dplyr::summarize(Mean = mean(d_Foreclosure)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = Mean, group=KNN, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity") +
  geom_text(inherit.aes = TRUE, aes(label=paste(round(Mean),"feet"), vjust=-.5)) +
  scale_fill_manual(values=palette2)+
  facet_wrap(~KNN) +
  labs(title="Nearest Foreclosure", y="Mean Distance", fill="Lead Presence") +
  theme(panel.background = element_blank(),
        axis.ticks.y = element_line(color="grey70"),
        axis.text.x =  element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey70"))

#t.test(minnea_foreclosure_distance$d_ForeclosureOne ~ minnea_foreclosure_distance$Lead_Hazar)


leadPoints2 <- lead %>%
  dplyr::select(geometry) %>%
  st_transform(26851) %>%
  mutate(type = "Lead2")

allPlaces3 <- rbind(leadPoints2, leadPoints)
allPlaces3 <- cbind(as.data.frame(st_coordinates(allPlaces3)), data.frame(allPlaces3))
LeadPoints2XY <-
  allPlaces3 %>%
  filter(type == "Lead2") %>%
  dplyr::select(X,Y) %>%
  as.matrix() 

Leadnn1 <- get.knnx(LeadPoints2XY,leadPointsXY,k=2)
Leadnn3 <- get.knnx(LeadPoints2XY,leadPointsXY,k=4)
Leadnn5 <- get.knnx(LeadPoints2XY,leadPointsXY,k=6)

leadOneLead <-
  as.data.frame(Leadnn1$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>%
  mutate(new = ifelse(V1 < 1, V2, V1)) %>%
  dplyr::select(-V1, -V2) %>% 
  tidyr::gather(Lead2, Lead_Distance, new) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_LeadOne = mean(Lead_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadThreeLead <-
  as.data.frame(Leadnn3$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>% 
  mutate(new = ifelse(V1 < 1, V2, V1)) %>%
  dplyr::select(-V1, -V2) %>%
  tidyr::gather(Lead2, Lead_Distance, V3:new) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_LeadThree = mean(Lead_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

leadFiveLead <-
  as.data.frame(Leadnn5$nn.dist) %>% 
  tibble::rownames_to_column(var = "Lead") %>%
  mutate(new = ifelse(V1 == 0, V2, V1)) %>%
  dplyr::select(-V1, -V2) %>%
  tidyr::gather(Lead2, Lead_Distance, V3:new) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  group_by(Lead) %>% 
  dplyr::summarize(d_LeadFive = mean(Lead_Distance)) %>% 
  dplyr::arrange(as.numeric(Lead)) %>% 
  dplyr::select(-Lead)

minnea_LeadOne <- as.data.frame(cbind(parcels_all_lead, d_Lead = leadOneLead$d_LeadOne, KNN = "Nearest Lead Test Site"))
minnea_LeadThree <- as.data.frame(cbind(parcels_all_lead, d_Lead = leadThreeLead$d_LeadThree, KNN = "Three Nearest Lead Test Sites"))
minnea_LeadFive <- as.data.frame(cbind(parcels_all_lead, d_Lead = leadFiveLead$d_LeadFive, KNN="Five Nearest Lead Test Sites"))
minnea_Lead_distance <- rbind(minnea_LeadOne, minnea_LeadThree,minnea_LeadFive)

lead_Distance_Facet <- minnea_Lead_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  dplyr::summarize(Mean = median(d_Lead)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = Mean, group=KNN, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity") +
  geom_text(inherit.aes = TRUE, aes(label=paste(round(Mean),"feet"), vjust=-.5)) +
  scale_fill_manual(values=palette2)+
  facet_wrap(~KNN) +
  labs(title="Nearest Test Site", y="Mean Distance", fill="Lead Presence") +
  theme(panel.background = element_blank(),
        axis.ticks.y = element_line(color="grey70"),
        axis.text.x =  element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey70"))

lead_Distance_Facet1 <- minnea_Lead_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  #dplyr::summarize(Mean = mean(d_Lead)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = as.integer(d_Lead))) +
  geom_boxplot(aes(fill=as.factor(Lead_Hazar))) +
  scale_y_continuous(limits = quantile(minnea_Lead_distance$d_Lead, c(0, 0.9))) +
  #geom_text(inherit.aes = TRUE, aes(label=paste(round(Mean),"feet"), vjust=-.5)) +
  scale_fill_manual(values=palette2)+
  facet_wrap(~KNN) +
  labs(title="Nearest Test Site", y="Distance", fill="Lead Presence") +
  plotTheme()

lead_Distance_Facet_violin <- minnea_Lead_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  ggplot(., aes(x = Lead_Hazar, y = as.integer(d_Lead))) +
  geom_boxplot(aes(fill = as.factor(Lead_Hazar)),position=position_dodge(width=0.9)) +
  scale_fill_manual(values=paletteLead, labels = c("Negative", "Positive")) +
  facet_wrap(~KNN) +
  scale_y_continuous(limits = quantile(minnea_Lead_distance$d_Lead, c(0, 0.9))) +
  labs(title="Nearest Lead Presence", y="Distance (feet)", fill="Lead Presence", subtitle="Distance means visualized as points") +
  stat_summary(fun.y=mean, geom="point", size=5, colour="white") +
  plotTheme() 

foreclosure_Distance_Facet_violin <- minnea_foreclosure_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  ggplot(., aes(x = Lead_Hazar, y = as.integer(d_Foreclosure))) +
  geom_boxplot(aes(fill = as.factor(Lead_Hazar))) +
  scale_fill_manual(values=paletteLead, labels = c("Negative", "Positive")) +
  facet_wrap(~KNN) +
  scale_y_continuous(limits = quantile(minnea_foreclosure_distance$d_Foreclosure, c(0, 0.9))) +
  labs(title="Nearest Foreclosure", y="Distance (feet)", fill="Lead Presence", subtitle="Distance means visualized as points", caption= "One nearest foreclosure is the most significant different") +
  stat_summary(fun.y=mean, geom="point", size=5, colour="white") +
  plotTheme() 

resComp_Distance_Facet_violin <- minnea_comp_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  ggplot(., aes(x = Lead_Hazar, y = as.integer(d_ResComp))) +
  geom_boxplot(aes(fill = as.factor(Lead_Hazar))) +
  scale_fill_manual(values=paletteLead, labels = c("Negative", "Positive")) +
  facet_wrap(~KNN) +
  scale_y_continuous(limits = quantile(minnea_comp_distance$d_ResComp, c(0, 0.9))) +
  labs(title="Nearest 311 Complaint", y="Distance (feet)", fill="Lead Presence", subtitle="Residential Conditions Complaints only; Distance means visualized as points", caption="Distance to five nearest calls is the most significantly different") +
  stat_summary(fun.y=mean, geom="point", size=5, colour="white") +
  plotTheme() 

vacancy_Distance_Facet_violin <- minnea_vacancy_distance %>% 
  group_by(Lead_Hazar, KNN) %>%
  ggplot(., aes(x = Lead_Hazar, y = as.integer(d_vacant))) +
  geom_boxplot(aes(fill = as.factor(Lead_Hazar))) +
  scale_fill_manual(values=paletteLead, labels = c("Negative", "Positive")) +
  facet_wrap(~KNN) +
  scale_y_continuous(limits = quantile(minnea_vacancy_distance$d_vacant, c(0, 0.9))) +
  labs(title="Nearest Vacant Lot", y="Distance (feet)", fill="Lead Presence", subtitle="Distance means visualized as points") +
  stat_summary(fun.y=mean, geom="point", size=5, colour="white") +
  plotTheme() 

library(spdep)
leadCoords <- Parcels %>%
     dplyr::select(geometry) %>%
     st_transform(26851) %>% 
     mutate(X = st_coordinates(Parcels %>% st_transform(26851))[,1]) %>% 
     mutate(Y = st_coordinates(Parcels %>% st_transform(26851))[,2]) %>% 
     as.data.frame() %>% 
     dplyr::select(X,Y) %>% 
     as.matrix()

neighborPointsLead <- knn2nb(knearneigh(leadCoords, 5))
spatialWeights <- nb2listw(neighborPointsLead, style="W")

moranTest <- moran.mc(as.numeric(as.character(Parcels$Led_Hzr)), spatialWeights, nsim = 999)

moransCluster <- ggplot(as.data.frame(moranTest$res), aes(moranTest$res)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic), colour = "red",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I", subtitle="P-Value = .001")
# Spatial Lag -------------------------------------------------------------

# Chi-Squared tests -------------------------------------------------------

ZoningDataFrame <- parcels_all_lead %>%
  dplyr::select(PID, Lead_Hazar, Zoning) %>% 
  mutate(density = ifelse(Zoning == "R1"| Zoning == "R1A"| Zoning == "R2"| Zoning == "R2B", "Low", ifelse(Zoning == "R3" | Zoning == "R4", "Medium", ifelse(Zoning == "R5"| Zoning == "R6"| Zoning == "B4N"| Zoning == "B4-2"| Zoning == "B4S-1", "High", "Other")))) %>% 
  mutate(density = factor(density, levels=c("High", "Medium","Low","Other"))) %>% 
  #filter(PR_TYP_NM1 %in% c("APARTMENT", "CONDOMINIUM", "LOW INCOME RENTAL",
  #"RESIDENTIAL","RESIDENTIAL-TWO UNIT", "TRIPLEX")) %>%
  group_by(density, Lead_Hazar) %>% 
  dplyr::summarize(count = n()) %>%
  mutate(total=sum(count)) %>% 
  mutate(rate = as.integer(round((count/total)*100))) %>%
  as.data.frame()

ZoningDataFrame2 <- parcels_all_lead %>%
  dplyr::select(PID, Lead_Hazar, Zoning) %>% 
  mutate(density = ifelse(Zoning == "R1"| Zoning == "R1A"| Zoning == "R2"| Zoning == "R2B", "Low", ifelse(Zoning == "R3" | Zoning == "R4", "Medium", ifelse(Zoning == "R5"| Zoning == "R6"| Zoning == "B4N"| Zoning == "B4-2"| Zoning == "B4S-1", "High", "Other")))) %>% 
  mutate(density = factor(density, levels=c("High", "Medium","Low","Other"))) %>% 
  dplyr::select(-Lead_Hazar, -Zoning, -geometry)

ZoningForChi <- ZoningDataFrame %>% 
  dplyr::select(-density, -total, -rate, -Lead_Hazar) %>%
  t()

HC <- chisq.test(ZoningForChi[,1:2])$p.value
MC <- chisq.test(ZoningForChi[,3:4])$p.value
LC <- chisq.test(ZoningForChi[,5:6])$p.value
OC <- chisq.test(ZoningForChi[,7:8])$p.value

ZoneFacet <- ZoningDataFrame %>%
  ggplot(., aes(x = Lead_Hazar, y = rate, group=density, fill = as.factor(Lead_Hazar))) + 
  geom_bar(stat="identity")+ 
  facet_wrap(~density)+
  geom_text(inherit.aes = TRUE, aes(label=paste(rate,"%"), vjust=1.5)) +
  scale_fill_manual(values=paletteLead, labels=c("Negative","Positive")) +
  labs(title = "Lead Presence by Zoning Density", x = "Lead", y = "Rate", fill="Lead Presence") +
  plotTheme()

HousingTypeDataFrame <- parcels_all_lead %>%
  dplyr::select(Lead_Hazar, PR_TYP_NM1.x) %>% 
  filter(Lead_Hazar != 2) %>% 
  filter(PR_TYP_NM1.x %in% c("APARTMENT", "CONDOMINIUM", "LOW INCOME RENTAL",
                           "RESIDENTIAL","RESIDENTIAL-TWO UNIT", "TRIPLEX")) %>%
  group_by(PR_TYP_NM1.x, Lead_Hazar) %>% 
  dplyr::summarize(count = n()) %>%
  mutate(total=sum(count)) %>% 
  mutate(rate = as.integer(round((count/total)*100))) %>%
  as.data.frame()

HousingForChi <- HousingTypeDataFrame %>% 
  dplyr::select(-PR_TYP_NM1.x, -total, -rate, -Lead_Hazar) %>%
  t()

ApartChi <- chisq.test(HousingForChi[,1:2])$p.value
CondoChi <- chisq.test(HousingForChi[,3:4])$p.value
LIRChi <- chisq.test(HousingForChi[,5:6])$p.value
ResidentialChi <- chisq.test(HousingForChi[,7:8])$p.value
ResidentialTUChi <- chisq.test(HousingForChi[,9:10])$p.value
TriplexChi <- chisq.test(HousingForChi[,11:12])$p.value

HouseFacet <- HousingTypeDataFrame %>%
  ggplot(., aes(x = Lead_Hazar, y = rate, group=PR_TYP_NM1.x, fill = as.factor(Lead_Hazar))) + 
  geom_bar(stat="identity")+ 
  facet_grid(.~PR_TYP_NM1.x)+
  geom_text(inherit.aes = TRUE, aes(label=paste(rate,"%"), vjust=1.5)) +
  scale_fill_manual(values=paletteLead, labels=c("Negative","Positive"))+
  labs(title = "Lead Presence by Housing Type", x = "Lead", y = "Rate", fill="Lead Presence") +
  plotTheme()

BuildingTypeDataFrame <- parcels_all_lead %>%
  dplyr::select(Lead_Hazar, BuildingUs) %>% 
  filter(Lead_Hazar !=2) %>% 
  filter(BuildingUs %in% c("2 Fam. Conv. Sgl. Dwlg.", "Apartment 4 or 5 Unit","Apartment 6+ Unit", 
                           "Apartment Converted","Double Bungalow", "Duplex", "Multi-Family & Rooms",
                           "Single Fam. Dwlg.", "Tri-plex")) %>%
  group_by(BuildingUs, Lead_Hazar) %>% 
  dplyr::summarize(count = n()) %>%
  mutate(total=sum(count)) %>% 
  mutate(rate = as.integer(round((count/total)*100))) %>%
  as.data.frame()

BldgForChi <- BuildingTypeDataFrame %>% 
  dplyr::select(-BuildingUs, -total, -rate, -Lead_Hazar) %>%
  t()

TwoFamChi <- chisq.test(BldgForChi[,1:2])$p.value   #statistically different
Apartment4Chi <- chisq.test(BldgForChi[,3:4])$p.value 
Apartment6Chi <- chisq.test(BldgForChi[,5:6])$p.value #statistically different
ApartmentConChi <- chisq.test(BldgForChi[,7:8])$p.value  #statistically different
BungalowChi <- chisq.test(BldgForChi[,9:10])$p.value  # kinda statistically different
DuplexChi <- chisq.test(BldgForChi[,11:12]) $p.value #statistically different
MFChi <- chisq.test(BldgForChi[,13:14])$p.value
SFChi <- chisq.test(BldgForChi[,15:16])$p.value     #statistically different
TriplexChi1 <- chisq.test(BldgForChi[,17:18])$p.value  # kinda statistically different

BldgFacet <- BuildingTypeDataFrame %>%
  ggplot(., aes(x = Lead_Hazar, y = rate, group=BuildingUs, fill = as.factor(Lead_Hazar))) + 
  geom_bar(stat="identity")+ 
  facet_grid(.~BuildingUs)+
  geom_text(inherit.aes = TRUE, aes(label=paste(rate,"%"), vjust=1.5)) +
  scale_fill_manual(values=paletteLead, labels=c("Negative","Positive"))+
  labs(title = "Lead Presence by Building Type", x = "Lead", y = "Rate", fill="Lead Presence") +
  plotTheme()

#Exterior Type Variable
table(parcelsLandBuilding$ExteriorTy)

ExteriorTypeDataFrame <- parcels_all_lead %>%
  filter(Lead_Hazar != 2) %>% 
  dplyr::select(Lead_Hazar, ExteriorTy) %>% 
  filter(ExteriorTy %in% c("Brick", "Concrete Fiber","Cement Board", 
                           "Metal/Vinyl","Other", "Stucco", "Wood")) %>%
  group_by(ExteriorTy, Lead_Hazar) %>% 
  dplyr::summarize(count = n()) %>%
  mutate(total=sum(count)) %>% 
  mutate(rate = as.integer(round((count/total)*100))) %>%
  as.data.frame()

ExteriorForChi <- ExteriorTypeDataFrame %>% 
  dplyr::select(-ExteriorTy, -total, -rate, -Lead_Hazar) %>%
  t()

BrickChi <- chisq.test(ExteriorForChi[,1:2])$p.value  #multiple are statistically different
VinylChi <- chisq.test(ExteriorForChi[,3:4])$p.value
OtherChi <- chisq.test(ExteriorForChi[,5:6])$p.value
StuccoChi <- chisq.test(ExteriorForChi[,7:8])$p.value
WoodChi <- chisq.test(ExteriorForChi[,9:10])$p.value

ExteriorFacet <- ExteriorTypeDataFrame %>% 
  ggplot(., aes(x = Lead_Hazar, y = rate, group=ExteriorTy, fill = as.factor(Lead_Hazar))) + 
  geom_bar(stat="identity")+ 
  facet_grid(.~ExteriorTy)+
  geom_text(inherit.aes = TRUE, aes(label=paste(rate,"%"), vjust=-.5)) +
  scale_fill_manual(values=paletteLead, labels=c("Negative","Positive"))+
  labs(title = "Lead Presence by Building Type", x = "Lead", y = "Rate", fill="Lead Presence") +
  plotTheme()

parcelvars <- c("High Density Zoning", "Medium Density Zoning", "Low Density Zoning", "Other Zoning",
                "Apartment", "Condo", "Low Income Rental", "Residential", "Residential Two-Unit", "Triplex",
                "Two Family", "Apartment, 4 or 5 Unit", "Apartment, 6+ Unit", "Converted Apartment", "Double Bungalow", "Duplex", "Multi-family", "Single Family", "Tri-plex",
                "Brick", "Vinyl", "Stucco", "Wood", "Other")
chitest <- c(HC, MC, LC, OC, 
             ApartChi, CondoChi, LIRChi, ResidentialChi, ResidentialTUChi, TriplexChi,
             TwoFamChi, Apartment4Chi, Apartment6Chi, ApartmentConChi, BungalowChi, DuplexChi, MFChi, SFChi, TriplexChi1,
             BrickChi, VinylChi, StuccoChi, WoodChi, OtherChi)
signif <- c("No", "Yes", "Yes","Yes","No","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","Yes","Yes","No","Yes","Yes","Yes","Yes")
parceltable <- cbind(Variable = parcelvars, P.Value = chitest, Type = "Parcel-Level Variables", Significance = signif)
write.csv(parceltable, "SpreadsheetData/parceltable.csv")

census_api_key("eebf4014242fce6be1c40439ea82cb6a2b28f921", install=TRUE, overwrite = TRUE)

# 2016 Block level data
Block16 <- 
  get_acs(geography = "block group", variables = c("B01001_001E","B01001_003E","B01001_004E","B01001_027E",
                                                   "B01001_028E","B09018_002E","B09019_004E",
                                                   "B14007_001E","B15003_001E","B15011_001E",
                                                   "B17001_001E","B17010_001E","B17011_001E",
                                                   "B17017_001E","B19013_001E","B19037_001E",
                                                   "B19051_001E","B19052_001E","B19053_001E",
                                                   "B19127_001E","B19201_001E","B25003_001E",
                                                   "B25004_001E","B25081_001E"), 
          endyear = 2016, state=27, county=053, geometry=T) %>%
  dplyr::select(variable,estimate,GEOID) %>%
  as.data.frame() %>%
  tidyr::spread(variable,estimate) %>%
  rename(TotalPopulation=B01001_001,
         TotalMaleUnder5=B01001_003,
         TotalMale5to9=B01001_004,
         TotalFemaleUnder5=B01001_027,
         TotalFemale5to9=B01001_028,
         TotalOwnchild=B09018_002,
         TotalInfamilyhouseholds=B09019_004,
         SCHOOLENROLLMENT3YearsOlder=B14007_001,
         EDUCATIONAL25Older=B15003_001,
         BACHELORMAJOR25Older=B15011_001,
         POVERTYSTATUSPAST12MONTHS=B17001_001,
         POVERTYSTATUSPAST12MONTHFAMILIES=B17010_001,
         AGGREGATEINCOMEDEFICIT12FAMILIES =B17011_001,
         MEDIANHOUSEHOLDINC12MONTHS =B19013_001,
         AGEOFHOUSEHOLDERBYHOUSEHOLDINCOME =B19037_001,
         EARNINGS12MONTHSFORHOUSEHOLDS =B19051_001,
         SALARYINCOME12MONTHSFORHOUSEHOLDS =B19052_001,
         SELFEMPLOYMENTINCOME12MONTHSFORHOUSEHOLDS =B19053_001,
         AGGREGATEFAMILYINCOMEPAST12MONTHS =B19127_001,
         NONFAMILYHOUSEHOLDINCOMEPAST12MONTHS =B19201_001,
         TENURE =B25003_001,
         VACANCYSTATUS =B25004_001,
         MORTGAGESTATUS =B25081_001) %>%
  st_sf() %>%
  st_transform(3857) 

write.csv(Block16, file = "Block16.csv")

#first import the shapefile of blockgroups in R
blockgroupshp <- read_sf("Shapefiles/BlockGroups_2016_clip_proj.shp")
#head(blockgroupshp)

#Dissolve the blockgroup shapefile into a mask for clipping out the ACS data
dissolveMinn <- 
  #  #start with block group  
  blockgroupshp %>%
  #  #dissolve
  st_union() %>%
  #  #convert to simple features 
  st_sf() %>%
  #  #project  
  st_transform(3857)

#select block groups in Block16 that are inside the minneapolis mask
st_crs(Block16)

#is the projection the same for these two?
st_crs(dissolveMinn) == st_crs(Block16)
#tranforms the dissolved by the projection block16
dissolveMinn <- st_transform(dissolveMinn, 3857)

#select by location
MinACS16 <- Block16[dissolveMinn,]

#change the ACS features into a dataframe
forjoin <- st_as_sf(MinACS16) %>% st_transform(3857)
Min16 <- st_join(blockgroupshp %>% st_transform(3857), forjoin)
names(Min16)

# Plot the Minneapolis shapefile with ACS data
ggplot() +
  geom_sf(data=Min16, aes(fill=TotalMaleUnder5))

head(Min16)

Parcels <- st_as_sf(finalTested)

Min16 <- dplyr::select(Min16, GEOID.x, TotalMaleUnder5, TotalFemaleUnder5,TotalMale5to9,TotalFemale5to9, TotalPopulation,BACHELORMAJOR25Older, POVERTYSTATUSPAST12MONTHFAMILIES, 
                       MEDIANHOUSEHOLDINC12MONTHS, EARNINGS12MONTHSFORHOUSEHOLDS, TENURE, 
                       VACANCYSTATUS, MORTGAGESTATUS)%>%
  mutate(ChildUnder5 = TotalMaleUnder5 + TotalFemaleUnder5,
         Child5to9 = TotalMale5to9 + TotalFemale5to9)

Min16 <- rename(Min16, Educ16 = BACHELORMAJOR25Older, Poverty16 = POVERTYSTATUSPAST12MONTHFAMILIES, 
                MedianInc16 = MEDIANHOUSEHOLDINC12MONTHS, 
                Earnings16 = EARNINGS12MONTHSFORHOUSEHOLDS,
                Tenure16 = TENURE, Vacancy16 = VACANCYSTATUS, 
                Mortgage16 = MORTGAGESTATUS)
head(Min16)


################################join with children blood lead data################
######The children's blood lead level
ChildBLLead<-read.csv("SpreadsheetData/ChildhoodLeadTract_clean.csv")
ChildBLLead_df <- as.data.frame(ChildBLLead)

#Get tract level ACS data
Tract16 <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001_003E","B01001_004E","B01001_027E",
                                             "B01001_028E","B09018_002E"), 
          endyear = 2016, state=27, county=053, geometry=T) %>%
  dplyr::select(variable,estimate,GEOID) %>%
  as.data.frame() %>%
  tidyr::spread(variable,estimate) %>%
  rename(TotalPopulation=B01001_001,
         TotalMaleUnder5=B01001_003,
         TotalMale5to9=B01001_004,
         TotalFemaleUnder5=B01001_027,
         TotalFemale5to9=B01001_028,
         TotalOwnchild=B09018_002) %>%
  st_sf() %>%
  st_transform(3857) 

write.csv(Tract16, file="Tract16.csv")

names(Tract16)
Tract16$GEOID <- as.character(Tract16$GEOID) 
ChildBLLead_df$TRACT_ID <- as.character(ChildBLLead$TRACT_ID)

#Join the children blood lead level with 
Tract16withLD <- left_join(Tract16, ChildBLLead_df, by=c("GEOID" = "TRACT_ID"))
Tract16withLD <- st_as_sf(Tract16withLD)
names(Tract16withLD)

parcels_all_lead1 <- st_as_sf(parcels_all_lead)

Parcels1 <- parcels_all_lead1 %>% st_transform(3857)

Parcels_w_blocks <- 
  aggregate(Min16, Parcels1, mean) %>%
  bind_cols(Parcels1) %>%
  dplyr::select(PID,BUILD_YR.y,Lead_Hazar, ChildUnder5,Child5to9,Educ16, Poverty16, MedianInc16,Earnings16,Tenure16,Vacancy16, Mortgage16)
head(Parcels_w_blocks)

Parcels_w_blocks <- st_transform(Parcels_w_blocks,3857) 

st_crs(Tract16withLD) == st_crs(Parcels_w_blocks)

Parcels_w_blocks_Childlead <- 
  aggregate(Tract16withLD, Parcels_w_blocks,mean) %>%
  bind_cols(Parcels_w_blocks)

#----------------------------Exploratory question: ACS and lead windows-------------------------------
#-----------------------everything below here are maps------------------------------------------------
##try with plotting Education rates in quintile breaks
Min16 <- Min16 %>% 
  mutate(EducRate = (Educ16/TotalPopulation))

breakvalues <- quantile((Min16$EducRate),
                        c(0,.2,.4,.6,.8, 1),na.rm=T)

Min16$Educbuckets <- 
  factor(cut(Min16$EducRate, c(breakvalues)),labels = c("labels"))

EdRate <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$Educbuckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("0.00% - 14.28%", "14.28% - 27.41%", "27.41% - 40.27%", "40.27% - 51.34%", "51.34% - 78.63%"),
                    name="Percent of residents with bachelor degree\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \nResidents' education level?",
       subtitle= "Lead Windows Inspection Results and number of residents with bachelor degree") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()

######try with plotting percentage of Children under 5 in quintile breaks
Min16 <- Min16 %>% 
  mutate(Childunder5Rate = (Child5to9/TotalPopulation))

breakvalues <- quantile((Min16$Childunder5Rate),
                        c(0,.2,.4,.6,.8, 1),na.rm=T)

Min16$Childunder5buckets <- 
  factor(cut(Min16$Childunder5Rate, c(breakvalues)),labels = c("labels"))

YoungKid <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$Childunder5buckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("0.00% - 1.71%", "1.71% - 4.16%", "4.16% - 6.34%", "6.34% - 9.58%", "9.58% - 20.68%"),
                    name="Percent of children under 5\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \npresence of children?",
       subtitle= "Lead Windows Inspection Results and percents of children under 5") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()
#
######try with plotting percentage of people in poverty in quintile breaks
Min16 <- Min16 %>% 
  mutate(Poverty16Rate = (Poverty16/TotalPopulation))

breakvalues <- quantile((Min16$Poverty16Rate),
                        c(0,.2,.4,.6,.8, 1),na.rm=T)

Min16$PovertyRatebuckets <- 
  factor(cut(Min16$Poverty16Rate, c(breakvalues)),labels = c("labels"))

Poverty <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$PovertyRatebuckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("0.00% - 15.42%", "15.42% - 18.86%", "18.86% - 22.76%", "22.76% - 25.88%", "25.88% - 35.83%"),
                    name="Percent of people in poverty\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \npoverty rate?",
       subtitle= "Lead Windows Inspection Results and percent of people in poverty") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()
#
######try with plotting median household income in quintile breaks
breakvalues <- quantile((Min16$MedianInc16),
                        c(0,.2,.4,.6,.8, 1),na.rm=T)

Min16$MedianInc16buckets <- 
  factor(cut(Min16$MedianInc16, c(breakvalues)),labels = c("labels"))

MedInc <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$MedianInc16buckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("9907.0 - 36096.4", "36096.4 - 51200.2", "51200.2 - 67618.2", "67618.2 - 91467.8", "91467.8 - 196569.0"),
                    name="Median Household Income\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \nMedian Household Income?",
       subtitle= "Lead Windows Inspection Results and Median Household Income") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()
#
######try with plotting median earnings in quintile breaks
breakvalues <- quantile((Min16$Earnings16),
                        c(0,.6,.7,.8,.9, 1),na.rm=T)

Min16$Earnings16buckets <- 
  factor(cut(Min16$Earnings16, c(breakvalues)),labels = c("labels"))

MedEarn <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$Earnings16buckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("77.0 - 433.6", "433.6 - 477.6", "477.6 - 558.6", "558.6 - 736.4", "736.4 - 1980.0"),
                    name="Median Earnings\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \nMedian Earnings?",
       subtitle= "Lead Windows Inspection Results and Median Earnings in the past 12 month") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()

################################
######try with plotting Tenure in quintile breaks
breakvalues <- quantile((Min16$Tenure16),
                        c(0,.6,.7,.8,.9, 1),na.rm=T)

Min16$Tenure16buckets <- 
  factor(cut(Min16$Tenure16, c(breakvalues)),labels = c("labels"))

Parcels <- st_transform(Parcels, 26851)
Parcels_lead

pal_blue5 = c("#154f4a", "#376b66", "#588782", "#7ea8a2", 
              "#a9ccc6")

Tenure <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$Tenure16buckets), color = 'white') +
  scale_fill_manual(values = pal_blue5,
                    labels = c("77.0 - 433.6", "433.6 - 477.6", "477.6 - 558.6", "558.6 - 736.4", "736.4 - 1980.0"),
                    name="Tenure\n (Quintile Breaks)") +
  geom_point(data=Parcels, y=(st_coordinates(Parcels)[,1]), x=(st_coordinates(Parcels)[,2]),
             aes(color= as.factor(Parcels$Lead_Hazar)),size=5) +
  labs(title= "Is Lead Window Presence related with \nTenure?",
       subtitle= "Lead Windows Inspection Results and Tenure") +
  scale_color_manual(values = paletteLead,
                     labels=c("Negative","Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()

######try with plotting median household income in quintile breaks
breakvalues <- quantile((Min16$Vacancy16),
                        c(0,.4,.8,.9,.95, 1),na.rm=T)

Min16$Vacancy16buckets <- 
  factor(cut(Min16$Vacancy16, c(breakvalues)),labels = c("labels"))

VacMap <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$Vacancy16buckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("0.0 - 12.2", "12.2 - 48.0", "48.0 - 76.0", "76.0 - 103.6", "103.6 - 246.0"),
                    name="Vacancy Status\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \nVacancy status?",
       subtitle= "Lead Windows Inspection Results and Vacancy status") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()

#
######try with plotting median household income in quintile breaks
breakvalues <- quantile((Min16$Mortgage16),
                        c(0,.2,.4,.6,.8, 1),na.rm=T)

Min16$Mortgage16buckets <- 
  factor(cut(Min16$Mortgage16, c(breakvalues)),labels = c("labels"))

MortMap <- ggplot() + 
  geom_sf(data = Min16, aes(fill = Min16$Mortgage16buckets), color = 'white') +
  scale_fill_manual(values = paletteChoropleth,
                    labels = c("0.0 - 102.0", "102.0 - 175.8", "175.8 - 247.0", "247.0 - 328.4", "328.4 - 832.0"),
                    name="Mortgage Status\n (Quintile Breaks)") +
  geom_point(data=lead, x=(st_coordinates(lead)[,1]), y=(st_coordinates(lead)[,2]),
             aes(color= as.factor(Lead_Hazar)),size=0.8) +
  labs(title= "Is Lead Window Presence related with \nMortgage status?",
       subtitle= "Lead Windows Inspection Results and Mortgage status") +
  scale_color_manual(values = c("#FA6900"),
                     labels=c("Positive"),
                     name = "Lead Window Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()
################################
blocks <- Min16 %>% st_transform(26851)
st_write(blocks, "Shapefiles/blocks.shp")

################################everything below here are bar charts #################################

#spatial join parcel data with current census data from Maureen
Parcels_w_blocks <- 
  aggregate(Min16, Parcels, mean) %>%
  bind_cols(Parcels) %>%
  dplyr::select(testedParcels_PID,BUILD_YR,Lead_Hazar, ChildUnder5,Child5to9,Educ16, Poverty16, MedianInc16, Earnings16, 
                Tenure16,Vacancy16, Mortgage16)
head(Parcels_w_blocks)

ggplot() +
  geom_sf(data=Parcels_w_blocks, aes(fill=Lead_Hazar))

##display lead inspection result with base map
baseMap + geom_sf(data=Parcels_w_blocks, aes(color=as.factor(Lead_Hazar))) + 
  scale_colour_manual(values = c("red", "blue"))

#Lead and Children under 5
BarChildUnder5 <- Parcels_w_blocks %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(ChildUnder5Mean = mean(ChildUnder5)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = ChildUnder5Mean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(ChildUnder5Mean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme() +  
  labs(x= "Presence of Lead", y= "Number of children under 5", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated with children presence?",
       subtitle= "Lead Windows Inspection Results and children under 5",
       subtitle= "P value")+
  labs(caption= "T-Test p-value > 0.5")

#Lead and Children 5 to 9
BarChildren5to9 <- Parcels_w_blocks %>%
  filter(Lead_Hazar != "2") %>% 
  group_by(Lead_Hazar) %>%
  dplyr::summarize(Child5to9Mean = mean(Child5to9)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = Child5to9Mean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(Child5to9Mean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead,
                     labels= )+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Number of children 5 to 9", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated with children presence?",
       subtitle= "Lead Windows Inspection Results and children 5 to 9")+
  labs(caption= "T-Test p-value < 0.5")

#Lead and education(number of people with Bachelor degree)
palette2 <- c("#542e71", "#84a9c0")
BarEducation <- Parcels_w_blocks %>% ###################################### this one - high rental gain associated with lead
  group_by(Lead_Hazar) %>%
  dplyr::summarize(EducationMean = mean(Educ16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = EducationMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(EducationMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Number of people with Bachlor Degree", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated with Education Level?",
       subtitle= "Lead Windows Inspection Results and Residents with Bachelor Degree")+
  labs(caption= "T-Test p-value < 0.5")

#Lead and poverty(number of people in poverty)
BarPoverty <- Parcels_w_blocks %>% 
  group_by(Lead_Hazar) %>%
  dplyr::summarize(povertyMean = mean(Poverty16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = povertyMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(povertyMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Number of people in Poverty", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated with Poverty Rate?",
       subtitle= "Lead Windows Inspection Results and Residents in Poverty")+
  labs(caption= "T-Test p-value > 0.5")

#Lead and Income(Median Household Income level)
BarMedianHHInc <- Parcels_w_blocks %>% 
  group_by(Lead_Hazar) %>%
  dplyr::summarize(MedianIncMean = mean(MedianInc16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = MedianIncMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(MedianIncMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Median House income", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated with Income?",
       subtitle= "Lead Windows Inspection Results and Median Household Income in the past 12 months")+
  labs(caption= "T-Test p-value < 0.5")

#Lead and earning(Earning in the past 12 month in household)
BarEarnings <- Parcels_w_blocks %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(EarningsMean = mean(Earnings16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = EarningsMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(EarningsMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Earnings", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated \nwith Earning?",
       subtitle= "Lead Windows Inspection Results and Earnings in the past 12 months for households")+
  labs(caption= "T-Test p-value < 0.01")

#Lead and Tenure16
BarTenure <- Parcels_w_blocks %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(TenureMean = mean(Tenure16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = TenureMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(TenureMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Tenures", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated \nwith Tenure?",
       subtitle= "Lead Windows Inspection Results and tenure")+
  labs(caption= "T-Test p-value < 0.01")

#Lead and Vacancy
BarVacancy <- Parcels_w_blocks %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(VacancyMean = mean(Vacancy16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = VacancyMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(VacancyMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Vacancy", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated \nwith vacancy?",
       subtitle= "Lead Windows Inspection Results and vacancy status")+
  labs(caption= "T-Test p-value < 0.05")

#Lead and Mortgage16
BarMortgage <- Parcels_w_blocks %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(MortgageMean = mean(Mortgage16)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = MortgageMean, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(MortgageMean , digits = 2)), vjust = -.25))+
  scale_fill_manual (values = paletteLead)+ 
  plotTheme()+
  labs(x= "Presence of Lead", y= "Vacancy", 
       fill = "Presence of Lead", title = "Is Lead Window Presence correlated with mortgage?",
       subtitle= "Lead Windows Inspection Results and mortgage status",
       subtitle= "P value")+
  labs(caption= "T-Test p-value > 0.5")
##Maureen Question Two-------------------
Parcels$BUILD_YR <-as.numeric(as.character(as.factor(Parcels$BUILD_YR)))
Parcels <- filter(Parcels, BUILD_YR>0) 

densityBuildYear <- ggplot(Parcels, aes(BUILD_YR, fill = as.factor(Parcels$Lead_Hazar), color = as.factor(Parcels$Lead_Hazar))) +
  geom_density(alpha = 0.25) + scale_fill_manual (values = paletteLead, labels=c("Negative","Positive")) + 
  scale_color_manual (values = paletteLead, labels=c("Negative","Positive")) + 
  geom_vline(xintercept = 1978)+
  geom_text(aes(x=1978, label="1978: Lead-based Products Banned", y=.0225), vjust=-1, angle = 90, color="blue") +
  plotTheme() +
  labs(title= "Build Year Distribution by Lead Presence", y="", fill="Lead Presence") +
  guides(color=FALSE) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_line(color="grey70"),
        axis.text.x = element_text(size=8))

#graph
densitySalePrice <- ggplot(Parcels, aes(SALE_PRICE, fill = as.factor(Parcels$Lead_Hazar), color = as.factor(Parcels$Lead_Hazar))) +
  geom_density(alpha = 0.25) + scale_fill_manual (values = paletteLead, labels=c("Negative","Positive")) + 
  scale_color_manual (values = paletteLead, labels=c("Negative","Positive")) + 
  #geom_vline(xintercept = 1978)+
  #geom_text(aes(x=1978, label="1978: Lead-based Products Banned", y=.0225), vjust=-1, angle = 90, color="blue") +
  plotTheme() +
  labs(title= "Sale Price Distribution by Lead Presence", y="", fill="Lead Presence") +
  scale_x_continuous(limits = quantile(Parcels$SALE_PRICE, c(.0,.95))) +
  guides(color=FALSE) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_line(color="grey70"),
        axis.text.x = element_text(size=8))

#Putting SaleCount later after join happens

OwnerRenter <- Parcels %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(MeanOwnPCT = mean(OWNER_PCT1)) %>% 
  ggplot(., aes(x = Lead_Hazar, y = MeanOwnPCT,fill = as.factor(Parcels$Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01)) +
  geom_text(inherit.aes = TRUE, aes(label=paste(round(MeanOwnPCT,digits = 2), "%"), vjust=-.5)) +
  #scale_y_continuous(labels = dollar)+
  scale_fill_manual (values = paletteLead, labels=c("Negative", "Positive"))+
  labs(x= "Presence of Lead", y= "Mean Ownership Percent", 
       fill = "Presence of Lead", title = "Mean Ownership Percent by Presence of Lead",
       caption = "T-Test p-value > 0.5")  +
  plotTheme()

Std_70 <- read.csv("SpreadsheetData/Standardized_1970.csv")
Std_70 <- filter(Std_70, state == "MN", county == "Hennepin County")
head(Std_70)
Std_70_select <- dplyr::select(Std_70,TRTID10,tract,POP70,HU70,VAC70,OWN70,RENT70)
head(Std_70_select)
tractshp$GEOID <- as.numeric(tractshp$GEOID)
Min70 <- left_join(tractshp, Std_70_select, by=c("GEOID" = "TRTID10"))
head(Min70)

Std_80 <- read.csv("SpreadsheetData/Standardized_1980.csv")
Std_80 <- filter(Std_80, state == "MN", county == "Hennepin County")
head(Std_80)
Std_80_select <- dplyr::select(Std_80,TRTID10,tract,POP80,HU80,VAC80,MHMVAL80,MRENT80,OWN80,RENT80)
head(Std_80_select)
Min80 <- left_join(tractshp, Std_80_select, by=c("GEOID" = "TRTID10"))
head(Min80)

Std_90 <- read.csv("SpreadsheetData/Standardized_1990.csv")
Std_90 <- filter(Std_90, state == "MN", county == "Hennepin County")
head(Std_90)
Std_90_select <- dplyr::select(Std_90,TRTID10,tract,POP90,HU90,VAC90,MHMVAL90,MRENT90, OWN90,RENT90)
head(Std_90_select)
Min90 <- left_join(tractshp, Std_90_select, by=c("GEOID" = "TRTID10"))
head(Min90)

Std_00 <- read.csv("SpreadsheetData/Standardized_2000.csv")
Std_00 <- filter(Std_00, state == "MN", county == "Hennepin County")
head(Std_00)
Std_00_select <- dplyr::select(Std_00,TRTID10,tract,POP00,HU00,VAC00,FAMILY00,FHH00, OWN00,RENT00)
head(Std_00_select)
Min00 <- left_join(tractshp, Std_00_select, by=c("GEOID" = "TRTID10"))
head(Min00)

Std_10 <- read.csv("SpreadsheetData/Standardized_2010.csv")
Std_10 <- filter(Std_10, state == "MN", county == "Hennepin County")
head(Std_10)
Std_10_select <- dplyr::select(Std_10,tractid,tract,pop10,hu10,vac10,family10,fhh10, own10,rent10)
head(Std_10_select)
Min10 <- left_join(tractshp, Std_10_select, by=c("GEOID" = "tractid"))
head(Min10)
v2010 <- get_acs(geography = "tract", variables = "B06011_001", state = "MN", county = "Hennepin", year = 2012)

head(v2010)
v2010 <- dplyr::select(v2010, GEOID, estimate)
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

head(TractInfo)

#Median Value Change Variables
TractInfo <- mutate(TractInfo, Value80_90 = (MHMVAL80/POP80) - (MHMVAL90/POP90))
#TractInfo <- mutate(TractInfo, Value90_00 = (MHMVAL90/POP90) - (FAMILY00/POP00))
#TractInfo <- mutate(TractInfo, Value00_10 = (FAMILY00/POP00) - (family10/pop10))
#TractInfo <- mutate(TractInfo, Value90_16 = (MHMVAL90/POP90) - (MedVal16/Pop16))
#vacancy change variables
TractInfo <- mutate(TractInfo, Vacancy80_90 = (VAC80/(VAC80+OWN80+RENT80) - (VAC90/(VAC90+OWN90+RENT90))))
TractInfo <- mutate(TractInfo, Vacancy90_00 = (VAC90/(VAC90+OWN90+RENT90) - (VAC00/(VAC00+OWN00+RENT00))))
TractInfo <- mutate(TractInfo, Vacancy00_10 = (VAC00/(VAC00+OWN00+RENT00) - (vac10/(vac10+own10+rent10))))
#vacancy change variables
TractInfo <- mutate(TractInfo, Rent80_90 = (RENT80/(VAC80+OWN80+RENT80) - (RENT90/(VAC90+OWN90+RENT90))))
TractInfo <- mutate(TractInfo, Rent90_00 = (RENT90/(VAC90+OWN90+RENT90) - (RENT00/(VAC00+OWN00+RENT00))))
TractInfo <- mutate(TractInfo, Rent00_10 = (RENT00/(VAC00+OWN00+RENT00) - (rent10/(vac10+own10+rent10))))

#Test out mapping some variables
tractshp$GEOID <- as.numeric(tractshp$GEOID)
tractshp1 <- as.data.frame(tractshp)
tractshp1$GEOID <- as.numeric(tractshp1$GEOID)
tractshp_info <- left_join(tractshp1, TractInfo, by=c("GEOID" = "TRTID10"))

quantile <- quantile(tractshp_info$Rent90_00, na.rm=T)

tractshp_info1 <- st_as_sf(tractshp_info) %>% st_transform(3857)

parcels_with_blocks <- st_centroid(Parcels_w_blocks)

testjoin <- st_join(parcels_with_blocks, tractshp_info1)

Parcels_w_tracts <-
  testjoin %>%
  as.data.frame() %>% 
  dplyr::select(PID,tract,Lead_Hazar, POP70,VAC70,OWN70,RENT70,POP80,
                RENT80 , POP90, HU90, VAC90, MHMVAL90, MRENT90, OWN90, RENT90, POP00, 
                HU00, VAC00, FAMILY00, FHH00, OWN00, RENT00, pop10, hu10, vac10,
                own10, rent10, MEDINC, Educ16, Poverty16, MedianInc16, Earnings16, 
                Tenure16,Vacancy16, Mortgage16, Value80_90, 
                Vacancy80_90, Vacancy90_00, Vacancy00_10, Rent80_90, Rent90_00, Rent00_10)


VacancyChangeBar <- Parcels_w_tracts %>%
  group_by(Lead_Hazar) %>%
  dplyr::summarize(VacancyChange = mean((Vacancy80_90))) %>% 
  ggplot(., aes(x = Lead_Hazar, y = VacancyChange, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(VacancyChange * 100, digits = 2), "%"), vjust = 1))+
  #scale_fill_manual (values = paletteLead, labels = c("Negative", "Positive"))+
  labs(x= "Presence of Lead", y= "Percent Change in Vacancy", 
       fill = "Presence of Lead", title = "Lead Presence and Vacancy Change,\n1980 - 1990",
       caption = "T-Test p-value < 0.05")+
  plotTheme()

RentalGain <- Parcels_w_tracts %>% ###################################### this one - high rental gain associated with lead
  filter(Lead_Hazar != "2") %>% 
  group_by(Lead_Hazar) %>%
  dplyr::summarize(RentalChange = mean((Rent90_00))) %>% 
  ggplot(., aes(x = Lead_Hazar, y = RentalChange, fill = as.factor(Lead_Hazar))) +
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  geom_text(aes(label=paste(round(RentalChange * 100, digits = 2), "%"), vjust = -.25))+
  scale_fill_manual (values = paletteLead, labels=c("Negative","Positive"))+
  labs(x= "Presence of Lead", y= "Percent Change in Rental Units", 
       fill = "Presence of Lead", title = "Lead Presence and Rental Change, \n1990-2000",
       caption = "T-Test p-value < 0.05")+
  plotTheme()

RentalGainMap <- ggplot() + 
  geom_sf(data = tractshp_info1 %>% st_transform(26851), aes(fill = tractshp_info1$Rent90_00), color = 'white') +
  scale_fill_gradientn(colors = pal_blue5,
                       label=as.character(round(quantile(tractshp_info1$Rent90_00, na.rm=T), digits = 3)),
                       name="Rental Change\n (Quintile Breaks)") +
  geom_point(data=Parcels,
             aes(x=(st_coordinates(Parcels)[,1]), y=(st_coordinates(Parcels)[,2]), color= as.factor(Lead_Hazar)),size=0.5) +
  labs(title= "Are Lead Windows related with \nRental Change, 1990-2000?",
       subtitle= "Lead Windows Inspection Results and Rental Change in Minneapolis") +
  scale_color_manual(values = paletteLead,
                     labels=c("Negative","Positive"),
                     name = "Inspection Results") + 
  theme(panel.grid.major = element_line(colour = "white")) +
  mapTheme()

tractshp_info2 <- tractshp_info1 %>% st_transform(26851)

st_write(tractshp_info2, "Shapefiles/tract_Shapefile.shp")
st_write(Parcels, "Shapefiles/parcels_Shapefile.shp")

#join parcel level and tract level info together                            # [3.7.18 - new join + adding Land/Bldg variables]
Parcels_all <- left_join(Parcels2, Parcels_w_tracts2,  by=c("PID" = "PID")) 
#join parcel level and tract level info together 
Parcels_all <- left_join(Parcels_all, Parcels_w_blocks2, by=c("PID" = "PID"))

#join land and building variables too
str(Parcels_all)
parcelsLandBuilding_cond <- dplyr::select(parcelsLandBuilding, PID, SizeInSqFt, Neighborho, Community,  
                                          TotalValue, BuildingUs, Constructi, ExteriorTy, Bedrooms)
Parcels_all <- left_join(Parcels_all, parcelsLandBuilding_cond,  by=c("testedParcels_PID" = "PID")) 

# join sale and valuation data
#   Sales
MNSalesData$PID <- as.factor(as.character(MNSalesData$PID ))
Parcels_all <- left_join(Parcels_all, MNSalesData, by=c("PID"="PID")) 
#  Valuation
Parcels_all <- left_join(Parcels_all, Valuation_tojoin, by=c("PID"="PID")) 

# join  Permit data
Parcels_all <- left_join(Parcels_all, permitCount, by=c("PID"="apn"))                       
Parcels_all <- left_join(Parcels_all, permitCount2, by=c("PID"="apn"))
Parcels_all <- mutate(Parcels_all, PermitLead = ifelse(PermitCountLead > 0, 1, 0))
#may need to rename the PermitCount, PermitCountLead and PermitLead variables if have .x or .y on end
#Parcels_all <- rename(Parcels_all, PermitCountTotal = PermitCountTotal.x, PermitCountLead = PermitCountLead.x)
head(Parcels_all)


#Parcels_Zoning <- left_join(Parcels_all, ZoningDataFrame, by="PID")       #error. Maureen. LHS doesnt have PID
Parcels_Lead <- left_join(ZoningDataFrame2, minnea_LeadThree, by="PID")
Parcels_Foreclosure <- left_join(Parcels_Lead, minnea_foreclosureOne, by="PID")
Parcels_ResComp <- left_join(Parcels_Foreclosure, minnea_compFive, by="PID")
Parcels_Vacancy <- left_join(Parcels_ResComp, minnea_vacancyFive, by="PID")
Parcels_final <- left_join(Parcels_all, Parcels_Vacancy, by="PID")

names(Parcels_final)

# select out just the relevant variables                                 # error - new variables missing
Parcels_final <- Parcels_final %>% 
  dplyr::select(c(PID 	,BUILD_YR.x.x 	,SALE_DATE.x.x 	,SALE_PRICE.x.x 	,TOT_PENALT.x.x ,
                  PR_TYP_NM1.x.x	,OWNER_PCT1.x.x	,QUAL_IMPR1.x.x 	, 
                  Lead_Hazar.x.x.x 	, PermitCountTotal 	,
                  PermitCountLead 	,PermitLead 	,POP70 	,VAC70 	,OWN70 	,
                  RENT70 	,POP80 	,RENT80 	,POP90 	,HU90 	,VAC90 	,MHMVAL90 	,
                  MRENT90 	,OWN90 	,RENT90 	,POP00 	,HU00 	,VAC00 	,FAMILY00 	,
                  FHH00 	,OWN00 	,RENT00 	,pop10 	,hu10 	,vac10 	,own10 	,rent10 ,
                  MEDINC 	,Educ16.x 	,Poverty16.x 	,MedianInc16.x 	,Earnings16.x 	,
                  Tenure16.x 	,Vacancy16.x 	,Mortgage16.x 	,Value80_90 	,
                  Vacancy80_90 	,Vacancy90_00 	,Vacancy00_10 	,Rent80_90 	,
                  Rent90_00 	,Rent00_10 	,ChildUnder5 	,Child5to9 	,SizeInSqFt 	,
                  Neighborho 	,Community 	,TotalValue 	,BuildingUs 	,
                  Constructi 	,ExteriorTy 	,Bedrooms 	,density 	,
                  d_Lead	,d_vacant	,d_ResComp	,d_Foreclosure	,
                  yearsold, yearsincesold, price_max, salecountTotal, price_min, pricechange, holc_grade))


### clean up levels
Parcels_final <- filter(Parcels_final, BuildingUs %in% c("2 Fam. Conv. Sgl. Dwlg.", "Apartment 4 or 5 Unit","Apartment 6+ Unit", 
                                                         "Apartment Converted","Double Bungalow", "Duplex", "Multi-Family & Rooms",
                                                         "Single Fam. Dwlg.", "Tri-plex"))
Parcels_final <- filter(Parcels_final, !PR_TYP_NM1.x.x %in% c("LOW INCOME RENTAL", "VACANT LAND-RESIDENTIAL"))

Parcels_finalOmit <- na.omit(Parcels_final)
Parcels_finalOmit <- Parcels_finalOmit %>% filter(as.numeric(as.character(BUILD_YR.x)) > 0000)

Parcels_finalOmit <- Parcels_finalOmit %>% mutate(Lead_Hazar = ifelse(Lead_Hazar.x.x.x == "2" | Lead_Hazar.x.x.x == "0", 0, 1))
Parcels_finalOmit$Lead_Hazar <- as.factor(as.character(Parcels_finalOmit$Lead_Hazar))
Parcels_final_year <- Parcels_finalOmit %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-Lead_Hazar.x.x.x)

model1 <- glm(Lead_Hazar ~ BUILD_YR.x, data=Parcels_final_year, family="binomial")
prediction_year <- predict(model1, Parcels_final_year)
Parcels_final_year$yearpredict <- prediction_year

aucCurve5 <- ggplot(Parcels_final_year, aes(d = as.numeric(as.character(Lead_Hazar)), m = as.numeric(yearpredict))) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="Year-Only Model ROC Curve", subtitle="AUC:.7103") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')

auc.score.year <- pROC::auc(Parcels_final_year$Lead_Hazar, Parcels_final_year$yearpredict)

Parcels_fo_year <- Parcels_finalOmit %>% filter(as.numeric(as.character(BUILD_YR.x)) < 1978)
Parcels_fo_year <- Parcels_fo_year %>% filter(as.numeric(as.character(BUILD_YR.x)) > 0000)
Parcels_fo_year$BUILD_YR.x <-as.integer(as.character(Parcels_fo_year$BUILD_YR.x))
#Parcels_fo_year$BUILD_YR.x <- (1978-Parcels_fo_year$BUILD_YR.x)
Parcels_fo_year$SALE_DATE.x <- as.integer(as.character(Parcels_fo_year$SALE_DATE.x))

Parcels_fo_year <- Parcels_fo_year %>% mutate(Lead_Hazar = ifelse(Lead_Hazar.x.x.x == "2" | Lead_Hazar.x.x.x == "0", 0, 1))
Parcels_fo_year$Lead_Hazar <- as.factor(as.character(Parcels_fo_year$Lead_Hazar))

# train$Lead_Hazar.x.x.x <- as.factor(train$Lead_Hazar.x.x.x)
# test$Lead_Hazar.x.x.x <- as.factor(test$Lead_Hazar.x.x.x)

write.csv(Parcels_finalOmit, f)

Parcels_parcels <- Parcels_fo_year[c(2:5, 7:12,55,58, 59, 62,63,65:74)]
Parcels_parcels$BUILD_YR.x <- as.integer(as.character(Parcels_parcels$BUILD_YR.x))
# Parcels_parcels$BUILD_YR.x <- (1978 - Parcels_parcels$BUILD_YR.x)
Parcels_parcels$SALE_DATE.x <- as.integer(as.character(Parcels_parcels$SALE_DATE.x))
Parcels_tracts <- Parcels_fo_year[c(9,13:38,46:52, 57, 74)]
Parcels_blocks <- Parcels_fo_year[c(9,39:45,53,54, 74)]

Parcels_parcels <- Parcels_fo_year[c(2:12,55,58, 59, 61:63,65:74)] 

PID2 <- as.data.frame(PID2)

inputStory1 <- Parcels_parcels %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-Lead_Hazar.x.x.x)
bind <- inputStory1 %>% filter(Lead_Hazar == 0)
inputStory1 <- rbind(inputStory1, bind, bind, bind)
inputStory1$BUILD_YR.x <- as.factor(as.character(inputStory1$BUILD_YR.x))

datTrain_Sub <- subset(inputStory1, select=-Lead_Hazar)
LeadHazard_train <- inputStory1$Lead_Hazar

fit_xgb <- xgboost(data.matrix(datTrain_Sub), as.numeric(as.character(LeadHazard_train)),
                   nrounds = 800,
                   eta =  0.001,
                   max_depth = 2,
                   gamma = 1,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1
                   , booster = "gbtree"
                   , eval_metric = "error"
                   , objective="binary:logistic")

thisPrediction = predict(fit_xgb,inputStory1 %>% dplyr::select(-Lead_Hazar) %>% data.matrix(), type="response")

inputStory1$prediction <- thisPrediction

auc.score <- pROC::auc(inputStory1$Lead_Hazar, inputStory1$prediction)

boruta.train <- Boruta(as.factor(as.character(Lead_Hazar)) ~ ., data = inputStory1 %>% dplyr::select(-prediction), doTrace = 0)

## save features
borutaVars <- getSelectedAttributes(boruta.train)     #	BUILD_YR.x.x.x. SALE_PRICE. MEDINC. d_Lead. d_Foreclosure.

boruta.formula <- formula(paste("as.factor(as.character(Lead_Hazar)) ~ ", paste(borutaVars, collapse = " + ")))

boruta.train <- glm(boruta.formula, data=inputStory1, family = binomial(link = "logit"))

borutaPrediction <- predict(boruta.train, inputStory1, type="response")

inputStory1$prediction1 <- borutaPrediction

auc.score1 <- pROC::auc(inputStory1$Lead_Hazar, inputStory1$prediction1)

model.null.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ 1,
                          data=inputStory1 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

model.full.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ .,
                          data= inputStory1 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

step <- step(model.null.ensemble,
             scope = list(upper=model.full.ensemble),
             direction="both",
             test="Chisq",
             data=Data)
formula <- step$formula

stepwise.train <- glm(formula, data=inputStory1, family=binomial(link="logit"))
stepwise.prediction <- predict(stepwise.train, inputStory1, type = "response")
inputStory1$prediction2 <- stepwise.prediction

auc.score2 <- pROC::auc(inputStory1$Lead_Hazar, inputStory1$prediction2)

all.train <- glm(as.factor(as.character(Lead_Hazar)) ~ .,
                 data= inputStory1 %>% dplyr::select(-prediction, -prediction1, -prediction2),
                 family = binomial(link="logit"))
all.predict <- predict(all.train,inputStory1,type="response")

inputStory1$prediction3 <-all.predict

auc.score3 <- pROC::auc(inputStory1$Lead_Hazar, inputStory1$prediction3)

inputStory2 <- Parcels_blocks %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-Lead_Hazar.x.x.x)
bind <- inputStory2 %>% filter(Lead_Hazar == 0)
inputStory2 <- rbind(inputStory2, bind, bind, bind)

datTrain_Sub <- subset(inputStory2, select=-Lead_Hazar)
LeadHazard_train <- as.numeric(as.character(inputStory2$Lead_Hazar))

fit_xgb <- xgboost(data.matrix(datTrain_Sub), LeadHazard_train,
                   nrounds = 800,
                   eta =  0.001,
                   max_depth = 2,
                   gamma = 1,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1
                   , booster = "gbtree"
                   , eval_metric = "error"
                   , objective="binary:logistic")

thisPrediction = predict(fit_xgb,inputStory2 %>% dplyr::select(-Lead_Hazar) %>% data.matrix(), type="response")

inputStory2$prediction <- thisPrediction

auc.score.1 <- pROC::auc(inputStory2$Lead_Hazar, inputStory2$prediction)

boruta.train <- Boruta(as.factor(as.character(Lead_Hazar)) ~ ., data = inputStory2 %>% dplyr::select(-prediction), doTrace = 0)

## save features
borutaVars <- getSelectedAttributes(boruta.train)     #	BUILD_YR.x.x.x. SALE_PRICE. MEDINC. d_Lead. d_Foreclosure.

boruta.formula <- formula(paste("as.factor(as.character(Lead_Hazar)) ~ ", paste(borutaVars, collapse = " + ")))

boruta.train <- glm(boruta.formula, data=inputStory2, family = binomial(link = "logit"))

borutaPrediction <- predict(boruta.train, inputStory2, type="response")

inputStory2$prediction1 <- borutaPrediction

auc.score1.1 <- pROC::auc(inputStory2$Lead_Hazar, inputStory2$prediction1)

model.null.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ 1,
                          data=inputStory2 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

model.full.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ .,
                          data= inputStory2 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

step <- step(model.null.ensemble,
             scope = list(upper=model.full.ensemble),
             direction="both",
             test="Chisq",
             data=Data)
formula <- step$formula

stepwise.train <- glm(formula, data=inputStory2, family=binomial(link="logit"))
stepwise.prediction <- predict(stepwise.train, inputStory2, type = "response")
inputStory2$prediction2 <- stepwise.prediction

auc.score2.1 <- pROC::auc(inputStory2$Lead_Hazar, inputStory2$prediction2)

all.train <- glm(as.factor(as.character(Lead_Hazar)) ~ .,
                 data= inputStory2 %>% dplyr::select(-prediction, -prediction1, -prediction2),
                 family = binomial(link="logit"))
all.predict <- predict(all.train,inputStory2,type="response")

inputStory2$prediction3 <-all.predict

auc.score3.1 <- pROC::auc(inputStory2$Lead_Hazar, inputStory2$prediction3)

inputStory3 <- Parcels_tracts %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-Lead_Hazar.x.x.x)
bind <- inputStory3 %>% filter(Lead_Hazar == 0)
inputStory3 <- rbind(inputStory3, bind, bind, bind)


datTrain_Sub <- subset(inputStory3, select=-Lead_Hazar)
LeadHazard_train <- as.numeric(as.character(inputStory3$Lead_Hazar))

fit_xgb <- xgboost(data.matrix(datTrain_Sub), LeadHazard_train,
                   nrounds = 800,
                   eta =  0.001,
                   max_depth = 2,
                   gamma = 1,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1
                   , booster = "gbtree"
                   , eval_metric = "error"
                   , objective="binary:logistic")

thisPrediction = predict(fit_xgb,inputStory3 %>% dplyr::select(-Lead_Hazar) %>% data.matrix(), type="response")

inputStory3$prediction <- thisPrediction

auc.score.2 <- pROC::auc(inputStory3$Lead_Hazar, inputStory3$prediction)

boruta.train <- Boruta(as.factor(as.character(Lead_Hazar)) ~ ., data = inputStory3 %>% dplyr::select(-prediction), doTrace = 0)

## save features
borutaVars <- getSelectedAttributes(boruta.train)     #	BUILD_YR.x.x.x. SALE_PRICE. MEDINC. d_Lead. d_Foreclosure.

boruta.formula <- formula(paste("as.factor(as.character(Lead_Hazar)) ~ ", paste(borutaVars, collapse = " + ")))

boruta.train <- glm(boruta.formula, data=inputStory3, family = binomial(link = "logit"))

borutaPrediction <- predict(boruta.train, inputStory3, type="response")

inputStory3$prediction1 <- borutaPrediction

auc.score1.2 <- pROC::auc(inputStory3$Lead_Hazar, inputStory3$prediction1)

model.null.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ 1,
                          data=inputStory3 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

model.full.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ .,
                          data= inputStory3 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

step <- step(model.null.ensemble,
             scope = list(upper=model.full.ensemble),
             direction="both",
             test="Chisq",
             data=Data)
formula <- step$formula

stepwise.train <- glm(formula, data=inputStory3, family=binomial(link="logit"))
stepwise.prediction <- predict(stepwise.train, inputStory3, type = "response")
inputStory3$prediction2 <- stepwise.prediction

auc.score2.2 <- pROC::auc(inputStory3$Lead_Hazar, inputStory3$prediction2)

all.train <- glm(as.factor(as.character(Lead_Hazar)) ~ .,
                 data= inputStory3 %>% dplyr::select(-prediction, -prediction1, -prediction2),
                 family = binomial(link="logit"))
all.predict <- predict(all.train,inputStory3,type="response")

inputStory3$prediction3 <-all.predict

auc.score3.2 <- pROC::auc(inputStory3$Lead_Hazar, inputStory3$prediction3)

inputStory4.1 <- Parcels_fo_year %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-c(Lead_Hazar.x.x.x, d_Lead, PR_TYP_NM1.x, Neighborho, ExteriorTy, Constructi))
PID <- Parcels_fo_year %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(PID, Lead_Hazar)
PID1 <- PID %>% filter(Lead_Hazar == 0)
PID2 <- rbind(PID, PID1, PID1, PID1) %>% dplyr::select(-Lead_Hazar)
bind <- inputStory4.1 %>% filter(Lead_Hazar == 0)
inputStory4.1 <- rbind(inputStory4.1, bind, bind, bind)

inputStory5.1 <- Parcels_fo_year %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(Neighborho, Lead_Hazar)
bind <- inputStory5.1 %>% filter(Lead_Hazar == 0)
inputStory5.1 <- rbind(inputStory5.1, bind, bind, bind) %>% dplyr::select(-Lead_Hazar)
inputStory5.1$Neighborho1 <- as.numeric(inputStory5.1$Neighborho)


datTrain_Sub <- subset(inputStory4.1, select=-Lead_Hazar)
LeadHazard_train <- inputStory4.1$Lead_Hazar

fit_xgb <- xgboost(data.matrix(datTrain_Sub), as.numeric(as.character(LeadHazard_train)),
                   nrounds = 800,
                   eta =  0.001,
                   max_depth = 2,
                   gamma = 1,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1
                   , booster = "gbtree"
                   , eval_metric = "error"
                   , objective="binary:logistic")

thisPrediction = predict(fit_xgb, inputStory4.1 %>% dplyr::select(-Lead_Hazar) %>% data.matrix(), type="response")


inputStory4.1$prediction <- thisPrediction

auc.score.4.1 <- pROC::auc(inputStory4.1$Lead_Hazar, inputStory4.1$prediction)

boruta.train <- Boruta(as.factor(as.character(Lead_Hazar)) ~ ., data = inputStory4.1 %>% dplyr::select(-prediction), doTrace = 0)

## save features
borutaVars <- getSelectedAttributes(boruta.train)     #	BUILD_YR.x.x.x. SALE_PRICE. MEDINC. d_Lead. d_Foreclosure.

boruta.formula <- formula(paste("as.factor(as.character(Lead_Hazar)) ~ ", paste(borutaVars, collapse = " + ")))

boruta.train <- glm(boruta.formula, data=inputStory4.1, family = binomial(link = "logit"))

borutaPrediction <- predict(boruta.train, inputStory4.1, type="response")

inputStory4.1$prediction1 <- borutaPrediction

auc.score1.4.1 <- pROC::auc(inputStory4.1$Lead_Hazar, inputStory4.1$prediction1)

model.null.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ 1,
                          data=inputStory4.1 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

model.full.ensemble = glm(as.factor(as.character(Lead_Hazar)) ~ .,
                          data= inputStory4.1 %>% dplyr::select(-prediction, -prediction1),
                          family = binomial(link="logit")
)

step <- step(model.null.ensemble,
             scope = list(upper=model.full.ensemble),
             direction="both",
             test="Chisq",
             data=Data)
formula <- step$formula


stepwise.train <- glm(formula, data=inputStory4.1, family=binomial(link="logit"))
stepwise.prediction <- predict(stepwise.train, inputStory4.1, type = "response")
inputStory4.1$prediction2 <- stepwise.prediction

matrix.coeff <- as.matrix(stepwise.train$coefficients,stepwise.train$coefficients,stepwise.train$coefficients,
                          stepwise.train$coefficients,stepwise.train$coefficients,stepwise.train$coefficients,
                          stepwise.train$coefficients,stepwise.train$coefficients,stepwise.train$coefficients,
                          stepwise.train$coefficients,stepwise.train$coefficients)

auc.score2.4.1 <- pROC::auc(inputStory4.1$Lead_Hazar, inputStory4.1$prediction2)


all.train <- glm(as.factor(as.character(Lead_Hazar)) ~ .,
                 data= inputStory4.1 %>% dplyr::select(-c(prediction2, prediction, prediction1)),
                 family = binomial(link="logit"))
all.predict <- predict(all.train,inputStory4.1, type="response")

inputStory4.1$prediction3 <-all.predict

auc.score3.4.1 <- pROC::auc(inputStory4.1$Lead_Hazar, inputStory4.1$prediction3)

testall <- predict(all.train,  Parcels_fo_year %>% filter(Lead_Hazar.x.x.x == "2") %>% droplevels() %>% dplyr::select(-c(Lead_Hazar, Lead_Hazar.x.x.x, PID, d_Lead, PR_TYP_NM1.x, Neighborho, ExteriorTy, Constructi)), type="response")

auc.data <- data.frame()
auc.data <- cbind(c("XGBoost", "Boruta", "Stepwise", "Kitchen Sink"))
auc.data <- cbind(auc.data, c(auc.score, auc.score1, auc.score2, auc.score3), 
                  c(auc.score.1, auc.score1.1, auc.score2.1, auc.score3.1),
                  c(auc.score.2, auc.score1.2, auc.score2.2, auc.score3.2),
                  c(auc.score.4.1, auc.score1.4.1, auc.score2.4.1, auc.score3.4.1))

myresult_parcels <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  inputStory1$predClass1<- ifelse(inputStory1$prediction3 > i ,1,0)
  sens <- sensitive(as.factor(inputStory1$predClass1), as.factor(inputStory1$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_parcels <- rbind(myresult_parcels,thisresult)
}
myresult_parcels

myresult_blocks <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  inputStory2$predClass1<- ifelse(inputStory2$prediction > i ,1,0)
  sens <- sensitive(as.factor(inputStory2$predClass1), as.factor(inputStory2$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_blocks <- rbind(myresult_blocks,thisresult)
}
myresult_blocks

myresult_tracts <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  inputStory3$predClass1<- ifelse(inputStory3$prediction3 > i ,1,0)
  sens <- sensitive(as.factor(inputStory3$predClass1), as.factor(inputStory3$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_tracts <- rbind(myresult_tracts,thisresult)
}
myresult_tracts

myresult_all <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  inputStory4.1$predClass1<- ifelse(inputStory4.1$stepwise_predict > i ,1,0)
  sens <- sensitive(as.factor(inputStory4.1$predClass1), as.factor(inputStory4.1$Lead_Hazar))
  spec <- specific(as.factor(inputStory4.1$predClass1), as.factor(inputStory4.1$Lead_Hazar))
  step1 <- confusionMatrix(as.factor(inputStory4.1$predClass1), as.factor(inputStory4.1$Lead_Hazar))
  overall <- step1$overall
  acc <- overall['Accuracy']
  thisresult <- data.frame(i, sens, spec, acc)
  myresult_all <- rbind(myresult_all,thisresult)
}
myresult_all

write.csv(myresult_all, "SpreadsheetData/ssa.csv")

myresult_all1 <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  inputStory4.1$predClass1<- ifelse(inputStory4.1$prediction3 > i ,1,0)
  sens <- sensitive(as.factor(inputStory4.1$predClass1), as.factor(inputStory4.1$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_all1 <- rbind(myresult_all1,thisresult)
}
myresult_all1

testing <- inputStory4.1

testing$predClass = ifelse(testing$prediction3 > .15 ,1,0)
testing$Correct = ifelse(testing$predClass == testing$Lead_Hazar, "Correct", "Incorrect")
testing$confuse = ifelse((testing$predClass == 0 & testing$Lead_Hazar == 0), "True Negative", ifelse((testing$predClass == 1 & testing$Lead_Hazar == 1), "True Positive", ifelse((testing$predClass == 0 & testing$Lead_Hazar == 1), "False Negative", "False Positive")))

myresult_parcels1 <- data.frame()
for (i in c(.3, .31, .32, .33, .34, .35)){
  i
  inputStory1$predClass1<- ifelse(inputStory1$prediction > i ,1,0)
  sens <- sensitive(as.factor(inputStory1$predClass1), as.factor(inputStory1$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_parcels1 <- rbind(myresult_parcels1,thisresult)
}
myresult_parcels1
#parcel cutoff .34

myresult_blocks1 <- data.frame()
for (i in c(.5, .51, .52, .53, .54, .55)){
  i
  inputStory2$predClass1<- ifelse(inputStory2$prediction > i ,1,0)
  sens <- sensitive(as.factor(inputStory2$predClass1), as.factor(inputStory2$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_blocks1 <- rbind(myresult_blocks1,thisresult)
}
myresult_blocks1
#blocks cutoff .53

myresult_tracts1 <- data.frame()
for (i in c(.3, .31, .32, .33, .34, .35, .36, .37, .38, .39, .4)){
  i
  inputStory3$predClass1<- ifelse(inputStory3$prediction3 > i ,1,0)
  sens <- sensitive(as.factor(inputStory3$predClass1), as.factor(inputStory3$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_tracts1 <- rbind(myresult_tracts1,thisresult)
}
myresult_tracts1


bestPrediction <- prediction(inputStory4.1$prediction1, inputStory4.1$Lead_Hazar)
#tracts cutoff .36

roc.perf = performance(bestPrediction, measure = "sens", x.measure="spec")
plot(roc.perf)
abline(a=0,b=1)

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
#This will print the optimal cut-off point and the corresponding
#specificity and sensitivity 
print(opt.cut(roc.perf, bestPrediction))

inputStory4.1$BYF <- as.factor(as.character(inputStory4.1$BUILD_YR.x))
yrmod <- glm(Lead_Hazar ~ BYF, data=inputStory4.1, family=binomial(link="logit"))
yearmodpred <- predict(yrmod, inputStory4.1, type = "response")
inputStory4.1$prediction4 <- yearmodpred

auc1 <- cbind(as.numeric(as.character(inputStory4.1$Lead_Hazar)), inputStory4.1$prediction, model="XGBoost Model ROC Curve")
auc2 <- cbind(as.numeric(as.character(inputStory4.1$Lead_Hazar)), inputStory4.1$prediction1, model="Boruta Model ROC Curve")
auc3 <- cbind(as.numeric(as.character(inputStory4.1$Lead_Hazar)), inputStory4.1$prediction2, model="Stepwise Model ROC Curve")
auc4 <- cbind(as.numeric(as.character(inputStory4.1$Lead_Hazar)), inputStory4.1$prediction3, model="Kitchen Sink Model ROC Curve")
auc5 <- cbind(as.numeric(as.character(inputStory4.1$Lead_Hazar)), inputStory4.1$prediction4, model="Build Year Only Model ROC Curve")
allAUC <- rbind(auc1, auc2, auc3, auc4, auc5) %>% as.data.frame()

write.csv(allAUC, "SpreadsheetData/allAUC.csv")

aucCurve1 <- ggplot(inputStory4.1, aes(d = as.numeric(as.character(Lead_Hazar)), m = as.numeric(prediction))) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="XGBoost Model ROC Curve") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')
aucCurve2 <- ggplot(inputStory4.1, aes(d = as.numeric(as.character(Lead_Hazar)), m = as.numeric(prediction1))) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="Boruta Model ROC Curve") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')
aucCurve3 <- ggplot(inputStory4.1, aes(d = as.numeric(as.character(Lead_Hazar)), m = as.numeric(prediction2))) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="Stepwise Model ROC Curve") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')
aucCurve4 <- ggplot(inputStory4.1, aes(d = as.numeric(as.character(Lead_Hazar)), m = as.numeric(prediction3))) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="All Variables Model ROC Curve") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')
grid.arrange(aucCurve1, aucCurve2, aucCurve3, aucCurve4, nrow=2, ncol=2)



ensembleFunction <- function(inputStory, iterations, sampleSize) {
  #create an empty dataframe
  endList <- data.frame(matrix(NA, nrow = nrow(inputStory %>% filter(Lead_Hazar.x.x.x == "2")), ncol = 0))
  inputStory1 <- inputStory %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-Lead_Hazar.x.x.x)
  bind <- inputStory1 %>% filter(Lead_Hazar == 0)
  inputStory1 <- rbind(inputStory1, bind, bind, bind)
  datTrain_Sub <- subset(inputStory1, select=-Lead_Hazar)
  LeadHazard_train <- as.numeric(as.character(inputStory1$Lead_Hazar))
  
  fit_xgb <- xgboost(data.matrix(datTrain_Sub), LeadHazard_train,
                     nrounds = 800,
                     eta =  0.001,
                     max_depth = 2,
                     gamma = 1,
                     colsample_bytree = 1,
                     min_child_weight = 1,
                     subsample = 1
                     , booster = "gbtree"
                     , eval_metric = "error"
                     , objective="binary:logistic")
  #build n models with a m% test set
  for (i in 1: iterations) {
    # sample <- sample.int(n = nrow(inputStory1), size = floor(sampleSize *nrow(inputStory1)), replace = F)
    # train <- inputStory1[sample, ]
    # #train the model
    # thisTrain <- glm(boruta.formula, data = train, family = binomial(link = "logit"))
    #create a vector of predictions for this model.
    thisPrediction = predict(fit_xgb, Parcels_fo_year %>% filter(Lead_Hazar.x.x.x == "2") %>% dplyr::select(-Lead_Hazar.x.x.x, -Lead_Hazar) %>% data.matrix(), type="response")
    #append the predictions to the data frame         
    endList <- cbind(endList, thisPrediction)
  }
  #label each prediction column 1 through n
  colnames(endList) <- paste("prediction", 1:ncol(endList), sep = "")
  #create a data frame of average, median and sd predictions. I have turned off the latter two for now
  return(data.frame(mean.Prediction = rowMeans(endList)))
}

ensembleFunction1 <- function(inputStory, iterations, sampleSize) {
  #create an empty dataframe
  endList <- data.frame(matrix(NA, nrow = nrow(inputStory %>% filter(Lead_Hazar.x.x.x == "2")), ncol = 0))
  inputStory1 <- inputStory %>% filter(Lead_Hazar.x.x.x == "0" | Lead_Hazar.x.x.x == "1") %>% droplevels() %>% dplyr::select(-Lead_Hazar.x.x.x)
  bind <- inputStory1 %>% filter(Lead_Hazar == 0)
  inputStory1 <- rbind(inputStory1, bind, bind, bind)
  forumla <- glm(Lead_Hazar ~ . , data=inputStory1, family = binomial(link="logit"))
  #build n models with a m% test set
  for (i in 1: iterations) {
     sample <- sample.int(n = nrow(inputStory1), size = floor(sampleSize *nrow(inputStory1)), replace = F)
     train <- inputStory1[sample, ]
     #train the model
     thisTrain <- glm(forumla, data = train, family = binomial(link = "logit"))
    #create a vector of predictions for this model.
    thisPrediction = predict(thisTrain, Parcels_fo_year %>% filter(Lead_Hazar.x.x.x == "2") %>% dplyr::select(-Lead_Hazar.x.x.x, -Lead_Hazar), type="response")
    #append the predictions to the data frame         
    endList <- cbind(endList, thisPrediction)
  }
  #label each prediction column 1 through n
  colnames(endList) <- paste("prediction", 1:ncol(endList), sep = "")
  #create a data frame of average, median and sd predictions. I have turned off the latter two for now
  return(data.frame(mean.Prediction = rowMeans(endList)))
}

Parcels_parcels$BUILD_YR.x <- as.factor(Parcels_parcels$BUILD_YR.x)

allStories.predictions <- 
  data.frame(
    cbind(
      story1.mean.predictions = ((ensembleFunction1(Parcels_parcels,100,1)[,1])),
      story2.mean.predictions = ((ensembleFunction(Parcels_blocks,100,1)[,1])),
      story3.mean.predictions = ((ensembleFunction1(Parcels_tracts,100,1)[,1])),
      dependent = as.factor(as.character(Parcels_fo_year$Lead_Hazar)),
      filter = as.factor(as.character(Parcels_fo_year$Lead_Hazar.x.x.x))
    ))

allStories.predictions$dependent <- ifelse(allStories.predictions$dependent == 1, 0, 2) %>% as.factor()
allStories.predictions$dependent <- ifelse(allStories.predictions$dependent == 2, 1, 0) %>% as.factor()
allStories.predictions$parcels <- ifelse(allStories.predictions$story1.mean.predictions > .34,1,0)
allStories.predictions$blocks <- ifelse(allStories.predictions$story2.mean.predictions > .53,1,0)
allStories.predictions$tracts <- ifelse(allStories.predictions$story3.mean.predictions > .25,1,0)

train <- allStories.predictions %>% filter(filter != "3") %>% dplyr::select(-filter)
test <- allStories.predictions %>% filter(filter == "3") %>% dplyr::select(-filter)

myresult_parcels_ensemble <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  train$predClass1 <- ifelse(train$story1.mean.predictions > i ,1,0)
  sens <- sensitive(as.factor(train$predClass1), as.factor(train$dependent))
  thisresult <- data.frame(i, sens)
  myresult_parcels_ensemble <- rbind(myresult_parcels_ensemble,thisresult)
}
myresult_parcels_ensemble

myresult_blocks_ensemble <- data.frame()
for (i in c(.55, .551, .552, .553, .56, .57, .58, .59, .6)){
  i
  train$predClass1 <- ifelse(train$story2.mean.predictions > i ,1,0)
  sens <- sensitive(as.factor(train$predClass1), as.factor(train$dependent))
  thisresult <- data.frame(i, sens)
  myresult_blocks_ensemble <- rbind(myresult_blocks_ensemble,thisresult)
}
myresult_blocks_ensemble

myresult_tracts_ensemble <- data.frame()
for (i in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95)){
  i
  train$predClass1 <- ifelse(train$story3.mean.predictions > i ,1,0)
  sens <- sensitive(as.factor(train$predClass1), as.factor(train$dependent))
  thisresult <- data.frame(i, sens)
  myresult_tracts_ensemble <- rbind(myresult_tracts_ensemble,thisresult)
}
myresult_tracts_ensemble

sample_new = sample.split(allStories.predictions, SplitRatio = .8)
datTrain_new = subset(allStories.predictions, sample_new == TRUE)
datTest_new = subset(allStories.predictions, sample_new == FALSE)

finalReg <- glm(dependent ~ .,data=train %>% dplyr::select(-c(parcels,tracts, blocks)),family=binomial)
ensemble_prediction_train <- predict(finalReg, train, type="response")
ensemble_prediction <- predict(finalReg, test, type="response")

train$predicted <- ensemble_prediction_train
test$predicted <- ensemble_prediction

testProbs<- data.frame(Lead = as.numeric(inputStory4.1$Lead_Hazar), Probs =inputStory4.1$prediction3, model="Stepwise") %>% 
  mutate(leadLabel = ifelse(Lead == 1, "Negative", "Positive"))

testProbs_omg <- data.frame(Lead = as.numeric(train$dependent), Probs =train$predicted, model="Ensemble") %>% 
  mutate(leadLabel = ifelse(Lead == 1, "Negative", "Positive"))

adminPropsPlot_omg <- ggplot(testProbs_omg, aes(x = Probs, fill=as.factor(Lead))) + geom_density(aes(y = ..count..)) +
  facet_grid(leadLabel ~ .) + xlab("Positive Result Probability") + scale_fill_manual(values=c("#477371", "yellow"), labels = c("Negative", "Positive")) + labs(fill="Lead Presence", title="Stepwise-Selected Variables") + plotTheme() + scale_x_continuous(limits=c(0,1))
adminPropsPlot_omg

#plot the distrubtion of predictied probabilities for each binary class - 0 and 1.
adminPropsPlot <- ggplot(testProbs, aes(x = Probs, fill=as.factor(Lead))) + geom_density(aes(y = ..count..)) +
  facet_grid(leadLabel ~ .) + xlab("Positive Result Probability") + scale_fill_manual(values=c("#477371", "yellow"), labels = c("Negative", "Positive")) + labs(fill="Lead Presence", title="Stepwise-Selected Variables") + plotTheme() + scale_x_continuous(limits=c(0,1))
adminPropsPlot

train$predClass = ifelse(train$predicted > .6734 ,1,0)
train$Correct = ifelse(train$predClass == train$dependent, "Correct", "Incorrect")
train$confuse = ifelse((train$predClass == 0 & train$dependent == 0), "True Negative", ifelse((train$predClass == 1 & train$dependent == 1), "True Positive", ifelse((train$predClass == 0 & train$dependent == 1), "False Negative", "False Positive")))
myresult_ensemble <- data.frame()
for (i in c(.44, 0.5, .55, .65, .7, .75)){
  i
  inputStory1$predClass<- ifelse(inputStory1$prediction > i ,1,0)
  sens <- sensitive(as.factor(inputStory1$predClass), as.factor(inputStory1$Lead_Hazar))
  thisresult <- data.frame(i, sens)
  myresult_ensemble <- rbind(myresult_ensemble,thisresult)
}

sensitive <- function (actuals, predictedScores, threshold = 0.5) 
{
  predicted_dir <- ifelse(as.numeric(as.character(predictedScores)) < threshold, 0, 1)
  actual_dir <- actuals
  no_without_and_predicted_to_not_have_event <- sum(actual_dir != 
                                                      1 & predicted_dir != 1, na.rm = T)
  no_without_event <- sum(actual_dir != 1, na.rm = T)
  return(no_without_and_predicted_to_not_have_event/no_without_event)
}

specific <- function (actuals, predictedScores, threshold = 0.5) 
{
  predicted_dir <- ifelse(as.numeric(as.character(predictedScores)) < threshold, 0, 1)
  actual_dir <- actuals
  no_with_and_predicted_to_have_event <- sum(actual_dir == 
                                               1 & predicted_dir == 1, na.rm = T)
  no_with_event <- sum(actual_dir == 1, na.rm = T)
  return(no_with_and_predicted_to_have_event/no_with_event)
}

ctrl <- trainControl(method = "repeatedcv", number = 99, savePredictions = TRUE)

mod_fit <- train(formula,  data=inputStory4.1, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
cv_auc <- mod_fit$pred
new <- (predict(mod_fit, type = "prob")) %>% mutate(relate= ifelse(new$`0` > new$`1`, new$`1`, new$`0`))

cv_auc$predict <- new$relate
table(cv_auc$Resample)

auc_crossvals <- data.frame()
for (i in seq(1,9)){
  i
  name <- paste0("Fold0",i,".Rep1")
  
  cv_auc1 <- cv_auc %>% 
    dplyr::filter(Resample == name) %>% 
    mutate(auc = pROC::auc(as.numeric(obs), as.numeric(predict)))
  auc_crossvals <- rbind(auc_crossvals, cv_auc1 %>% dplyr::select(auc) %>% unique())
  }
for (i in seq(10,99)){
  i
  name <- paste0("Fold",i,".Rep1")
  cv_auc1 <- cv_auc %>% 
    dplyr::filter(Resample == name) %>% 
    mutate(auc = pROC::auc(as.numeric(obs), as.numeric(predict)))
  auc_crossvals <- rbind(auc_crossvals, cv_auc1 %>% dplyr::select(auc) %>% unique())
}
ggplot()+geom_histogram(data=auc_crossvals, aes(x=auc), bins=10, color="grey70", fill="#154f4a")+scale_x_continuous(limits=c(0,1)) + labs(title="Cross-Validated Area Under the Curve")
write.csv(inputStory4.1, "SpreadsheetData/crossvalidation.csv")
crossvalid <- read.csv("SpreadsheetData/crossvalidation.csv") %>% select(-X)
spatialCV <- function(dataFrame, uniqueID, dependentVariable) {
  #initialize variables  
  training <- 0
  testing <- 0
  tuner <- 0
  currentUniqueID <- 0
  uniqueID_List <- 0
  y <- 0
  endList <- list()
  
  #create a list that is all the spatial group unqiue ids in the data frame (ie all the neighborhoods)    
  uniqueID_List <- unique(dataFrame[[uniqueID]])  
  x <- 1
  y <- length(uniqueID_List)
  
  #create a counter and while it is less than the number of counties...  
  while(x <= y) 
  {
    
    #call a current county    
    currentUniqueID <- uniqueID_List[x]
    
    #create a training set comprised of units not in that county and a test set of units
    #that are that county
    training <- dataFrame[ which(dataFrame[[uniqueID]] != uniqueID_List[x]),]
    testing <- dataFrame[ which(dataFrame[[uniqueID]] == uniqueID_List[x]),]
    
    trainingX <- training[ , -which(names(training) %in% c(dependentVariable))]
    testingX <- testing[ , -which(names(testing) %in% c(dependentVariable))]
    
    trainY <- training[[dependentVariable]]
    testY <- testing[[dependentVariable]]
    #tune a model - note I have hardwired the dependent variable and the trainControl    
    
    tuner <- glm(trainY~., data = trainingX, family=binomial)
    
    #come up with predictions on the test county
    thisPrediction <- predict(tuner, testingX, type="response")
    #calculate mape and mae and count of records for a given training set (to get a sense of sample size).
    thisAUC <- pROC::auc(as.numeric(testY),as.numeric(thisPrediction))
    countTestObs <- nrow(testingX)
    
    #add to the final list the current county and the MAPE    
    thisList <- cbind(currentUniqueID, thisAUC,countTestObs)
    #create a final list to output
    endList <- rbind(endList, thisList)
    #iterate counter    
    x <- x + 1 
    
  } 
  #return the final list of counties and associated MAPEs  
  return (as.data.frame(endList))
}

inputStory6 <- cbind(inputStory4.1,inputStory5.1)

inputStory7 <- inputStory6 %>% dplyr::filter(inputStory6$Neighborho != 4) %>% 
  dplyr::filter(Neighborho != 8) %>%
  dplyr::filter(Neighborho != 16) %>%
  dplyr::filter(Neighborho != 20) %>%
  dplyr::filter(Neighborho != 23) %>%
  dplyr::filter(Neighborho != 35) %>%
  dplyr::filter(Neighborho != 38) %>%
  dplyr::filter(Neighborho != 41) %>%
  dplyr::filter(Neighborho != 44) %>%
  dplyr::filter(Neighborho != 49) %>%
  dplyr::filter(Neighborho != 53) %>%
  dplyr::filter(Neighborho != 56) %>%
  dplyr::filter(Neighborho != 73) %>% 
  dplyr::filter(Community !="UNIVERSITY")


wow <- inputStory7 %>% 
  group_by(Neighborho, Lead_Hazar) %>% 
  dplyr::summarise(n=n())


inputStory8 <- inputStory7 %>% dplyr::select(c(Lead_Hazar, Neighborho, Neighborho1, BUILD_YR.x , Community , 
  BuildingUs , PermitCountTotal.x , OWNER_PCT1.x , Educ16.x , 
  Bedrooms , rent10 , TOT_PENALT.x , salecountTotal , d_vacant , 
  OWN90 , Vacancy80_90 , OWN70 , RENT80 , Child5to9 , yearsold , 
  VAC70))

inputStory4.2 <- cbind(PID2, inputStory4.1)
inputStory4.2$predClass  = ifelse(inputStory4.2$prediction2 > .2 ,1,0)
inputStory4.2$Correct = ifelse(inputStory4.2$predClass == inputStory4.2$Lead_Hazar, "Correct", "Incorrect")
inputStory4.2$confuse = ifelse((inputStory4.2$predClass == 0 & inputStory4.2$Lead_Hazar == 0), "True Negative", ifelse((inputStory4.2$predClass == 1 & inputStory4.2$Lead_Hazar == 1), "True Positive", ifelse((inputStory4.2$predClass == 0 & inputStory4.2$Lead_Hazar == 1), "False Negative", "False Positive")))

inputStory4.2.1 <- inputStory4.2 %>% distinct(PID, .keep_all=TRUE)

auc.omg <- auc(inputStory4.2.1$Lead_Hazar, inputStory4.2.1$prediction2)

caret::confusionMatrix(inputStory4.2.1$Lead_Hazar, as.factor(inputStory4.2.1$predClass))

pfn$PID <- as.character(pfn$PID)

pfn_new <- left_join(inputStory4.2, pfn, by="PID")
pfn_new1 <- st_as_sf(pfn_new)
pfn_new1$x <- st_coordinates(pfn_new1)[,1]
pfn_new1$y <- st_coordinates(pfn_new1)[,2]
pfn_new2 <- pfn_new1 %>% na.omit()
baseMap + geom_point(data=pfn_new2, aes(x=pfn_new2$x, y=pfn_new2$y, color=Correct)) + mapTheme() + labs(title="Correct and Incorrect Predictions")

spatialCV_results <- spatialCV(inputStory8, "Neighborho", "inputStory8$Lead_Hazar")

spatial <- as.data.frame(cbind(Neighborho = unlist(endList$currentUniqueID), thisAUC = unlist(endList$thisAUC)))


Parcels_fo_year$stepwise_predict <- predict(stepwise.train, Parcels_fo_year, type="response")

pfn_new_final <- left_join(Parcels_fo_year, pfn, by="PID")
pfn_new_final1 <- st_as_sf(pfn_new_final)
pfn_new_final1$x <- st_coordinates(pfn_new_final1)[,1]
pfn_new_final1$y <- st_coordinates(pfn_new_final1)[,2]
pfn_new_final2 <- pfn_new_final1 %>% na.omit()
pfn_new_final3 <- pfn_new_final2 %>% dplyr::select(stepwise_predict)
st_write(pfn_new_final3, "Shapefiles/final_points1.shp")

fromJSON("Shapefiles/pointsJson1.json")
Parcels_mapping <- Parcels_fo_year %>% 
  dplyr::group_by(Neighborho) %>% 
  dplyr::summarise(count=n())
Parcels_sum <- Parcels_fo_year %>% 
  dplyr::group_by(Neighborho) %>% 
  dplyr::summarise(sum = sum(stepwise_predict))

Parcels_map_sum <- cbind(Parcels_mapping, Parcels_sum)
Parcels_map_sum$rate <- Parcels_map_sum$sum/Parcels_map_sum$count 

testProbs0$predClass  = ifelse(testProbs0$Probs > .5 ,1,0)
testProbs0$Correct = ifelse(testProbs0$predClass == testProbs0$Lead, "Correct", "Incorrect")
testProbs0$confuse = ifelse((testProbs0$predClass == 0 & testProbs0$Lead == 0), "True Negative", ifelse((testProbs0$predClass == 1 & testProbs0$Lead == 1), "True Positive", ifelse((testProbs0$predClass == 0 & testProbs0$Lead == 1), "False Negative", "False Positive")))

xgb_trainData <- subset(Parcels_finalOmit, select=-Lead_Hazar.x.x.x)
LeadHazard_train <- as.factor(Parcels_finalOmit$Lead_Hazar.x.x.x)
levels(LeadHazard_train) <- c("No_Lead","Lead")
who2 <- sapply(who, as.numeric )


# train the model for each parameter combination in the grid, 
#   using CV to evaluate

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = c(.1, 0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = c(0,1),
  colsample_bytree = c(0, .2, .4, .6, .8, 1),
  min_child_weight = 1,
  subsample = c(0, .2, .4, .6, .8, 1)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "repeatedcv",
  number = 100,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(xgb_trainData),
  y = as.factor(LeadHazard_train),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

###Read in this table and then display the table "sens" using kable(sens)
resample_xgb <- read.csv("SpreadsheetData/resamplingTabletuning.csv")
sens <- resample_xgb %>% filter(Spec > 0.9945652)
sens <- sens[c(2:12)] %>% rename(Sens = Spec, Spec = Sens)

datTrain_Sub <- subset(datTrain, select=-Lead_Hazar.x.x.x)
LeadHazard_train <- datTrain$Lead_Hazar.x.x.x

fit_xgb <- xgboost(data.matrix(datTrain_Sub), LeadHazard_train,
                   nrounds = 800,
                   eta =  0.001,
                   max_depth = 2,
                   gamma = 1,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1
                   , booster = "gbtree"
                   , eval_metric = "error"
                   , objective="binary:logistic")

y_hat_xgb <- predict(fit_xgb, data.matrix(datTest))

testProbs3 <- data.frame(PID = datTest_all$PID, Lead = datTest_all$Lead_Hazar.x.x.x, Probs = y_hat_xgb, model="XGBoost") %>% 
  mutate(leadLabel = ifelse(Lead == 0, "Negative", "Positive"))
head(testProbs3)   
adminPropsPlot3 <- ggplot(testProbs3, aes(x = Probs, fill=as.factor(Lead))) + geom_density() +
  facet_grid(leadLabel ~ .) + xlab("Postive Presence Probability") + scale_fill_manual(values=paletteLead, labels = c("Negative", "Positive")) + labs(fill="Lead Presence", title="XGBoost Selected Variables") + plotTheme()
adminPropsPlot3

#
myresult3 <- data.frame()
for (i in c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 
            0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)){
  testProbs3$predClass<- ifelse(testProbs3$Probs > i ,1,0)
  sens <- specificity(as.factor(testProbs3$predClass),as.factor(testProbs3$Lead))
  thisresult <- data.frame(i, sens)
  myresult3 <- rbind(myresult3,thisresult)
}

#
testProbs3$predClass  = ifelse(testProbs3$Probs > .5 ,1,0)
testProbs3$Correct = ifelse(testProbs3$predClass == testProbs3$Lead, "Correct", "Incorrect")
testProbs3$confuse = ifelse((testProbs3$predClass == 0 & testProbs3$Lead == 0), "True Negative", ifelse((testProbs3$predClass == 1 & testProbs3$Lead == 1), "True Positive", ifelse((testProbs3$predClass == 0 & testProbs3$Lead == 1), "False Negative", "False Positive")))

#Matrix4 <- confusionMatrix(testProbs3$predClass,testProbs3$Lead)
#Matrix4
# accuracy=.7374. sens=1.00. spec=.7234. auc= .6342

aucCurve4 <- ggplot(testProbs3, aes(d = Lead, m = Probs)) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="Gradient Boost Selected Variables ROC Curve") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')

auc.score.4 <- pROC::auc(testProbs3$Lead, testProbs3$Probs)

################################Xiao################################
##### Ken's new way of generating confusion matrix
confusion_matrix_3x3_reg <- confusionMatrixFor_Neg1_0_1(testProbs3$Lead, testProbs3$predClass)
Matrix3 <- confusion_matrix_3x3_reg[2:3,2:3]
colnames(Matrix3) <- c("-1", "1")
rownames(Matrix3) <- c("-1", "1")
#This is wrong
#statsFromConfusionMatrix(MatrixX)

##### correct Sensitivity(default cutoff=0.5)
sensitivity <- sensitivity(actuals=testProbs3$Lead,predictedScores=testProbs3$predClass)
##### correct specificity(default cutoff=0.5)
specificity <- specificity(actuals=testProbs3$Lead,predictedScores=testProbs3$predClass)
##### best cutoff
max_sens_cutoff <- optimalCutoff(actuals=testProbs3$Lead,predictedScores=testProbs3$predClass, optimiseFor='Both')
print(max_sens_cutoff)
maxsens <- sensitivity(actuals=testProbs3$Lead,predictedScores=testProbs3$predClass,threshold=max_sens_cutoff)
# Mis-Classification Error: proportion of all events that were incorrectly classified
misClassError <- misClassError(actuals=testProbs3$Lead,predictedScores=testProbs3$predClass, threshold=0.5)

auc.score <- pROC::auc(testProbs3$Lead, testProbs3$Probs)

thismodel <- data.frame(sensitivity,specificity,max_sens_cutoff,maxsens,misClassError,auc.score)
allmodels <- rbind(allmodels,thismodel)
####################################################################################

importance_matrix <- xgb.importance(colnames(datTrain_Sub), model = fit_xgb)
importancePlot <- xgb.plot.importance(importance_matrix = importance_matrix[1:10])

#Feature Importance chart
importancePlot_gg <- ggplot(importancePlot, aes(x=reorder(Feature, -Importance), y=Importance, 
                                                fill=reorder(Feature, -Importance)))+
  geom_bar(stat="identity", width = .8, position = position_dodge(width = .01))+
  #scale_fill_gradientn(colors = c("#a8dbd9","#85c4c9","#68abb8","#4f90a6","#3b738f",
  # "#2a5674", "#2a5674","#2a5674","#2a5674","#2a5674"))+
  #geom_text(aes(label=paste0(Lead), vjust = -.25))+
  labs(x= "Feature", y= "Feature Importance", 
       fill = "Importance", title = "Feature Importance")+
  plotTheme()

finalTested$PID <- as.numeric(as.character(finalTested$PID))
str(finalTested)
testProbs3$PID <- as.numeric(as.character(testProbs3$PID))
mapxgboost <- left_join(testProbs3, finalTested)

probMap_xgb <- st_as_sf(mapxgboost)

correctMap <- ggplot() +                                                                                      #error Maureen
  geom_sf(data=minneapolis_roads %>% filter(SPEED_LIM > 40), aes(), fill=NA, colour='grey70') + 
  geom_sf(data=minneapolis_water, aes(), fill="skyblue", colour = NA) +
  geom_sf(data=minneapolis, aes(), fill=NA, colour="black", size = 1.5) +
  geom_point(data=probMap_xgb, aes(x=(st_coordinates(probMap_xgb)[,1]), y=(st_coordinates(probMap_xgb)[,2]), color=as.factor(probMap_xgb$Correct)), size = 1) +
  scale_color_manual(values=paletteCorrect, labels = c("Correct", "Incorrect"))+
  labs(title= "Correct Classification of \nXGBoost Model, Minneapolis", color="Classification") +
  mapTheme()

predictedMap <- ggplot() + 
  geom_sf(data=minneapolis_roads %>% filter(SPEED_LIM > 40), aes(), fill=NA, colour='grey70') + 
  geom_sf(data=minneapolis_water, aes(), fill="skyblue", colour = NA) +
  geom_sf(data=minneapolis, aes(), fill=NA, colour="black", size = 1.5) +
  geom_point(data=probMap_xgb, aes(x=(st_coordinates(probMap_xgb)[,1]), y=(st_coordinates(probMap_xgb)[,2]), color=as.factor(probMap_xgb$predClass)), size=1) +
  scale_color_manual(values=paletteClassified, labels = c("Predicted Negative", "Predicted Positive"))+
  labs(title= "Predicted Class for \nXGBoost Model, Minneapolis", color="Prediction") +
  mapTheme()

##for goodness of fit: the f1 score (recall) of the **minority** class (the mean between sensitive and specificity find the formula)
#.75 is what we're

##Tree model
fit_tree <- rpart(Lead_Hazar.x.x.x ~ .,
                  method="anova", data = datTrain)
summary(fit_tree)

tree <- plot(fit_tree, uniform=TRUE, 
             main="Home Price Prediction") 
tree <- text(fit_tree, use.n=TRUE, all=TRUE, cex=.55)

y_hat_tree <- predict(fit_tree,datTest)

print(paste("RF", mae(datTest_all$Lead_Hazar.x, y_hat_tree)))

testProbs4 <- data.frame(PID = datTest_all$PID, Lead = datTest_all$Lead_Hazar.x.x.x, Probs = y_hat_tree, model="Random Forest") %>% 
  mutate(leadLabel = ifelse(Lead == 0, "Negative", "Positive"))
head(testProbs4)   
adminPropsPlot4 <- ggplot(testProbs4, aes(x = Probs, fill=as.factor(Lead))) + geom_density() +
  facet_grid(leadLabel ~ .) + xlab("Postivie Presence Probability") + scale_fill_manual(values=paletteLead, labels = c("Negative", "Positive")) + labs(fill="Lead Presence", title="Random Forest Selected Variables") + plotTheme()
adminPropsPlot4

#
myresult4 <- data.frame()
for (i in c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 
            0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)){
  i
  testProbs4$predClass<- ifelse(testProbs4$Probs > i ,1,0)
  sens <- specificity(as.factor(testProbs4$predClass),as.factor(testProbs4$Lead))
  thisresult <- data.frame(i, sens)
  myresult4 <- rbind(myresult4,thisresult)
}

testProbs4$predClass  = ifelse(testProbs4$Probs > .5 ,1,0)
testProbs4$Correct = ifelse(testProbs4$predClass == testProbs4$Lead, "Correct", "Incorrect")
testProbs4$confuse = ifelse((testProbs4$predClass == 0 & testProbs4$Lead == 0), "True Negative", ifelse((testProbs4$predClass == 1 & testProbs4$Lead == 1), "True Positive", ifelse((testProbs4$predClass == 0 & testProbs4$Lead == 1), "False Negative", "False Positive")))

aucCurve5 <- ggplot(testProbs4, aes(d = Lead, m = Probs)) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  labs(title="Random Forest Selected Variables ROC Curve") +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')

auc.score.5 <- pROC::auc(testProbs4$Lead, testProbs4$Probs)
