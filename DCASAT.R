#D.CASTREJON, A.SOLTYS, A.TREJO
#SUMMER DATA CAMP NEIU 2024



##### ~~~~PLEASE DON'T REMOVE LEADING ZEROS FROM .CSV~~~~~~ ####
#Access to file 
#setwd("C:/Users/...")
#getwd()
data <- read.csv(file.choose())

#Creating copy
data1 <- data

#Summary of data
summary_data <- summary(data1)
print(summary_data)

# Packages
library(ggplot2)
library(dplyr)  
library(sf)


# Descriptive Stats
#for obesity
datamean_obesity <- mean(data1$Obesity_Prevalence)
datamedian_obesity <- median(data1$Obesity_Prevalence)
datamax_obesity <- max(data1$Obesity_Prevalence)
datamin_obesity <- min(data1$Obesity_Prevalence)
datasd_obesity <- sd(data1$Obesity_Prevalence)
datavar_obesity <- var(data1$Obesity_Prevalence)



#For Urban/Rural
datamean_metro <- mean(data1$Metro_Values)
datamedian_metro <- median(data1$Metro_Values)
datamax_metro <- max(data1$Metro_Values)
datamin_metro <- min(data1$Metro_Values)
datasd_metro <- sd(data1$Metro_Values)
datavar_metro <- var(data1$Metro_Values)


#For Hispanic
datamean_hispanic <- mean(data1$Hispanic)
datamedian_hispanic <- median(data1$Hispanic)
datamax_hispanic <- max(data1$Hispanic)
datamin_hispanic <- min(data1$Hispanic)
datasd_hispanic <- sd(data1$Hispanic)
datavar_hispanic <- var(data1$Hispanic)

#For Asian
datamean_asian <- mean(data1$Asian)
datamedian_asian  <- median(data1$Asian)
datamax_asian <- max(data1$Asian)
datamin_asian  <- min(data1$Asian)
datasd_asian  <- sd(data1$Asian)
datavar_asian  <- var(data1$Asian)

#For Black
datamean_black <- mean(data1$Black)
datamedian_black <- median(data1$Black)
datamax_black <- max(data1$Black)
datamin_black <- min(data1$Black)
datasd_black <- sd(data1$Black)
datavar_black <- var(data1$Black)

#For White
datamean_white <- mean(data1$White)
datamedian_white <- median(data1$White)
datamax_white <- max(data1$White)
datamin_white <- min(data1$White)
datasd_white <- sd(data1$White)
datavar_white <- var(data1$White)

#Food access
datamean_food <- mean(data1$food_access)
datamedian_food <- median(data1$food_access)
datamax_food <- max(data1$food_access)
datamin_food <- min(data1$food_access)
datasd_food <- sd(data1$food_access)
datavar_food <- var(data1$food_access)

#Inactivity
datamean_inactivity <- mean(data1$inactivity)
datamedian_inactivity <- median(data1$inactivity)
datamax_inactivity <- max(data1$inactivity)
datamin_inactivity <- min(data1$inactivity)
datasd_inactivity <- sd(data1$inactivity)
datavar_inactivity <- var(data1$inactivity)

#Poverty
datamean_poverty <- mean(data1$poverty)
datamedian_poverty <- median(data1$poverty)
datamax_poverty <- max(data1$poverty)
datamin_poverty <- min(data1$poverty)
datasd_poverty <- sd(data1$poverty)
datavar_poverty <- var(data1$poverty)


#Correlations
#For Obesity
corObesityMetro<- cor(data1$Obesity_Prevalence,data1$Metro_Values)
corObesityHispanic <- cor(data1$Obesity_Prevalence,data1$Hispanic)
corObesityAsian <- cor(data1$Obesity_Prevalence,data1$Asian)
corObesityBlack <- cor(data1$Obesity_Prevalence,data1$Black)
corObesityWhite <- cor(data1$Obesity_Prevalence,data1$White)
corObesityPoverty <- cor(data1$Obesity_Prevalence,data1$poverty)
corObesityFood <- cor(data1$Obesity_Prevalence,data1$food_access)
corObesityInactivity <- cor(data1$Obesity_Prevalence,data1$inactivity)



#Histograms
par(mfrow = c(2,3))
hist(data1$Obesity_Prevalence)
hist(data1$Black)
hist(data1$Hispanic)
hist(data1$Asian)
hist(data1$White)
hist(data1$Metro_Values)

par(mfrow = c(1,3))
hist(data1$poverty)
hist(data1$food_access)
hist(data1$inactivity)



#Regression analysis
#Impact of Race/Ethnicity on Obesity

#Impact of  Black Population on Obesity 
m1 <- lm(Obesity_Prevalence~Black, data1)
summary(m1)
#Impact of Black Population and Metro location on Obesity 
m2 <- lm(Obesity_Prevalence~Black+Metro_Values, data1)
summary(m2)
#Regression of  Black Population, Food Access, Poverty
# and inactivity on Obesity
mB1 <- lm(Obesity_Prevalence~Black+food_access+poverty+inactivity,data1)
summary(mB1)

#Regression Plot for Obesity on Black Population
ggplot(data1, aes(x = Black, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "lightgreen")+
  labs(x="Black",y = "Obesity")
#Regression plot of  Black Population, Food Access, Poverty
# and inactivity on Obesity
ggplot(data1, aes(x = Black+food_access+poverty+inactivity, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  labs(x="Black with Food Access, Poverty & Inactivity",y = "Obesity")

#Impact of Obesity on White Population
m3 <- lm(Obesity_Prevalence~White, data1)
summary(m3)
#Impact of Obesity and Metro location on White Population
m4 <- lm(Obesity_Prevalence~White+Metro_Values, data1)
summary(m4)
#Regression of  White Population, Food Access, Poverty
# and inactivity on Obesity
mW1 <- lm(Obesity_Prevalence~White+food_access+poverty+inactivity,data1)
summary(mW1)

#Regression Plot for Obesity on White Population
ggplot(data1, aes(x = White, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "lightblue") +
  labs(x="White",y = "Obesity")
#Regression plot of  White Population, Food Access, Poverty
# and inactivity on Obesity
ggplot(data1, aes(x = White+food_access+poverty+inactivity, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")+
  labs(x="White with Food Access, Poverty & Inactivity",y = "Obesity")


#Impact of Obesity on Asian Population
m5 <- lm(Obesity_Prevalence~Asian, data1)
summary(m5)
#Impact of Obesity and Metro location on Asian Population
m6 <- lm(Obesity_Prevalence~Asian+Metro_Values, data1)
summary(m6)
#Regression of  Asian Population, Food Access, Poverty
# and inactivity on Obesity
mA1 <- lm(Obesity_Prevalence~Asian+food_access+poverty+inactivity,data1)
summary(mA1)


#Regression Plot for Obesity on Asian Population
ggplot(data1, aes(x = Asian, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(x="Asian",y = "Obesity")
#Regression plot of  Asian Population, Food Access, Poverty
# and inactivity on Obesity
ggplot(data1, aes(x = Asian+food_access+poverty+inactivity, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "brown") +
  labs(x="Asian with Food Access, Poverty & Inactivity",y = "Obesity")


#Impact of Obesity on Hispanic Population
m7 <- lm(Obesity_Prevalence~Hispanic, data1)
summary(m7)
#Impact of Obesity and Metro location on Asian Population
m8 <- lm(Obesity_Prevalence~Hispanic+Metro_Values, data1)
summary(m8)
#Regression of  Asian Population, Food Access, Poverty
# and inactivity on Obesity
mH1 <- lm(Obesity_Prevalence~Hispanic+food_access+poverty+inactivity,data1)
summary(mH1)


#Regression Plot for Obesity on Hispanic Population
ggplot(data1, aes(x = Hispanic, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "violet") +
  labs(x="Hispanic",y = "Obesity")
#Regression plot of  Hispanic Population, Food Access, Poverty
# and inactivity on Obesity
ggplot(data1, aes(x = Hispanic+food_access+poverty+inactivity, y = Obesity_Prevalence)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "purple") +
  labs(x="Hispanic with Food Access, Poverty & Inactivity",y = "Obesity")

#ILLINOIS Obesity geospatial mapping
#Link to SHP file
#browseURL("https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")


#ALL STATES
#creating new map and data variables
shape2 <-"C:..../county.shp"
us <- st_read(shape2)
data2 <- data

# Ensures GEOID columns are of the same type 
us$GEOID <-  sprintf("%05d",as.numeric(us$GEOID))
data2$GEOID <- sprintf("%05d",as.numeric(data2$GEOID)) 


# Checking for matching GEOIDs 
common_geoids <- intersect(us$GEOID, data2$GEOID) 
print(paste("Number of common GEOIDs:", length(common_geoids))) 

# Merge the map data with the obesity data 
merge_data <- us %>% 
  left_join(data2, by = "GEOID")

#filtering IL,GA, CA
plot_states <- c("17", "13", "06") 
filtered_data <- merge_data %>% 
  filter(STATEFP %in% plot_states & !is.na(Obesity_Prevalence))


# Setting CRS
#Sets coordinates to geographic
if (is.na(st_crs(filtered_data))) { 
  st_crs(filtered_data) <- 4326 
} 
# Checking Coordinate Reference System (CRS) 
print(st_crs(filtered_data))


#Geospatial Plot for Obesity Prevalence
obesity_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = Obesity_Prevalence), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Obesity Prevalence in IL, GA, and CA") 
print(obesity_plot)

#Geospatial Plot for Hispanic
library(ggplot2)
Hispanic_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = Hispanic), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Hispanic Population in IL, GA, and CA") 
print(Hispanic_plot)

#Geospatial Plot for Black Population
Black_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = Black), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Black Population in IL, GA, and CA") 
print(Black_plot)

#Geospatial Plot for Asian Population
Asian_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = Asian), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Asian Population in IL, GA, and CA") 
print(Asian_plot)

#Geospatial Plot for White Population
White_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = White), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("White Population in IL, GA, and CA") 
print(White_plot)

#Geospatial Plot for Metro/Non-Metro
Metro_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = Metro_Values), color = NA) + 
  theme_minimal() + ggtitle("Metropolitan vs NonMetro in IL, GA, and CA") 
print(Metro_plot)

#Geospatial Plot for Poverty
Poverty_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = poverty), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Poverty in IL, GA, and CA") 
print(Poverty_plot)

#Geospatial Plot for Food Access
Food_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = food_access), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Food Access in IL, GA, and CA") 
print(Food_plot)

#Geospatial Plot for Food Access
Inactivity_plot <- ggplot(data = filtered_data) +
  geom_sf(aes(fill = inactivity), color = NA) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() + ggtitle("Inactivity in IL, GA, and CA") 
print(Inactivity_plot)









