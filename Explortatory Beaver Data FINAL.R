################################################################################
######### Exploratory Data Analysis For Beaver Pond and Wildfire Data ##########
############################## William Samuel ##################################
################################################################################

library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(lessR)
library(plotrix)
library(patchwork)



setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-fire R Scripts")

data <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Input Data Files/Points_Polygons_Basin_and_Fire_Data_7_31_22.csv")
#View(data)
#str(data)

basin_data <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Input Data Files/Sampled_Drainages_and_Fires_7_31_22.csv")
#View(basin_data)
#str(basin_data)

perc_burned__data <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Input Data Files/Percentages_of_basins_burned_7_31_22.csv")
#View(perc_burned__data)
#str(perc_burned__data)

newdata <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/newdata_7_31_22.csv")

Ydata <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Ydata_7_31_22.csv")

Basin_Sums <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums.csv")

Basin_Sums_Y <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_Y.csv")

Basins_Surveyed <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basins_Surveyed.csv")

Basin_Sums_with_Zero <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_with_Zero.csv")

Basin_Sums_with_Zero_Y <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_with_Zero.csv")

Burned_dams <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Burned_dams.csv")

Unburned_dams <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Unburned_dams.csv")

Burned_dams_Y <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Burned_dams_Y.csv")

Unburned_dams_Y <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Unburned_dams_Y.csv")



# ArcGIS Workflow ---------------------------------------------------------

#Add Surface Information
#Add Join
#Spatial Join
#Fill this in later...

#Josh taught me a better method:
#Just do a single spatial join with each attribute (fire data, elevation data), then join all of those with a table join later on. Much simpler. 


# Steps to take in order to prepare the .csv files to run in this  --------

#Calculate "Time_since_burn" (2017 - year of burn)
#Create "Burned" and "Burned_Numerical" columns (1 is burned, zero is not)
#Create numerical columns for dam type (e.g., Main, Side, Off, Oxbow, "1" or "NA")
#..... finish the rest later.... 



# Start with data organization --------------------------------------------


###### MAKE SURE YOU PROPERLY LIST EACH DAM AS BURNED OR UNBURNED BASED ON FIRE_NAME BECAUSE FIRE_ID MIGHT NOT BE RELIABLE 

###### MAKE SURE YOU CALCULATE TIME_SINCE_BURN AND ONLY INCLUDE FIRES BEFORE AND IN 2017###
#data sheets with a ...7_30_22 ending DO NOT have any fires removed
#data sheets with a ...7_31_22 ending DO have fires during and after 2017 removed

#_______________________________________________________________________________

#R Course Data from library(tidyverse)
#groundfish <- rename(groundfish,"length_mm" = "Length Millimeters", "weight_kg" = "Weight Kilograms") 
#groundfish <- dplyr::select(groundfish, Year, Species, Sex, Age, length_mm, weight_kg) 
#groundfish <- filter(groundfish, Species == "Sablefish")

#_______________________________________________________________________________





#remove all error dams

newdata <- data[!(data$Notes=="Error" | data$Notes=="error"),]

#Remove dams found in drainage with poor image quality ("contaminated", excluded from analysis). 
newdata <- data[!(data$Notes=="Contaminated" | data$Notes=="contaminated"),]
View(newdata)

#Calculate Time_Since_Burn
newdata <- mutate(newdata, Time_Since_Burn = 2017-Year_Burned)




write.csv(newdata, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/newdata_7_31_22.csv")

newdata <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/newdata_7_31_22.csv")
View(newdata)


#Deal with this later in a linear regression
    #I just used this to visualize outliers
    boxplot(Area_m2 ~ Burned, data = newdata, labels(newdata$Dam_Num)) 
    car::Boxplot(Area_m2 ~ Burned, data = newdata)

    #remove row 460, an outlier (Dam_Num 454)
    #newdata <- edit(data) 
    #OR this is easier once you find the problem dam
    newdata2 <- newdata[-c(466),]
    #View(newdata)
    car::Boxplot(Area_m2 ~ Burned, data = newdata2)
    

#Only use the beaver dams we are sure of:
Ydata <- newdata[!(newdata$Beaver_Created=="N" | newdata$Beaver_Created=="M"),]
View(Ydata)

#Create categorical variables 
newdata$Dam_Type<-as.factor(newdata$Dam_Type)
Ydata$Dam_Type<-as.factor(Ydata$Dam_Type)
newdata$Burned<-as.factor(newdata$Burned)
Ydata$Burned<-as.factor(Ydata$Burned)


write.csv(Ydata, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Ydata_7_31_22.csv")

Ydata <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Ydata_7_31_22.csv")



# Calculate the burned vs unburned dams -----------------------------------


burned_dams <- newdata[!(newdata$Burned_numerical=="0"),]
#View(burned_dams) #361 Burned dams
unburned_dams <- newdata[!(newdata$Burned_numerical=="1"),]
#View(unburned_dams) #512 Unburned dams

write.csv(burned_dams, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Burned_dams.csv")

write.csv(unburned_dams, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Unburned_dams.csv")


burned_dams_Y <- Ydata[!(Ydata$Burned_numerical=="0"),]
#View(burned_dams_Y) #173 Burned Y dams
unburned_dams_Y <- Ydata[!(Ydata$Burned_numerical=="1"),]
#View(unburned_dams_Y) #249 Unburned Y dams

write.csv(burned_dams_Y, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Burned_dams_Y.csv")

write.csv(unburned_dams_Y, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Unburned_dams_Y.csv")



# Sum number of beaver dams in each catchment -----------------------------

#install.packages("lessR")
#library(lessR)


#use pivot function to sum and average data by basin
Dams_per_basin <- pivot(data=newdata, compute=sum, variable=Count, by=c(Basin_Name))
Dams_per_basin
Burned_Dams_per_basin <- pivot(data=newdata, compute=mean, variable=Burned, by=c(Basin_Name))
Burned_Dams_per_basin
Dam_area_per_basin_mean <-  pivot(data=newdata, compute=mean, variable=Area_m2, by=c(Basin_Name))
Dam_area_per_basin_mean 
Main_dam_per_basin <- pivot(data=newdata, compute=sum, variable=Main, by=c(Basin_Name))
Main_dam_per_basin
Side_dam_per_basin <- pivot(data=newdata, compute=sum, variable=Side, by=c(Basin_Name))
Side_dam_per_basin
Off_dam_per_basin <- pivot(data=newdata, compute=sum, variable=Off, by=c(Basin_Name))
Off_dam_per_basin
Burned_numerical <- pivot(data=newdata, compute=mean, variable=Burned, by=c(Basin_Name))
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
Burned_numerical$Burned_n <- replace(Burned_numerical$Burned_n, Burned_numerical$Burned_n>=1,1) 
Burned_numerical$Burned_n <- replace(Burned_numerical$Burned_n,Burned_numerical$Burned_n   <1,0)
Burned_numerical$Burned_basin <- Burned_numerical$Burned_n
Burned_numerical
#newdata$Time_Since_Burn <- mutate(newdata, Time_Since_Burn = 2017-Year_Burned) #calculate Time_Since_Burn, already did this earlier
newdata$Time_Since_Burn <- as.numeric(newdata$Time_Since_Burn)
Time_since_burn <- pivot(data=newdata, compute=min, variable=Time_Since_Burn, by=c(Basin_Name)) 
Time_since_burn
Basin_size <- pivot(data=newdata, compute=mean, variable=Basin_AreaSqKm, by=c(Basin_Name))
Basin_size
newdata$Z <- na_if(newdata$Z, "<Null>")
newdata$Z<-as.numeric(newdata$Z)
Dam_elevation_mean <- pivot(data=newdata, compute=mean, variable=Z, by=c(Basin_Name))
Dam_elevation_mean

#Combine all these matricies
Basin_sums <- data.frame(c(Dams_per_basin, Burned_Dams_per_basin, Dam_area_per_basin_mean, Main_dam_per_basin, Side_dam_per_basin, Off_dam_per_basin, Burned_numerical, Time_since_burn, Basin_size, Dam_elevation_mean)) 

#get rid of the useless columns
Basin_sums<-Basin_sums[,-c(3:5,8:11,13,15:17,19:21,23:28,30:32,34:36,38:40)]

#Replace Infinite values ("Inf") with NA
Basin_sums <- do.call(data.frame,                
                   lapply(Basin_sums,
                          function(x) replace(x, is.infinite(x), NA)))

#NEED TO FIGURE OUT WHY ARCGIS WONT "CALCULATE GEOMETRY" ON 200 PONDS....
#But for now we will just delete those zeros and treat them like NA's
Basin_sums$Area_m2_mean <- na_if(Basin_sums$Area_m2_mean, 0)
View(Basin_sums)



# The easiest way to do this is to use the function write.csv()
write.csv(Basin_sums, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums.csv")

Basin_Sums <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums.csv")



#Repeat for the Ydata (beaver dams we are sure about)
Dams_per_basin_Y <- pivot(data=Ydata, compute=sum, variable=Count, by=c(Basin_Name))
Dams_per_basin_Y
Burned_Dams_per_basin_Y <- pivot(data=Ydata, compute=mean, variable=Burned, by=c(Basin_Name))
Burned_Dams_per_basin_Y
Dam_area_per_basin_mean_Y <-  pivot(data=Ydata, compute=mean, variable=Area_m2, by=c(Basin_Name))
Dam_area_per_basin_mean_Y 
Main_dam_per_basin_Y <- pivot(data=Ydata, compute=sum, variable=Main, by=c(Basin_Name))
Main_dam_per_basin_Y
Side_dam_per_basin_Y <- pivot(data=Ydata, compute=sum, variable=Side, by=c(Basin_Name))
Side_dam_per_basin_Y
Off_dam_per_basin_Y <- pivot(data=Ydata, compute=sum, variable=Off, by=c(Basin_Name))
Off_dam_per_basin_Y
Burned_numerical_Y <- pivot(data=Ydata, compute=mean, variable=Burned, by=c(Basin_Name))
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
Burned_numerical_Y$Burned_n <- replace(Burned_numerical_Y$Burned_n, Burned_numerical_Y$Burned_n>=1,1) 
Burned_numerical_Y$Burned_n <- replace(Burned_numerical_Y$Burned_n,Burned_numerical_Y$Burned_n <1,0)
Burned_numerical_Y$Burned_basin <- Burned_numerical_Y$Burned_n
Burned_numerical_Y
#Ydata$Time_Since_Burn <- mutate(Ydata, Time_Since_Burn = 2017-Year_Burned) #calculate Time_Since_Burn, already did this earlier
#Ydata$Time_Since_Burn <- as.numeric(Ydata$Time_Since_Burn), already did this
Time_since_burn_Y <- pivot(data=Ydata, compute=min, variable=Time_Since_Burn, by=c(Basin_Name)) 
Time_since_burn_Y
Basin_size_Y <- pivot(data=Ydata, compute=mean, variable=Basin_AreaSqKm, by=c(Basin_Name))
Basin_size_Y
Ydata$Z <- na_if(Ydata$Z, "<Null>")
Ydata$Z<-as.numeric(Ydata$Z)
Dam_elevation_mean_Y <- pivot(data=Ydata, compute=mean, variable=Z, by=c(Basin_Name))
Dam_elevation_mean_Y

#Combine all these matricies
Basin_sums_Y <- data.frame(c(Dams_per_basin_Y, Burned_Dams_per_basin_Y, Dam_area_per_basin_mean_Y, Main_dam_per_basin_Y, Side_dam_per_basin_Y, Off_dam_per_basin_Y, Burned_numerical_Y, Time_since_burn_Y, Basin_size_Y, Dam_elevation_mean_Y)) 

#get rid of the useless columns
Basin_sums_Y<-Basin_sums_Y[,-c(3:5,8:11,13,15:17,19:21,23:28,30:32,34:36,38:40)]

#Replace Infinite values ("Inf") with NA
Basin_sums_Y <- do.call(data.frame,                
                      lapply(Basin_sums_Y,
                             function(x) replace(x, is.infinite(x), NA)))

#NEED TO FIGURE OUT WHY ARCGIS WONT "CALCULATE GEOMETRY" ON 200 PONDS....
#But for now we will just delete those zeros and treat them like NA's
Basin_sums_Y$Area_m2_mean <- na_if(Basin_sums_Y$Area_m2_mean, 0)
View(Basin_sums_Y)




write.csv(Basin_sums_Y, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_Y.csv")
#UPDATE
Basin_Sums_Y <- read.csv("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_Y.csv")






# Include the basins that were surveyed but had no beaver ponds -----------
basin_data
str(basin_data)
View(basin_data)

Basins_Surveyed <- basin_data %>% filter(Surveyed== 1)
View(Basins_Surveyed)
str(Basins_Surveyed)

#Join/merge the Basin sums with the actual Survey Results for beaver-fire regression
Basins_Surveyed <- Basins_Surveyed %>%  dplyr::rename( Basin_Name = Name)
Basin_Sums <- Basin_Sums %>%  dplyr::rename(Basin_Name = c.Basin_Name.)

#Basins_Surveyed <- merge(x = Basins_Surveyed, y = Basin_Sums, by = "Basin_Name"#,
#           all.y = TRUE)
Basins_Surveyed <- merge(x = Basins_Surveyed, y = Basin_Sums, by = "Basin_Name", all.x = TRUE)
str(Basins_Surveyed)

write.csv(Basins_Surveyed, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basins_Surveyed.csv")


# Update the Basin_Sums data set with the basins that have no beaver ponds------
#Now we need to add the basins with no beaver ponds to the Basin_Sums and Basin_Sums_Y data

#First, I have to repeat the data summation used to create the Basin_Sums files
#Ill ignore the beaver data since there are none in this file
Burned_numerical <- pivot(data=basin_data, compute=mean, variable=Burned, by=c(Basin_Name))
Burned_numerical
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
Burned_numerical$Burned_n <- replace(Burned_numerical$Burned_n, Burned_numerical$Burned_n>=1,1) 
Burned_numerical$Burned_n <- replace(Burned_numerical$Burned_n,Burned_numerical$Burned_n   <1,0)
Burned_numerical$Burned_basin <- Burned_numerical$Burned_n
Burned_numerical
basin_data$Time_Since_Burn <- mutate(basin_data, Time_Since_Burn = 2017-Year_Burned) #calculate Time_Since_Burn, already did this earlier
Time_since_burn <- pivot(data=basin_data, compute=min, variable=Time_Since_Burn, by=c(Basin_Name)) 
Time_since_burn
Basin_size <- pivot(data=basin_data, compute=mean, variable=Basin_AreaSqKm, by=c(Basin_Name))
Basin_size
#Dont have these data yet
#newdata$Z <- na_if(newdata$Z, "<Null>")
#newdata$Z<-as.numeric(newdata$Z)
#Dam_elevation_mean <- pivot(data=newdata, compute=mean, variable=Z, by=c(Basin_Name))
#Dam_elevation_mean

#Combine all these matricies
Zero_Basin_Sums <- data.frame(c(Burned_numerical, Time_since_burn, Basin_size))#, Dam_elevation_mean)) 

#get rid of the useless columns
Zero_Basin_Sums<-Zero_Basin_Sums[,-c(2:4, 6:8, 10:12)]

#Replace Infinite values ("Inf") with NA
Zero_Basin_Sums <- do.call(data.frame,                
                      lapply(Zero_Basin_Sums,
                             function(x) replace(x, is.infinite(x), NA)))

#Join/merge the Basin sums with the actual Survey Results for beaver-fire regression
Zero_Basin_Sums <- Zero_Basin_Sums %>%  dplyr::rename(Basin_Name = c.Basin_Name.)

Basin_Sums_with_Zero <- merge(x = Zero_Basin_Sums, y = Basin_Sums, by = "Basin_Name", all.x = TRUE)
str(Basin_Sums_with_Zero)
View(Basin_Sums_with_Zero)
# This left merge creates duplicate columns. Time_Since_Burn_min does not match, since Basin_Sums was created with beaver ponds and Zero_Basin_Sums was created with the fires in the catchments.

#Replace NA's with 0's in the Count_n column
Basin_Sums_with_Zero <- Basin_Sums_with_Zero %>% 
  mutate(Count_n = ifelse(is.na(Count_n), 0, Count_n))

#I'll create a combined column that uses the latest burned beaver pond when present, and the latest fire when not present 
Basin_Sums_with_Zero <- Basin_Sums_with_Zero %>% 
  mutate(Time_Since_Burn_combo = 
           ifelse(Count_n == 0, Time_Since_Burn_min.x, Time_Since_Burn_min.y))

#Delete extra repeat columns
Basin_Sums_with_Zero <- Basin_Sums_with_Zero[,-c(4,5)]

Basin_Sums_with_Zero <- Basin_Sums_with_Zero %>%  dplyr::rename(Basin_AreaSqKm_mean = Basin_AreaSqKm_mean.y)

write.csv(Basin_Sums_with_Zero, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_with_Zero.csv")

#Create a Basin_Sums_with_Zero_Y dataset, repeat the whole process

#Join/merge the Basin sums with the actual Y data Survey Results for beaver-fire regression
Basin_Sums_Y <- Basin_Sums_Y %>%  dplyr::rename(Basin_Name = c.Basin_Name.)

Basin_Sums_with_Zero_Y <- merge(x = Zero_Basin_Sums, y = Basin_Sums_Y, by = "Basin_Name", all.x = TRUE)
str(Basin_Sums_with_Zero_Y)
View(Basin_Sums_with_Zero_Y)

#Replace NA's with 0's in the Count_n column
Basin_Sums_with_Zero_Y <- Basin_Sums_with_Zero %>% 
  mutate(Count_n = ifelse(is.na(Count_n), 0, Count_n),
         Time_Since_Burn_combo = ifelse(Count_n == 0, Time_Since_Burn_min.x, Time_Since_Burn_min.y))

#Delete extra repeat columns
#Basin_Sums_with_Zero_Y <- Basin_Sums_with_Zero_Y[,-c(2, 17)]
#Basin_Sums_with_Zero_Y <- Basin_Sums_with_Zero_Y[,-c(4,5)]

Basin_Sums_with_Zero_Y <- Basin_Sums_with_Zero_Y %>%  dplyr::rename(Basin_AreaSqKm_mean = Basin_AreaSqKm_mean.x)

write.csv(Basin_Sums_with_Zero_Y, file = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Output Data Files/Basin_Sums_with_Zero_Y.csv")

# Summary Statistics ------------------------------------------------------

summary(Basin_Sums_Y$Count_n)
#Mean pond area
summary(Ydata$Area_m2)

summary(Basin_Sums_Y$Count_n) #average of 5.45 dams per basin
summary(Basin_Sums_Y$Area_m2_mean) 
summary(Ydata$Area_m2) #ponds are 1488.8 m2 on average
summary(Basin_Sums_Y$Basin_AreaSqKm_mean) #average basin size is 105 km2

#Mean number of ponds per 100 sqkm
mean((Basin_Sums_Y$Count_n/Basin_Sums_Y$Basin_AreaSqKm_mean)*100)
#5.8 dams per 100km2
sd((Basin_Sums_Y$Count_n/Basin_Sums_Y$Basin_AreaSqKm_mean)*100) #SD = 8.06
std.error((Basin_Sums_Y$Count_n/Basin_Sums_Y$Basin_AreaSqKm_mean)*100) #SE = 0.91


#Dam Area
summary(Ydata$Area_m2) #ponds are 1488.8 m2 on average
sd(Ydata$Area_m2, na.rm=TRUE) #SD = 3315.983
std.error(Ydata$Area_m2, na.rm=TRUE) #SE = 163.37

#Dam Area per 100 sqkm
mean((Basin_Sums_Y$Area_m2_mean/Basin_Sums_Y$Basin_AreaSqKm_mean)*100, na.rm = TRUE)
#1489 m2 per 100km2
mean(Ydata$Area_m2, na.rm=TRUE)
sd(Ydata$Area_m2, na.rm=TRUE) #SD = 3315.983
std.error(Ydata$Area_m2, na.rm=TRUE) #SE = 163.37


#Calculating how many dams occur in burned basins
Basin_Sums_Y %>% group_by(Burned_basin) %>%
  summarize(Count_n = sum(Count_n)) #274 ponds occurred in burned basins, 146 in unburned basins
274+146 = 420
247/420 (58%)

#this wont work unless I rewrite it to be Basin_Sums not Burned_dams_Y etc. 
#sd(burned_dams_Y$Count_n) 
#sd(unburned_dams_Y$Count_n, na.rm = TRUE) 
#std.error(burned_dams_Y$Count_n) 
#std.error(unburned_dams_Y$Count_n, na.rm = TRUE) 

mean(burned_dams_Y$Area_m2, na.rm = TRUE) #mean = 785
mean(unburned_dams_Y$Area_m2, na.rm = TRUE) #SD = 1964
sd(burned_dams_Y$Area_m2, na.rm = TRUE) #SD = 1365.998
sd(unburned_dams_Y$Area_m2, na.rm = TRUE) #SD = 4077.552
std.error(burned_dams_Y$Area_m2) #SE = 106.002
std.error(unburned_dams_Y$Area_m2, na.rm = TRUE) #SE = 259.9752

#Total pond area
sum(newdata$Area_m2, na.rm = TRUE) #total beaver pond area = 1,741,443 m^2, or 1.74 km2
sum(Ydata$Area_m2, na.rm = TRUE) #total Y beaver pond area = 613,398 m^2, or 0.61 km2


# Various analyses with fire data-----------------------------------------------


Basin_Sums
Basin_Sums_Y

#Correlation Matrix
#pairs(Basin_Sums[,c(2:4,7:9)], pch = 19)

#outlier(data$Area_m2)

#Number of Dams per basin vs time since last burned
plot(Count_n ~ Time_Since_Burn_min, data= Basin_Sums_with_Zero)
abline(lm(Count_n ~ Time_Since_Burn_min, data= Basin_Sums_with_Zero))
plot(Count_n ~ Time_Since_Burn_min, data= Basin_Sums_Y)
abline(lm(Count_n ~ Time_Since_Burn_min, data= Basin_Sums_Y))


#Pond area vs time since last burned
plot(Area_m2_mean ~ Time_Since_Burn_min.x, data= Basin_Sums_with_Zero,
     ylim = c(0,12000))
plot(Area_m2_mean ~ Time_Since_Burn_min, data= Basin_Sums_Y,
     ylim = c(0,12000))
plot(Area_m2 ~ Time_Since_Burn, data= newdata2, ylim = c(0,15000))
abline(lm(Area_m2 ~ Time_Since_Burn, data= newdata2))
lm(Area_m2 ~ Time_Since_Burn, data= newdata2)
plot(Area_m2 ~ Time_Since_Burn, data= Ydata, ylim = c(0,10000))
abline(lm(Area_m2 ~ Time_Since_Burn, data= Ydata))
lm(Area_m2 ~ Time_Since_Burn, data= Ydata)

#Standardized by basin Area
plot((Count_n/Basin_AreaSqKm_mean)*100 ~ Time_Since_Burn_min, data = Basin_Sums_Y)
abline(lm((Count_n/Basin_AreaSqKm_mean)*100 ~ Time_Since_Burn_min, data = Basin_Sums_Y))
summary(lm((Count_n/Basin_AreaSqKm_mean)*100 ~ Time_Since_Burn_min, data = Basin_Sums_Y))

plot((Area_m2_mean/Basin_AreaSqKm_mean)*100 ~ Time_Since_Burn_min, data = Basin_Sums_Y)
abline(lm((Area_m2_mean/Basin_AreaSqKm_mean)*100 ~ Time_Since_Burn_min, data = Basin_Sums_Y))
summary(lm((Area_m2_mean/Basin_AreaSqKm_mean)*100 ~ Time_Since_Burn_min, data = Basin_Sums_Y))

#Number of Dams per basin in burned/unburned, including the basins with no beaver ponds (zero data)
#... nothing is significant
Burned_basin_numerical_with_zero <- 
  Basin_Sums_with_Zero %>% filter(Burned_basin.x == 1)
Unburned_basin_numerical_with_zero <- 
  Basin_Sums_with_Zero %>% filter(Burned_basin.x == 0)

Burned_basin_numerical_Y_with_zero <- 
  Basin_Sums_with_Zero_Y %>% filter(Burned_basin.x == 1)
Unburned_basin_numerical_Y_with_zero <- 
  Basin_Sums_with_Zero_Y %>% filter(Burned_basin.x == 0)


t.test((Burned_basin_numerical_with_zero$Count_n/Burned_basin_numerical_with_zero$Basin_AreaSqKm_mean)*100,
       (Unburned_basin_numerical_with_zero$Count_n/Unburned_basin_numerical_with_zero$Basin_AreaSqKm_mean)*100, 
       alt = "greater") #Not significant
t.test((Burned_basin_numerical_Y_with_zero$Count_n/Burned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100,
       (Unburned_basin_numerical_Y_with_zero$Count_n/Unburned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100, 
       alt = "greater", na.rm = TRUE) #Not Significant

boxplot((Burned_basin_numerical_Y_with_zero$Count_n/Burned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical_Y_with_zero$Count_n/Unburned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100)




#When you remove the zero data catchments though, Y data becomes significant

#These t-testes were not significant, not sure why... 
#t.test((Basin_Sums$Burned_n/Basin_Sums$Basin_AreaSqKm_mean)*100, Basin_Sums$Burned_na, alt = "less")  #Not significant
#t.test((Basin_Sums_Y$Burned_n/Basin_Sums_Y$Basin_AreaSqKm_mean)*100, Basin_Sums_Y$Burned_na, alt="less") #Not significant

Burned_basin_numerical <- Basin_Sums %>% filter(Burned_basin == 1)
Unburned_basin_numerical <- Basin_Sums %>% filter(Burned_basin == 0)

Burned_basin_numerical_Y <- Basin_Sums_Y %>% filter(Burned_basin == 1)
Unburned_basin_numerical_Y <- Basin_Sums_Y %>% filter(Burned_basin == 0)

t.test((Burned_basin_numerical$Count_n/Burned_basin_numerical$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical$Count_n/Unburned_basin_numerical$Basin_AreaSqKm_mean)*100, alt = "greater") #Significant: p = 0.0001382
boxplot((Burned_basin_numerical$Count_n/Burned_basin_numerical$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical$Count_n/Unburned_basin_numerical$Basin_AreaSqKm_mean)*100)

t.test((Burned_basin_numerical_Y$Count_n/Burned_basin_numerical_Y$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical_Y$Count_n/Unburned_basin_numerical_Y$Basin_AreaSqKm_mean)*100, alt = "greater") # significant: p = 0.02733 
boxplot((Burned_basin_numerical_Y$Count_n/Burned_basin_numerical_Y$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical_Y$Count_n/Unburned_basin_numerical_Y$Basin_AreaSqKm_mean)*100)




#Create a new column for burned vs unburned that is non-numerical
Basin_Sums_Y <- Basin_Sums_Y %>% 
  mutate("Burned_basin_2" = if_else(Burned_basin == 1, "Burned", "Unburned"))
Basin_Sums_with_Zero_Y <- Basin_Sums_with_Zero_Y %>% 
  mutate("Burned_basin.y_2" = if_else(Burned_basin.y == 1, "Burned", "Unburned"))

#find the n = ... for your plot
length(Basin_Sums_Y$Burned_basin[Basin_Sums_Y$Burned_basin == "0"]) #35
length(Basin_Sums_Y$Burned_basin[Basin_Sums_Y$Burned_basin == "1"]) #42

#Create the y lab with a superscript
#ylab1 <- expression(Number~of~Y~Ponds~per~Basin~ (per 100km^2)) <- this one doesn't work :/
ylab1 <- expression(Number~of~HC~Ponds~per~Basin~ per~"100km"^2)


#bquote("Number"~"of"~"Y"~"Ponds"~"per"~"Basin"~ per ~ 100km^2)

p1 <- ggplot(Basin_Sums_Y, aes(x=Burned_basin_2, y=((Count_n/Basin_AreaSqKm_mean)*100), fill=Burned_basin_2))+#))+
  #,label=Time_Since_Burn_min))+
  geom_boxplot(color = "black")+
  geom_jitter()+
  theme_classic()+
  scale_fill_manual(values=c("red", "darkgreen"))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70), limits = c(0, 60))+
  labs(title = "A", x = "", y = ylab1 )+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "serif", size = 24, color="black"),
    axis.title = element_text(family = "serif", face = "bold", size = 20, color="black"),
    axis.text = element_text(family = "serif", size = 18, color="black"))+ #color = "grey29",
  annotate(geom="text", x=2.1, y=60, label="p = 0.027*",
           color="black", size = 7, family = "serif")
  #annotate(geom="text", x=1, y=40, label="n = 42",
  #         color="black", size = 6, family = "serif")+
  #annotate(geom="text", x=2, y=40, label="n = 35",
  #         color="black", size = 6, family = "serif")
  #annotate(geom="text", x=2, y=50, label="italic('p') = 0.013",
  #      color="black", size = 8, family = "serif", parse = TRUE)
  #annotate(geom="text",label=paste0("italic('p')~'='",p),parse=T,x=4.5,y=25,size=8)
  #annotate("text", x = 4.5, y = 2.2, size = 5,
  #         label = "italic(p)~=~0.013",
  #        parse = TRUE)
p1


ggsave(plot= p1,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Basin Count Burned vs Unburned.jpeg",
       dpi = 2000, 
       height = 6,
       width = 4,
       units = "in")





 
#Older code
  dev.off()
  par(mfrow = c(1,2), mar = c(1, 5, 1, 1), xpd = TRUE)
  #par(mar = c(1, 5, 1, 1), xpd = TRUE)
  windowsFonts(A = windowsFont("Times New Roman"))
  
  boxplot(((Count_n/Basin_AreaSqKm_mean)*100)~Burned_basin, data=Basin_Sums_Y,
          ylab = "Number of Y Ponds per Basin", 
          cex.lab = 1.5, cex.axis = 1.2,
          xlab = NULL, xaxt='n',
          col=c('darkgreen', 'red'))+
    legend("topleft", inset = c(0.05, 0.03), cex = 1.2,
           legend = c("Unburned", "Burned"),
           fill = c('darkgreen', 'red'))
  
  text(x=1.1, y=40.5, "T-Test: p = 0.130", cex = 1.2)
  
  boxplot(Count_n~Burned_basin, data=Basin_Sums,
          ylab = "Number of Ponds per Sub-catchment", 
          cex.lab = 1.5, cex.axis = 1.2,
          xlab = NULL, xaxt='n',
          col=c('darkgreen', 'red'))+
    legend("topleft", inset = c(0.05, 0.03), cex = 1.2,
           legend = c("Unburned", "Burned"),
           fill = c('darkgreen', 'red'))
  text(x=1.15, y=26, "T-Test: p = 0.070", cex = 1.2)
  dev.off()
  
  
########  
  
  
#Pond area in burned/unburned
boxplot(Area_m2_mean~Burned_basin, data=Basin_Sums, ylim = c(0, 5000))
boxplot(Area_m2_mean~Burned_basin, data=Basin_Sums_Y)

Basin_Sums_Y$Burned_basin <- as.factor(Basin_Sums_Y$Burned_basin)




#t.test(burned_dams$Area_m2, unburned_dams$Area_m2)
#t.test(burned_dams_Y$Area_m2, unburned_dams_Y$Area_m2, alt="less")
t.test((Basin_Sums$Area_m2_mean/Basin_Sums$Basin_AreaSqKm_mean)*100, Basin_Sums$Burned_na, alt = "greater") #Super significant, p = 0.00079
t.test((Basin_Sums_Y$Area_m2_mean/Basin_Sums_Y$Basin_AreaSqKm_mean)*100, Basin_Sums_Y$Burned_na, alt="greater") #Significant!!! p < 0.0000001, but you have to standardize by area  



t.test((Burned_basin_numerical_with_zero$Area_m2_mean/Burned_basin_numerical_with_zero$Basin_AreaSqKm_mean)*100,
       (Unburned_basin_numerical_with_zero$Area_m2_mean/Unburned_basin_numerical_with_zero$Basin_AreaSqKm_mean)*100, 
       alt = "less") #p = 0.1018
t.test((Burned_basin_numerical_Y_with_zero$Area_m2_mean/Burned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100,
       (Unburned_basin_numerical_Y_with_zero$Area_m2_mean/Unburned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100, 
       alt = "less", na.rm = TRUE) #Not Significant

boxplot((Burned_basin_numerical_Y_with_zero$Area_m2_mean/Burned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical_Y_with_zero$Area_m2_mean/Unburned_basin_numerical_Y_with_zero$Basin_AreaSqKm_mean)*100)



t.test((Burned_basin_numerical$Area_m2_mean/Burned_basin_numerical$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical$Area_m2_mean/Unburned_basin_numerical$Basin_AreaSqKm_mean)*100, alt = "less") #Significant: p = 0.0615
boxplot((Burned_basin_numerical$Area_m2_mean/Burned_basin_numerical$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical$Area_m2_mean/Unburned_basin_numerical$Basin_AreaSqKm_mean)*100)

t.test((Burned_basin_numerical_Y$Area_m2_mean/Burned_basin_numerical_Y$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical_Y$Area_m2_mean/Unburned_basin_numerical_Y$Basin_AreaSqKm_mean)*100, alt = "less") # significant: p = 0.01544 
boxplot((Burned_basin_numerical_Y$Area_m2_mean/Burned_basin_numerical_Y$Basin_AreaSqKm_mean)*100, (Unburned_basin_numerical_Y$Area_m2_mean/Unburned_basin_numerical_Y$Basin_AreaSqKm_mean)*100)








y_lab2 <- expression(Mean~HC~Pond~Area~(m^2)~per~Basin~per~"100km"^2)


p2 <- ggplot(Basin_Sums_Y, aes(x=Burned_basin_2,y=((Area_m2_mean/Basin_AreaSqKm_mean)*100), fill=Burned_basin_2))+#))+
  #,label=Time_Since_Burn_min))+
  geom_boxplot(color = "black")+
  geom_jitter()+
  theme_classic()+
  scale_fill_manual(values=c("red", "darkgreen"))+
  scale_y_continuous(limits = c(0, 15000), breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000))+
  labs(title = "B", x = "", y = y_lab2 )+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "serif", size = 24),
    axis.title = element_text(family = "serif", face = "bold", size = 20),
    axis.text = element_text(family = "serif", size = 18, color="black"))+#color = "grey29"
  annotate(geom="text", x=2.1, y=15000, label="p = 0.015*",
           color="black", size = 7, family = "serif")
#annotate(geom="text", x=2, y=50, label="italic('p') = 0.013",
#      color="black", size = 8, family = "serif", parse = TRUE)
#annotate(geom="text",label=paste0("italic('p')~'='",p),parse=T,x=4.5,y=25,size=8)
#annotate("text", x = 4.5, y = 2.2, size = 5,
#         label = "italic(p)~=~0.013",
#        parse = TRUE)
p2




ggsave(plot= p2,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Pond Area Burned vs Unburned.jpeg",
       dpi = 2000, 
       height = 6,
       width = 4,
       units = "in")



Count_and_Area_Burned_vs_Unburned <- p1+p2
Count_and_Area_Burned_vs_Unburned

ggsave(plot= Count_and_Area_Burned_vs_Unburned,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Basin Count and area Burned vs Unburned.jpeg",
       dpi = 2000, 
       height = 6,
       width = 8,
       units = "in")





#older code
dev.off()
#par(mar = c(1, 5, 1, 1), xpd = TRUE)
#Pond area of burned VS unburned ponds, not summed by basin
boxplot(Area_m2~Burned_numerical, data=Ydata,
        ylab = y_lab1, cex.lab = 1.5, cex.axis = 1.2,
        xlab = NULL, xaxt='n',
        col=c('darkgreen', 'red'))+
  legend("topright", inset = c(0.05, 0.03), cex = 1.2,
         legend = c("Unburned", "Burned"),
         fill = c('darkgreen', 'red'))
text(x=1.9, y=20000, "T-Test: p < 0.0001", cex = 1.2)

dev.off()


burned_Y_area_standard <- Basin_Sums_Y %>% 
  filter(Burned_basin == 1) %>% 
  mutate("Area_per_100sqkm" = (Area_m2_mean/Basin_AreaSqKm_mean)*100)
burned_Y_area_standard <- burned_Y_area_standard$Area_per_100sqkm

unburned_Y_area_standard <- Basin_Sums_Y %>% 
  filter(Burned_basin == 0) %>% 
  mutate("Area_per_100sqkm" = (Area_m2_mean/Basin_AreaSqKm_mean)*100)
unburned_Y_area_standard <- unburned_Y_area_standard$Area_per_100sqkm

t.test(burned_Y_area_standard, unburned_Y_area_standard, alt = "less")
  
#Pond area in burned VS unburned basins, standardized by area
t.test(Basin_Sums_Y$Area_m2_mean)
y_lab2 <- expression(Mean ~ Y ~ Pond ~ Area ~ (m^2) ~ per ~ "100" ~ km^2)
windowsFonts(A = windowsFont("Times New Roman"))
boxplot((Area_m2_mean/(Basin_AreaSqKm_mean/100))~Burned_basin, data=Basin_Sums_Y,
        ylab = y_lab2, cex.lab = 1.5, cex.axis = 1.2,
        xlab = NULL, xaxt='n',
        col=c('darkgreen', 'red'))+
  legend("topright", inset = c(0.05, 0.03), cex = 1.2,
         legend = c("Unburned", "Burned"),
         fill = c('darkgreen', 'red'), 
         family = "A")
text(x=1.9, y=10600, "T-Test: p < 0.015*", 
     cex = 1.2, family = "A")


sd(burned_dams_Y$Area_m2, na.rm = TRUE) #SD = 1365.998
sd(unburned_dams_Y$Area_m2, na.rm = TRUE) #SD = 4077.552
#install.packages("plotrix")
#library(plotrix)
std.error(burned_dams_Y$Area_m2) #SE = 106.002
std.error(unburned_dams_Y$Area_m2, na.rm = TRUE) #SE = 259.9752



#Frequency of Main, Side, Off Channel Dams
par(mfrow = c(2,3))
hist(Basin_Sums$Main_n, ylim = c(0,50))
hist(Basin_Sums$Side_n, ylim = c(0,50))
hist(Basin_Sums$Off_n, ylim = c(0,50))
hist(Basin_Sums_Y$Main_n, ylim = c(0,50))
hist(Basin_Sums_Y$Side_n, ylim = c(0,50))
hist(Basin_Sums_Y$Off_n, ylim = c(0,50))

#Try to group by fire
library(ggplot2)

ggplot(Basin_Sums,aes(x=Main_n))+geom_histogram()+facet_grid(~Burned_n)+theme_bw()
ggplot(Basin_Sums_Y,aes(x=Main_n))+geom_histogram()+facet_grid(~Burned_n)+theme_bw()


ggplot(Basin_Sums_Y,aes(x=Main_n))+geom_histogram()+facet_grid(~Time_Since_Burn_mean)+theme_bw()


ggplot(Basin_Sums_Y,aes(x=Count_n))+geom_histogram()+facet_grid(~Time_Since_Burn_mean)+theme_bw()
ggplot(Basin_Sums_Y,aes(x=Count_n))+geom_histogram()+facet_grid(~Time_Since_Burn_mean)+theme_bw()





par(mfrow = c(1,2))


dev.off()


head(Basin_Sums)

plot(Count_n~ Z_mean, data=Basin_Sums_Y)
plot(Area_m2_mean~ Z_mean, data=Basin_Sums_Y)
plot(Count_n~Time_Since_Burn_mean, data = Basin_Sums)
abline(lm(Count_n~Time_Since_Burn_min, data = Basin_Sums))
plot(Area_m2_mean~Time_Since_Burn_min, data=Basin_Sums)

summary(lm(Count_n~Burned_n+Time_Since_Burn_min+Z_mean+Basin_AreaSqKm_mean, data=Basin_Sums))
summary(lm(Area_m2_mean~Basin_AreaSqKm_mean+Z_mean+Burned_n+Time_Since_Burn_min, data=Basin_Sums))



ylab1 <- expression(Number~of~HC~Ponds~per~Basin~ per~"100km"^2)

Basin_Sums_Y$Burned_basin <- as.factor(Basin_Sums_Y$Burned_basin)

p3 <- ggplot(Basin_Sums_Y, aes(x=Z_mean,y=((Count_n/Basin_AreaSqKm_mean)*100), color=Burned_basin))+
  #,label=Time_Since_Burn_min))+
  #geom_smooth()+
  geom_point()+
  geom_smooth(method='lm', se = FALSE)+
  theme_classic()+
  scale_color_manual(values=c("darkgreen", "red"))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70), limits = c(0, 60))+
  labs(x = "Elevation (m)", y = ylab1 )+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "serif", size = 24),
    axis.title = element_text(family = "serif", size = 20),
    axis.text = element_text(family = "serif", size = 18, color="black"))


p3


ggsave(plot= p3,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Count Vs elevation.jpeg",
       dpi = 2000, 
       height = 6,
       width = 6,
       units = "in")




p4 <- ggplot(Basin_Sums_Y, aes(x=Z_mean,y=((Area_m2_mean/Basin_AreaSqKm_mean)*100), color=Burned_basin))+
  #,label=Time_Since_Burn_min))+
  #geom_smooth()+
  geom_point()+
  geom_smooth(method='lm', se = FALSE)+
  theme_classic()+
  scale_color_manual(values=c("darkgreen", "red"))+
  scale_y_continuous(limits = c(0, 7500), breaks = c(0, 2500, 5000, 7500))+
  labs(x = "Number of Years Since Burn", y = y_lab2 )+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "serif", size = 24),
    axis.title = element_text(family = "serif", size = 20),
    axis.text = element_text(family = "serif", size = 18, color="black"))

p4




ggsave(plot= p4,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Pond Area Vs elevation.jpeg",
       dpi = 2000, 
       height = 6,
       width = 6,
       units = "in")






p5 <- ggplot(Basin_Sums_Y, aes(x=Time_Since_Burn_min,y=((Count_n/Basin_AreaSqKm_mean)*100)))+
  #,label=Time_Since_Burn_min))+
  #geom_smooth()+
  geom_point()+
  geom_smooth(method='lm', se = FALSE)+
  theme_classic()+
  scale_color_manual(values=c("darkgreen", "red"))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70), limits = c(0, 60))+
  labs(x = "Number of Years Since Burn", y = ylab1 )+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "serif", size = 24),
    axis.title = element_text(family = "serif", size = 20),
    axis.text = element_text(family = "serif", size = 18, color="black"))

p5




ggsave(plot= p5,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Count Vs time_since_burn.jpeg",
       dpi = 2000, 
       height = 6,
       width = 6,
       units = "in")





p6 <- ggplot(Basin_Sums_Y, aes(x=Time_Since_Burn_min,y=((Area_m2_mean/Basin_AreaSqKm_mean)*100)))+
  #,label=Time_Since_Burn_min))+
  #geom_smooth()+
  geom_point()+
  geom_smooth(method='lm', se = FALSE)+
  theme_classic()+
  scale_color_manual(values=c("darkgreen", "red"))+
  scale_y_continuous(limits = c(0, 7500), breaks = c(0, 2500, 5000, 7500))+
  labs(x = "Number of Years Since Burn", y = y_lab2 )+
  theme(
    legend.position = "none",
    plot.title = element_text(family = "serif", size = 24),
    axis.title = element_text(family = "serif", size = 20),
    axis.text = element_text(family = "serif", size = 18, color="black"))

p6




ggsave(plot= p6,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Figures/Pond Area Vs time_since_burn.jpeg",
       dpi = 2000, 
       height = 6,
       width = 6,
       units = "in")


# Pond Area ---------------------------------------------------------------
#Pond Area Histogram
par(mfrow = c(1,2))
hist(newdata$Area_m2, xlab = "Beaver Pond Area (m^2)")
hist(Ydata$Area_m2, xlab = "Beaver Pond Area (m^2)")



#Burned vs Unburned Pond Area
boxplot(Area_m2 ~ Burned, data = newdata, 
        ylab = "Beaver Pond Area (m^2)", xlab = NULL)
boxplot(Area_m2 ~ Burned, data = Ydata, 
        ylab = "Beaver Pond Area (m^2)", xlab = NULL)

t.test(newdata$Burned, newdata$Area_m2) #Dams in burned areas are statistically 
                                        #significantly larger. 

#Area vs Catchment
par(mar = c(8, 5, 3, 2))
boxplot(Area_m2 ~ Basin_Name, data = newdata, las=2,
        ylab = "Beaver Pond Area (m^2)", xlab = NULL)
boxplot(Area_m2 ~ Basin_Name, data = Ydata, las=2,
        ylab = "Beaver Pond Area (m^2)", xlab = NULL)
  dev.off()
  
#t.test(newdata$Burned, newdata$Area_m2) is this correct? or should it be an ANOVA?


#Area vs Dam Type
boxplot(Area_m2 ~ Dam_Type, data = newdata, las=2, xlab = NULL)+
    title(xlab = "Dam Type", mgp = c(4, 1, 0))
boxplot(Area_m2 ~ Dam_Type, data = Ydata, las=2, xlab = NULL)+
  title(xlab = "Dam Type", mgp = c(4, 1, 0))
  

t.test(newdata$Burned, newdata$Area_m2) #



#Area vs elevation
plot(Area_m2 ~ Z, data = newdata, las = 2,
        ylab = "Beaver Pond Area (m^2)", xlab = "Elevation (m^2)")
plot(Area_m2 ~ Z, data = Ydata, las = 2,
        ylab = "Beaver Pond Area (m^2)", xlab = "Elevation (m^2)")


#Area vs Dam Type
boxplot(Area_m2 ~ Dam_Type, data = newdata, las = 2, xlab=NULL)+
  title(xlab = "Dam Type", mgp = c(4, 1, 0))
boxplot(Area_m2 ~ Dam_Type, data = Ydata, las = 2, xlab=NULL)+
  title(xlab = "Dam Type", mgp = c(4, 1, 0))

dev.off()



# Damn Categories ---------------------------------------------------------

par(mfrow = c(1,2))


#Dam Type vs Elevation
plot(Dam_Type ~ Z, data = newdata, las = 2,
     ylab = "Beaver Pond Type", xlab = "Elevation (m^2)")
plot(Dam_Type ~ Z, data = Ydata, las = 2,
     ylab = "Beaver Pond Type", xlab = "Elevation (m^2)")


#Dam type vs burn history
plot(Dam_Type ~ Burned, data = newdata, las=2, ylab = "Beaver Pond Type")
plot(Dam_Type ~ Burned, data = Ydata, las=2, ylab = "Beaver Pond Area Type")


t.test(newdata$Burned, newdata$Area_m2) #


boxplot(Area_m2 ~ HUCName, data = newdata, las=2)

t.test(newdata$Burned, newdata$Area_m2)

dev.off()



# Time Since Last Burn ----------------------------------------------------

par(mfrow = c(1,2))

#Time since last burn vs Dam Area
plot(Area_m2~Time_Since_Burn, data=newdata)
plot(Area_m2~Time_Since_Burn, data=Ydata)

#Time since last burn vs Dam Type
plot(Dam_Type~Time_Since_Burn, data=newdata)
plot(Dam_Type~Time_Since_Burn, data=Ydata)











