################################################################################
#################### Beaver Pond and Wildfire Regression #######################
############################ William Samuel ####################################
################################################################################


library(tidyverse) #data organization 
library(dplyr) #data organization
#library(car)
library(lessR) #For table Pivots
library(corrplot) #for correlation plots
library(rcompanion) #for pseudo R-squared
library(plotrix) #for Standard Error
library(ggplot2) #Plotting
library(cowplot) #Neat, minimalist plots
library(patchwork) #Panel plots
library(MuMIn)    #Helpful for dredging and selecting models and model parameters
library(fmsb)     #For VIF function
library(AICcmodavg)	#For model averaging which can help you find the most important predictors
#AICmodavg <- there's a paper about model averaging


#This has a ton of great information about psuedo r-squared, bootstrapping, confidince intervals, etc. 
#https://rcompanion.org/handbook/G_10.html#:~:text=There%20is%20no%20R%2Dsquared,Cox%20and%20Snell%2C%20and%20Nagelkerke.



setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2")

all_VB <- read.csv("Output Data Files/all_VB.csv")

all_VB_Y <- read.csv("Output Data Files/all_VB_Y.csv")

all_RCA <- read.csv("Output Data Files/all_RCA.csv")

all_RCA_Y <- read.csv("Output Data Files/all_RCA_Y.csv")

VB_sums_full <- read.csv("Output Data Files/VB_sums_full.csv")

VB_Y_sums_full <- read.csv("Output Data Files/VB_Y_sums_full.csv")

RCA_sums_full <- read.csv("Output Data Files/RCA_sums_full.csv")

RCA_Y_sums_full <- read.csv("Output Data Files/RCA_Y_sums_full.csv")

#VB_sums <- read.csv("Output Data Files/VB_sums.csv")

#VB_Y_sums <- read.csv("Output Data Files/VB_Y_sums.csv")

#RCA_sums <- read.csv("Output Data Files/RCA_sums.csv")

#RCA_Y_sums <- read.csv("Output Data Files/RCA_Y_sums.csv")

netmap_data <- read.csv("Output Data Files/netmap_data.csv") #This wont read in from GitHub because of duplicate column names

netmap_sums <- read.csv("Output Data Files/netmap_sums.csv")

prediction_blank_RCA <- read.csv("Output Data Files/Prediction_blank_RCA.csv") #This wont read in from GitHub because of duplicate column names

prediction_blank_VB <- read.csv("Output Data Files/Prediction_blank_VB.csv") #This wont read in from GitHub because of duplicate column names


  
  
# ArcGIS Data Workflow and Information -----------------------------------------

#Spatial Join Beaver Points to Beaver Polygons
#Snap the joined points to the NetMap streamlines ("EDGES" (confluence to confluence) 

#Spatial Join points to "EDGES" (confluence to confluence), "NetMap 2km" (Individual reaches), RCAs, and RCA valley bottoms (VB).
#Reverse the Spatial Join (e.g., points to RCA, points to VB)

#Table to Table in ArcGIS, as .csv

#Associate the rest of the data using table joins in R (e.g., RCA to fire data, by RCA ID)

#Info from Josh about ArcGIS files:
#EDGES : confluence to confluence streamlines. These are the NetMap reaches dissolved together into one line, and then split at each confluence. These streamlines are used to create the RCAs and can also be joined to tabular data just like RCAs polygons and RCA_VB polygons. This feature class is useful if you ever want to symbolize the streamlines instead of the RCA/RCA_VB polygons.

#NetMap_2km : NetMap reaches with greater than 2km upstream area. The NetMap reaches from all 6 basins have been merged into this one feature class, and there is a unique alphanumeric reach "uID" than can be used to subset the reaches by basin. The "uID" can also join RCAs polygons and RCA_VB polygons to the original NetMap reach attributes (see the "NETMAP..." tables below)

#RCA : Reach contributing area polygons created from confluence to confluence streamlines. Each RCA has a unique ID. These polygons were the spatial unit used for zonal statistics tables with an "_RCA" suffix.

#RCA_VB :  Reach contributing area polygons clipped to valley bottom extent**. Each RCA_VB has a unique ID. These polygons were the spatial unit used for zonal statistics tables with an "_RCA_VB" suffix. (**The valley bottom extent is our "in-house" version, which is more continuous than the NetMap product. Like the NetMap product, valley bottom extent approximates a floodplain at 2x channel depth.)

#ALD_MEAN... : Active layer depth from the Terrestrial Ecosystem Model. This is a 1km resolution dataset we received from Helene Genet.

#FOLIAR_... : Percent foliar cover for multiple species. This is a 10m resolution dataset from Nawrocki (2020) and uses satellite data from 2017-2020.

#MTBS_FireData... : Combined fire data for the fire with maximum burned area within a given spatial unit. Includes MTBS burn severity, fire ignition date,etc. MTBS is a LandSat based, 30m resolution dataset.

#PFT_... : Percent plant functional type for year 2015. This is a LandSat based, 30m resolution dataset from Macander and Nelson (2022).

#SOLAR_... : Solar radiation model run on an IFSAR DEM resampled to 30m. The model is for direct + diffuse radiation, at hourly calculations at 2 week intervals, from Equinox to Equinox (Julian day 80 to 264). Output is in Watt Hours per Square Meter (WH/m2).

#NETMAP_REACH_DATA... : A copy of the NetMap_2km feature class attribute table. This table contains the "uID" column used to join RCA polygons to individual NetMap reaches. 

#NETMAP_REACH_RCA... : A table listing all NetMap reaches that have their midpoints within a given RCA. The "uID" column corresponds to unique NetMap reaches and can be used to join tabular NetMap reach data to RCAs. Since this is a "one-to-many" style join, you will need to define a function to aggregate the NetMap data. For most operations, we simply use the mean of NetMap reach data grouped by RCA. 

#_______________________________________________________________________________

# Summarize NetMap Data  -------------------------------------------------------



netmap_reach_data <- read.csv("Input Data Files/NETMAP_REACH_DATA.csv")
netmap_reach_rca <- read.csv("Input Data Files/NETMAP_REACH_RCA.csv")
netmap_data <- merge(x = netmap_reach_rca, y = netmap_reach_data, by = "uID", all.x = TRUE)
netmap_data <- netmap_data %>%  dplyr::rename(uRCA = uRCA_int)
str(netmap_data)
#write.csv(netmap_data, file = "Output Data Files/netmap_data.csv")

netmap_sums <- read.csv("Input Data Files/netmap_sums.csv")

RCA <- read.csv("Input Data Files/RCA.csv")
RCA_VB <- read.csv("Input Data Files/RCA_VB.csv")


vb_basins_surveyed <- read.csv("Input Data Files/VB_Basins_Surveyed.csv")
rca_basins_surveyed <- read.csv("Input Data Files/RCA_Basins_Surveyed.csv")
vb_basins_surveyed <- vb_basins_surveyed %>% filter(Surveyed == 1)
rca_basins_surveyed <- rca_basins_surveyed %>% filter(Surveyed == 1)
vb_basins_surveyed <- vb_basins_surveyed[,c(5:7, 27, 34)] 
rca_basins_surveyed <- rca_basins_surveyed[,c(5:7, 21, 28)] 
length(unique(vb_basins_surveyed$uRCA))
length(unique(rca_basins_surveyed$uRCA))



#Summarize Netmap Data by RCA ID (uRCA)
str(netmap_data)
#uRCA <- pivot(data=netmap_data, compute=mean, variable=________, by=c(uRCA))
#uRCA
length_m <- pivot(data=netmap_data, compute=sum, variable=LENGTH_M, by=c(uRCA))
length_m <- length_m[,-c(2,3)]
head(length_m)
area_sqkm <- pivot(data=netmap_data, compute=mean, variable=AREA_SQKM, by=c(uRCA)) #Should this be averaged or summed? I think its the upstream drainage area, so it can be averaged for each RCA: http://www.netmaptools.org/Pages/NetMapHelp/drainage_area.htm?mw=Mzgz&st=MQ==&sct=MTcwMA==&ms=AAAAAAA=
area_sqkm <- area_sqkm[,-c(2,3)]
head(area_sqkm)
elev_m <- pivot(data=netmap_data, compute=mean, variable=ELEV_M, by=c(uRCA)) 
elev_m <- elev_m[,-c(2,3)]
head(elev_m)
out_dist <- pivot(data=netmap_data, compute=min, variable=OUT_DIST, by=c(uRCA)) 
out_dist <- out_dist[,-c(2,3)]
head(out_dist)
#src_dist <- pivot(data=netmap_data, compute=min, variable=SRC_DIST, by=c(uRCA)) 
#src_dist <- src_dist[,-c(2,3)] ###DONT KNOW WHAT THIS VARIABLE IS
#head(src_dist)
#from_dist <- pivot(data=netmap_data, compute=min, variable=FROM_DIST, by=c(uRCA)) 
#from_dist <- from_dist[,-c(2,3)] ###DONT KNOW WHAT THIS VARIABLE IS
#head(from_dist)
gradient <- pivot(data=netmap_data, compute=mean, variable=GRADIENT, by=c(uRCA)) 
gradient <- gradient[,-c(2,3)]
head(gradient)
meananprc_m <- pivot(data=netmap_data, compute=mean, variable=MNANPRC_M, by=c(uRCA)) 
meananprc_m <- meananprc_m[,-c(2,3)] ###Mean Annual Precipitation
head(meananprc_m)
meanancms <- pivot(data=netmap_data, compute=mean, variable=MEANANNCMS, by=c(uRCA)) 
meanancms <- meanancms[,-c(2,3)] ###Mean Annual Discharge (CMS)
head(meanancms)
width_m <- pivot(data=netmap_data, compute=mean, variable=WIDTH_M, by=c(uRCA)) 
width_m <- width_m[,-c(2,3)] 
head(width_m)
depth_m <- pivot(data=netmap_data, compute=mean, variable=DEPTH_M, by=c(uRCA)) 
depth_m <- depth_m[,-c(2,3)] 
head(depth_m)
max_grad_d <- pivot(data=netmap_data, compute=max, variable=MAX_GRAD_D, by=c(uRCA)) 
max_grad_d <- max_grad_d[,-c(2,3)] ###Maximum Downstream Gradient
head(max_grad_d)
lake <- pivot(data=netmap_data, compute=max, variable=LAKE, by=c(uRCA)) 
lake <- lake[,-c(2,3)] 
head(lake)
stream_order <- pivot(data=netmap_data, compute=max, variable=STRM_ORDER, by=c(uRCA)) 
stream_order <- stream_order[,-c(2,3)] 
head(stream_order)
azimth_deg <- pivot(data=netmap_data, compute=mean, variable=AZIMTH_DEG, by=c(uRCA)) 
azimth_deg <- azimth_deg[,-c(2,3)] 
head(azimth_deg)
sinuosity <- pivot(data=netmap_data, compute=mean, variable=SINUOSITY, by=c(uRCA)) 
sinuosity <- sinuosity[,-c(2,3)] 
head(sinuosity)
val_width <- pivot(data=netmap_data, compute=mean, variable=VAL_WIDTH, by=c(uRCA)) 
val_width <- val_width[,-c(2,3)] 
head(val_width)
fp_width <- pivot(data=netmap_data, compute=mean, variable=FP_WIDTH, by=c(uRCA)) 
fp_width <- fp_width[,-c(2,3)] 
head(fp_width)
dropmax <- pivot(data=netmap_data, compute=max, variable=DROPMAX, by=c(uRCA)) 
dropmax <- dropmax[,-c(2,3)] ###Maximum WaterFall Drop
head(dropmax)
fp_width <- pivot(data=netmap_data, compute=mean, variable=FP_WIDTH, by=c(uRCA)) 
fp_width <- fp_width[,-c(2,3)] 
head(fp_width)
fish <- pivot(data=netmap_data, compute=min, variable=Fish, by=c(uRCA)) 
fish <- fish[,-c(2,3)] ###Fish Bearing
head(fish)
basin_id <- pivot(data=netmap_data, compute=min, variable=Basin_ID, by=c(uRCA)) 
basin_id <- basin_id[,-c(2,3)] 
head(basin_id)
vwi_floor <- pivot(data=netmap_data, compute=mean, variable=VWI_Floor, by=c(uRCA)) 
vwi_floor <- vwi_floor[,-c(2,3)] ###Valley Width Index-IP
head(vwi_floor)
valcnstrnt <- pivot(data=netmap_data, compute=mean, variable=ValCnstrnt, by=c(uRCA)) 
valcnstrnt <- valcnstrnt[,-c(2,3)] ###Channel Confinement at 2x depth
head(valcnstrnt)
flow_vel <- pivot(data=netmap_data, compute=mean, variable=FlowVel, by=c(uRCA)) 
flow_vel <- flow_vel[,-c(2,3)] ###Flow velocity (m/s)
head(flow_vel)
bankful_flow <- pivot(data=netmap_data, compute=mean, variable=BFQ, by=c(uRCA)) 
bankful_flow <- bankful_flow[,-c(2,3)] ###Bankful flow (cms)
head(bankful_flow)
bankful_flow <- pivot(data=netmap_data, compute=mean, variable=BFQ, by=c(uRCA)) 
bankful_flow <- bankful_flow[,-c(2,3)] ###Bankful flow (cms)
head(bankful_flow)
stream_power <- pivot(data=netmap_data, compute=mean, variable=StrmPow, by=c(uRCA)) 
stream_power <- stream_power[,-c(2,3)] ###Stream Power (Watts/m)
head(stream_power)
beaver_habitat <- pivot(data=netmap_data, compute=min, variable=BeavHab, by=c(uRCA)) 
beaver_habitat <- beaver_habitat[,-c(2,3)] ###Beaver Habitat, Favorable (1) or not (0), Pollock et al. 2004
head(beaver_habitat)
fish_resident <- pivot(data=netmap_data, compute=min, variable=FISH_RESID, by=c(uRCA)) 
fish_resident <- fish_resident[,-c(2,3)] ###Resident Fish Present? 
head(fish_resident)
gep_cum <- pivot(data=netmap_data, compute=mean, variable=GEP_Cum, by=c(uRCA)) 
gep_cum <- gep_cum[,-c(2,3)] ###Generic Erosion Potential - summed downstream 
head(gep_cum)
gep <- pivot(data=netmap_data, compute=mean, variable=GEP, by=c(uRCA)) 
gep <- gep[,-c(2,3)] ###Generic Erosion Potential
head(gep)
ip_chinook <- pivot(data=netmap_data, compute=mean, variable=IP_CHIN2, by=c(uRCA)) 
ip_chinook <- ip_chinook[,-c(2,3)] ###Habitat Intrinsic Potential-Chinook, Busch et al. 2011, http://www.onrc.washington.edu/MarinePrograms/IPOlympicRegion/IPmodeling/Discussion/IPModels/rra1597.pdf
head(ip_chinook)

#I CANT GET THIS TO WORK.....
#netmap_data <- netmap_data %>% 
#  add_column(CCon_num = NA) %>% 
#  mutate(CCon_num = ifelse(CCon == "UNCONSTRAINED", 1, CCon_num)) %>% 
#  mutate(CCon_num = ifelse(CCon == "INDETERMINATE", 2, CCon_num)) %>% 
#  mutate(CCon_num = ifelse(CCon == "CONSTRAINED", 3, CCon_num)) %>% 
#  mutate(CCon_num = ifelse(is.na(CCon), NA, CCon_num))
#View(netmap_data)
#ccon_num <- pivot(data=netmap_data, compute=max, variable=CCon_num, by=c(uRCA)) 
#ccon_num <- ccon_num[,-c(2,3)] ###Channel Confinement Class, I created a numerical class. Unconstrained (1), Indeterminate (2), Constrained (3)
#head(ccon_num)

netmap_sums <- data.frame(c(length_m, area_sqkm, out_dist, gradient, meananprc_m, meanancms, width_m, depth_m, max_grad_d, lake, stream_order, azimth_deg, sinuosity, val_width, fp_width, dropmax, fish, basin_id, vwi_floor, valcnstrnt, flow_vel, bankful_flow, stream_power, beaver_habitat, fish_resident, gep_cum, gep, ip_chinook, elev_m))
head(netmap_sums)

#View(Basin_sums)

#get rid of the useless columns
netmap_sums <- netmap_sums[,-c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57)]
netmap_sums <- netmap_sums %>%  dplyr::rename(uRCA = c.uRCA.)
str(netmap_sums)

#Add RCA and VB data to the dataset
netmap_sums <- merge(x = netmap_sums, y = RCA[,c(3,6,7)], by = "uRCA", all.x = TRUE)
head(netmap_sums)
netmap_sums <- merge(x = netmap_sums, y = RCA_VB[,c(8,11,12)], by = "uRCA", all.x = TRUE)
head(netmap_sums)


write.csv(netmap_sums, file = "Output Data Files/netmap_sums.csv")



#_______________________________________________________________________________

# Summarize by RCA and VB -------------------------------------------------
#Do this for both Y data and the full dataset


beaver_points_to_vb <- read.csv("Input Data Files/Beaver_Points_to_VB.csv")
#View(Beaver_Points_to_VB)

beaver_points_to_rca <- read.csv("Input Data Files/Beaver_Points_to_RCA.csv")

beaver_points_to_netmap <- read.csv("Input Data Files/Beaver_Points_to_NetMap_2km.csv")

beaver_points_to_edges <- read.csv("Input Data Files/Beaver_Points_to_EDGES.csv")

beaver_polygon_area <- read.csv("Input Data Files/Beaver_Polygon_Area.csv")

vb_to_beaver_points <- read.csv("Input Data Files/VB_to_Beaver_Points.csv")

rca_to_Beaver_Points <- read.csv("Input Data Files/RCA_to_Beaver_Points.csv")

netmap_to_beaver_points <- read.csv("Input Data Files/NetMap_2km_to_Beaver_Points.csv")

edges_to_beaver_points <- read.csv("Input Data Files/EDGES_to_Beaver_Points.csv")

foliar_rca <- read.csv("Input Data Files/FOLIAR_RCA.csv")
foliar_vb <- read.csv("Input Data Files/FOLIAR_RCA_VB.csv")
#Rename columns to create a common field
foliar_vb <- foliar_vb %>%  dplyr::rename(uRCA = uRCA_int)
foliar_rca <- foliar_rca %>%  dplyr::rename(uRCA = uRCA_int)

mtbs_fire_rca <- read.csv("Input Data Files/MTBSFireData_by_RCA_Max_Fire_Area_Only.csv")
mtbs_fire_vb <- read.csv("Input Data Files/MTBSFireData_by_RCA_VB_Max_Fire_Area_Only.csv")

pft_2015_rca <- read.csv("Input Data Files/PFT_RCA_2015.csv")
pft_2015_vb <- read.csv("Input Data Files/PFT_RCA_VB_2015.csv")

solar_rca <- read.csv("Input Data Files/SOLAR_RCA.csv")
solar_vb <- read.csv("Input Data Files/SOLAR_RCA_VB.csv")

ald_mean_rca <- read.csv("Input Data Files/ALD_MEAN_RCA.csv")
ald_mean_vb <- read.csv("Input Data Files/ALD_MEAN_RCA_VB.csv")


RCA <- read.csv("Input Data Files/RCA.csv")
RCA_VB <- read.csv("Input Data Files/RCA_VB.csv")

netmap_sums <- read.csv("Output Data Files/netmap_sums.csv")


vb_basins_surveyed <- read.csv("Input Data Files/VB_Basins_Surveyed.csv")
rca_basins_surveyed <- read.csv("Input Data Files/RCA_Basins_Surveyed.csv")
vb_basins_surveyed <- vb_basins_surveyed %>% filter(Surveyed == 1)
rca_basins_surveyed <- rca_basins_surveyed %>% filter(Surveyed == 1)
vb_basins_surveyed <- vb_basins_surveyed[,c(5:7, 27, 34)] 
rca_basins_surveyed <- rca_basins_surveyed[,c(5:7, 21, 28)] 
length(unique(vb_basins_surveyed$uRCA))
length(unique(rca_basins_surveyed$uRCA))







beaver_points_to_vb[beaver_points_to_vb$Dam_Num %in% c('133'),]


#Need to remove Dam num and object ID
#133, 92, 226, 227
#146, 115
#147, 116
#148B, 888
#149B, 889

#Remove duplicate dams that didn't get caught during the first QA/QC
beaver_points_to_vb <- beaver_points_to_vb[-c(92, 226, 227, 115,116, 888,889),]
beaver_points_to_rca <- beaver_points_to_rca[-c(92, 226, 227, 115,116, 888,889),]
beaver_points_to_netmap <- netmap_to_beaver_points[-c(92, 226, 227, 115,116, 888,889),]
beaver_points_to_edges <- edges_to_beaver_points[-c(92, 226, 227, 115,116, 888,889),]


beaver_polygon_area[beaver_polygon_area$Dam_Num %in% c('133'),]
#Dam num = row num
#133 = 796
#146 (already removed)
#147 = 689
#148B = 574
#149B = 575
#Insert Beaver Pond Area (Shape_Area) because it was calculated wrong before
beaver_polygon_area <- beaver_polygon_area[-c(796, 689, 574, 575),]


 
beaver_polygon_area <- beaver_polygon_area %>%  dplyr::rename( Pond_Area_m2 = Shape_Area)
beaver_polygon_area



#Add the pond area, which I had to recalcualte after the fact because it calculated wrong
beaver_points_to_vb <- merge(x = beaver_points_to_vb, y = beaver_polygon_area, by = "Dam_Num", all.x = TRUE)
head(beaver_points_to_vb)
beaver_points_to_rca <- merge(x = beaver_points_to_rca, y = beaver_polygon_area, by = "Dam_Num", all.x = TRUE)
beaver_points_to_netmap <- merge(x = beaver_points_to_netmap, y = beaver_polygon_area, by = "Dam_Num", all.x = TRUE)
beaver_points_to_edges <- merge(x = beaver_points_to_edges, y = beaver_polygon_area, by = "Dam_Num", all.x = TRUE)





###Summarizing by Valley Bottom

#remove all error dams
#beaver_points_to_vb_new <- beaver_points_to_vb[!(beaver_points_to_vb$Notes=="Error" | data$Notes=="error"),] WONT WORK FOR SOME REASON
nrow(beaver_points_to_vb)
beaver_points_to_vb_new <- beaver_points_to_vb %>% 
  filter(Notes!="Error") %>% 
  filter(Notes!="error")
nrow(beaver_points_to_vb_new)


#Remove dams found in drainages with poor image quality ("contaminated", excluded from analysis). 
#beaver_points_to_vb_new <- beaver_points_to_vb_new[!(beaver_points_to_vb_new$Notes=="Contaminated" | data$Notes=="contaminated"),]
nrow(beaver_points_to_vb_new)
beaver_points_to_vb_new <- beaver_points_to_vb_new %>% 
  filter(Notes!="Contaminated") %>% 
  filter(Notes!="contaminated")
nrow(beaver_points_to_vb_new)

##Data Prep before pivoting
#Calculate Time_Since_Burn
beaver_points_to_vb_new <- mutate(beaver_points_to_vb_new, Time_Since_Burn = 2017-Year)

beaver_points_to_vb_new <- beaver_points_to_vb_new %>% 
  mutate(Time_Since_Burn = ifelse(Time_Since_Burn == 2017, NA, Time_Since_Burn))

str(beaver_points_to_vb_new)

beaver_points_to_vb_new <- beaver_points_to_vb_new %>% 
  mutate(Burned = ifelse(is.na(Time_Since_Burn), 0, 1))

beaver_points_to_vb_new <- beaver_points_to_vb_new %>% 
  mutate(Main = ifelse(Dam_Type == "main", 1, 0))
beaver_points_to_vb_new <- beaver_points_to_vb_new %>% 
  mutate(Side = ifelse(Dam_Type == "side", 1, 0))
beaver_points_to_vb_new <- beaver_points_to_vb_new %>% 
  mutate(Off = ifelse(Dam_Type == "off", 1, 0))


length(unique(beaver_points_to_vb_new$uRCA))

#use pivot function to sum and average data by basin
dams_per_basin <- pivot(data=beaver_points_to_vb_new, compute=sum, variable=Join_Count, by=c(uRCA))
dams_per_basin
sum(dams_per_basin$Join_Count_sum>="0", na.rm=T)
burned_dams_per_basin <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Burned, by=c(uRCA))
burned_dams_per_basin
dam_area_per_basin_mean <-  pivot(data=beaver_points_to_vb_new, compute=mean, variable=Pond_Area_m2, by=c(uRCA))
dam_area_per_basin_mean ###NEED TO UPDAT THE DAM AREA BECAUSE ARCGIS WONT COMPUTE THE AREA FOR ALL THE DAMS
main_dam_per_basin <- pivot(data=beaver_points_to_vb_new, compute=sum, variable=Main, by=c(uRCA))
main_dam_per_basin
side_dam_per_basin <- pivot(data=beaver_points_to_vb_new, compute=sum, variable=Side, by=c(uRCA))
side_dam_per_basin
off_dam_per_basin <- pivot(data=beaver_points_to_vb_new, compute=sum, variable=Off, by=c(uRCA))
off_dam_per_basin
burned_numerical <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Burned, by=c(uRCA))
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
burned_numerical <- burned_numerical %>% 
  mutate(Burned_mean = ifelse(Burned_mean>0, 1, 0))
#beaver_points_to_vb_new$Time_Since_Burn <- as.numeric(beaver_points_to_vb_new$Time_Since_Burn)
time_since_burn <- pivot(data=beaver_points_to_vb_new, compute=min, variable=Time_Since_Burn, by=c(uRCA)) 
time_since_burn
#basin_size <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Basin_AreaSqKm, by=c(uRCA))
#basin_size CAN UPDATE THIS USING THE NETMAP DATA
beaver_points_to_vb_new$Z <- na_if(beaver_points_to_vb_new$Z, "<Null>")
beaver_points_to_vb_new$Z<-as.numeric(beaver_points_to_vb_new$Z)
dam_elevation_mean <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Z, by=c(uRCA))
dam_elevation_mean #THIS IS PROBABLY UNECCESSARY, I CAN USE THE NETMAP DATA

#Combine all these matricies
VB_sums <- data.frame(c(dams_per_basin, burned_dams_per_basin, dam_area_per_basin_mean, main_dam_per_basin, side_dam_per_basin, off_dam_per_basin, burned_numerical, time_since_burn, dam_elevation_mean)) 
#View(Basin_sums)

#get rid of the useless columns
VB_sums<-VB_sums[,-c(3:5,7:11,13,15:17,19:21,23:27,29:31,33:35)] #23:28 gets rid of the burned numerical

#Replace Infinite values ("Inf") with NA
VB_sums <- do.call(data.frame,                
                      lapply(VB_sums,
                             function(x) replace(x, is.infinite(x), NA)))
VB_sums <- VB_sums %>%  dplyr::rename(Dams_Per_Basin = Join_Count_n)
VB_sums <- VB_sums %>%  dplyr::rename(Burned_numerical = Burned_mean.1)
VB_sums <- VB_sums %>%  dplyr::rename(uRCA = c.uRCA.)


head(VB_sums)
length(unique(VB_sums$uRCA))



write.csv(VB_sums, file = "Output Data Files/VB_sums.csv")


##Join data zonal statistics data to beaver point data (Valley Bottom)
#Foliar cover (Nawrocki et al.)
VB_sums_full <- merge(x = VB_sums, y = foliar_vb, by = "uRCA", all.x = TRUE)
str(VB_sums_full)
#MTBS Fire Data
VB_sums_full <- merge(x = VB_sums_full, y = mtbs_fire_vb, by = "uRCA", all.x = TRUE)
str(VB_sums_full)
#Solar Data
VB_sums_full <- merge(x = VB_sums_full, y = solar_vb, by = "uRCA", all.x = TRUE)
head(VB_sums_full)
#Plant Functional Type (MaCander and Nelson 2022)
VB_sums_full <- merge(x = VB_sums_full, y = pft_2015_vb, by = "uRCA", all.x = TRUE)
head(VB_sums_full)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
VB_sums_full <- merge(x = VB_sums_full, y = ald_mean_vb, by = "uRCA", all.x = TRUE)
head(VB_sums_full)
###ADD NetMap Streamline Data!!!!!!!!!
VB_sums_full <- merge(x = VB_sums_full, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(VB_sums_full) ####
str(VB_sums_full, list.len=ncol(VB_sums_full))
sum(VB_sums_full$Dams_Per_Basin>="1", na.rm=T)
length(unique(VB_sums_full$uRCA))

VB_sums_full <- VB_sums_full %>% select(-contains("OID"))

VB_sums_full <- merge(x = VB_sums_full, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(VB_sums_full)

write.csv(VB_sums_full, file = "Output Data Files/VB_sums_full.csv") #this is for analysis with JUST the basins with beaver ponds. Below, includes the catchments surveyed that didn't have beaver ponds


#Join the basins surveyed with the surveyed data (to include 0's)
all_VB <- merge(x = vb_basins_surveyed, y = VB_sums, by = "uRCA", all.x = TRUE)
##Join data zonal statistics data to beaver point data (Valley Bottom)
#Foliar cover (Nawrocki et al.)
all_VB <- merge(x = all_VB, y = foliar_vb, by = "uRCA", all.x = TRUE)
str(all_VB)
#MTBS Fire Data
all_VB <- merge(x = all_VB, y = mtbs_fire_vb, by = "uRCA", all.x = TRUE)
str(all_VB)
#Solar Data
all_VB <- merge(x = all_VB, y = solar_vb, by = "uRCA", all.x = TRUE)
head(all_VB)
#Plant Functional Type (MaCander and Nelson 2022)
all_VB <- merge(x = all_VB, y = pft_2015_vb, by = "uRCA", all.x = TRUE)
head(all_VB)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
all_VB <- merge(x = all_VB, y = ald_mean_vb, by = "uRCA", all.x = TRUE)
head(all_VB)
#ADD NetMap Streamline Data!!!!!!!!!
all_VB <- merge(x = all_VB, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(all_VB) ####
str(all_VB, list.len=ncol(all_VB))
sum(all_VB$Dams_Per_Basin>="1", na.rm=T)
length(unique(all_VB$uRCA))

all_VB <- all_VB %>% select(-contains("OID"))

all_VB <- merge(x = all_VB, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(all_VB)


write.csv(all_VB, file = "Output Data Files/all_VB.csv")



###NEED TO DO FOR Y DAMS TOO
bp_vb_Y_new <- beaver_points_to_vb_new %>% filter(Beaver_cre == "Y")
#use pivot function to sum and average data by basin
dams_per_basin <- pivot(data=bp_vb_Y_new, compute=sum, variable=Join_Count, by=c(uRCA))
dams_per_basin
burned_dams_per_basin <- pivot(data=bp_vb_Y_new, compute=mean, variable=Burned, by=c(uRCA))
burned_dams_per_basin
dam_area_per_basin_mean <-  pivot(data=bp_vb_Y_new, compute=mean, variable=Pond_Area_m2, by=c(uRCA))
dam_area_per_basin_mean ###NEED TO UPDAT THE DAM AREA BECAUSE ARCGIS WONT COMPUTE THE AREA FOR ALL THE DAMS
main_dam_per_basin <- pivot(data=bp_vb_Y_new, compute=sum, variable=Main, by=c(uRCA))
main_dam_per_basin
side_dam_per_basin <- pivot(data=bp_vb_Y_new, compute=sum, variable=Side, by=c(uRCA))
side_dam_per_basin
off_dam_per_basin <- pivot(data=bp_vb_Y_new, compute=sum, variable=Off, by=c(uRCA))
off_dam_per_basin
burned_numerical <- pivot(data=bp_vb_Y_new, compute=mean, variable=Burned, by=c(uRCA))
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
burned_numerical <- burned_numerical %>% 
  mutate(Burned_mean = ifelse(Burned_mean>0, 1, 0))
burned_numerical
#beaver_points_to_vb_new$Time_Since_Burn <- as.numeric(beaver_points_to_vb_new$Time_Since_Burn)
time_since_burn <- pivot(data=bp_vb_Y_new, compute=min, variable=Time_Since_Burn, by=c(uRCA)) 
time_since_burn
#basin_size <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Basin_AreaSqKm, by=c(uRCA))
#basin_size CAN UPDATE THIS USING THE NETMAP DATA
bp_vb_Y_new$Z <- na_if(bp_vb_Y_new$Z, "<Null>")
bp_vb_Y_new$Z<-as.numeric(bp_vb_Y_new$Z)
dam_elevation_mean <- pivot(data=bp_vb_Y_new, compute=mean, variable=Z, by=c(uRCA))
dam_elevation_mean #THIS IS PROBABLY UNECCESSARY, I CAN USE THE NETMAP DATA

#Combine all these matricies
VB_Y_sums <- data.frame(c(dams_per_basin, burned_dams_per_basin, dam_area_per_basin_mean, main_dam_per_basin, side_dam_per_basin, off_dam_per_basin, burned_numerical, time_since_burn, dam_elevation_mean)) 
#View(Basin_sums)

#get rid of the useless columns
VB_Y_sums<-VB_Y_sums[,-c(3:5,7:11,13,15:17,19:21,23:27,29:31,33:35)] #23:28 gets rid of the burned numerical

#Replace Infinite values ("Inf") with NA
VB_Y_sums <- do.call(data.frame,                
                   lapply(VB_Y_sums,
                          function(x) replace(x, is.infinite(x), NA)))
VB_Y_sums <- VB_Y_sums %>%  dplyr::rename(Dams_Per_Basin = Join_Count_n)
VB_Y_sums <- VB_Y_sums %>%  dplyr::rename(Burned_numerical = Burned_mean.1)
VB_Y_sums <- VB_Y_sums %>%  dplyr::rename(uRCA = c.uRCA.)

head(VB_Y_sums)
length(unique(VB_Y_sums$uRCA))

write.csv(VB_Y_sums, file = "Output Data Files/VB_Y_sums.csv") 



##Join data zonal statistics data to beaver point data (Y Valley Bottom)
#Foliar cover (Nawrocki et al.)
VB_Y_sums_full <- merge(x = VB_Y_sums, y = foliar_vb, by = "uRCA", all.x = TRUE)
str(VB_Y_sums_full)
#MTBS Fire Data
VB_Y_sums_full <- merge(x = VB_Y_sums_full, y = mtbs_fire_vb, by = "uRCA", all.x = TRUE)
str(VB_Y_sums_full)
#Solar Data
VB_Y_sums_full <- merge(x = VB_Y_sums_full, y = solar_vb, by = "uRCA", all.x = TRUE)
head(VB_Y_sums_full)
#Plant Functional Type (MaCander and Nelson 2022)
VB_Y_sums_full <- merge(x = VB_Y_sums_full, y = pft_2015_vb, by = "uRCA", all.x = TRUE)
head(VB_Y_sums_full)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
VB_Y_sums_full <- merge(x = VB_Y_sums_full, y = ald_mean_vb, by = "uRCA", all.x = TRUE)
head(VB_Y_sums_full)
###ADD NetMap Streamline Data!!!!!!!!!
VB_Y_sums_full <- merge(x = VB_Y_sums_full, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(VB_Y_sums_full) ####
str(VB_Y_sums_full, list.len=ncol(VB_Y_sums_full))
sum(VB_Y_sums_full$Dams_Per_Basin>="1", na.rm=T)
length(unique(VB_Y_sums_full$uRCA))

VB_Y_sums_full <- VB_Y_sums_full %>% select(-contains("OID"))

VB_Y_sums_full <- merge(x = VB_Y_sums_full, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(VB_Y_sums_full)

write.csv(VB_Y_sums_full, file = "Output Data Files/VB_Y_sums_full.csv") #this is for analysis with JUST the basins with beaver ponds. Below, includes the catchments surveyed that didn't have beaver ponds


#Join the basins surveyed with the summarized beaver data 
all_VB_Y <- merge(x = vb_basins_surveyed, y = VB_Y_sums, by = "uRCA", all.x = TRUE)
##Join data zonal statistics data to beaver point data (Valley Bottom)
#Foliar cover (Nawrocki et al.)
all_VB_Y <- merge(x = all_VB_Y, y = foliar_vb, by = "uRCA", all.x = TRUE)
str(all_VB_Y)
#MTBS Fire Data
all_VB_Y <- merge(x = all_VB_Y, y = mtbs_fire_vb, by = "uRCA", all.x = TRUE)
str(all_VB_Y)
#Solar Data
all_VB_Y <- merge(x = all_VB_Y, y = solar_vb, by = "uRCA", all.x = TRUE)
head(all_VB_Y)
#Plant Functional Type (MaCander and Nelson 2022)
all_VB_Y <- merge(x = all_VB_Y, y = pft_2015_vb, by = "uRCA", all.x = TRUE)
head(all_VB_Y)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
all_VB_Y <- merge(x = all_VB_Y, y = ald_mean_vb, by = "uRCA", all.x = TRUE)
head(all_VB_Y)
###ADD NetMap Streamline Data!!!!!!!!!
all_VB_Y <- merge(x = all_VB_Y, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(all_VB_Y) ####
str(all_VB_Y, list.len=ncol(all_VB_Y))
sum(all_VB_Y$Dams_Per_Basin>="1", na.rm=T)
length(unique(all_VB_Y$uRCA))

all_VB_Y <- all_VB_Y %>% select(-contains("OID"))

all_VB_Y <- merge(x = all_VB_Y, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(all_VB_Y)

write.csv(all_VB_Y, file = "Output Data Files/all_VB_Y.csv")






###Repeat for RCAs
nrow(beaver_points_to_rca)
beaver_points_to_rca_new <- beaver_points_to_rca %>% 
  filter(Notes!="Error") %>% 
  filter(Notes!="error")
nrow(beaver_points_to_vb_new)

beaver_points_to_rca_new <- beaver_points_to_rca_new %>% 
  filter(Notes!="Contaminated") %>% 
  filter(Notes!="contaminated")
nrow(beaver_points_to_rca_new)

#beaver_points_to_rca_new <- beaver_points_to_rca[!(beaver_points_to_rca$Notes=="Error" | data$Notes=="error"),]
#beaver_points_to_rca_new <- beaver_points_to_rca_new[!(beaver_points_to_rca_new$Notes=="Contaminated" | data$Notes=="contaminated"),]

beaver_points_to_rca_new <- mutate(beaver_points_to_rca_new, Time_Since_Burn = 2017-Year)

beaver_points_to_rca_new <- beaver_points_to_rca_new %>% 
  mutate(Time_Since_Burn = ifelse(Time_Since_Burn == 2017, NA, Time_Since_Burn))

str(beaver_points_to_rca_new)

beaver_points_to_rca_new <- beaver_points_to_rca_new %>% 
  mutate(Burned = ifelse(is.na(Time_Since_Burn), 0, 1))

beaver_points_to_rca_new <- beaver_points_to_rca_new %>% 
  mutate(Main = ifelse(Dam_Type == "main", 1, 0))
beaver_points_to_rca_new <- beaver_points_to_rca_new %>% 
  mutate(Side = ifelse(Dam_Type == "side", 1, 0))
beaver_points_to_rca_new <- beaver_points_to_rca_new %>% 
  mutate(Off = ifelse(Dam_Type == "off", 1, 0))

length(unique(beaver_points_to_vb_new$uRCA))






#use pivot function to sum and average data by basin
dams_per_basin <- pivot(data=beaver_points_to_rca_new, compute=sum, variable=Join_Count, by=c(uRCA))
dams_per_basin
sum(dams_per_basin$Join_Count_sum>="0", na.rm=T)

burned_dams_per_basin <- pivot(data=beaver_points_to_rca_new, compute=mean, variable=Burned, by=c(uRCA))
burned_dams_per_basin
dam_area_per_basin_mean <-  pivot(data=beaver_points_to_rca_new, compute=mean, variable=Pond_Area_m2, by=c(uRCA))
dam_area_per_basin_mean ###NEED TO UPDAT THE DAM AREA BECAUSE ARCGIS WONT COMPUTE THE AREA FOR ALL THE DAMS
main_dam_per_basin <- pivot(data=beaver_points_to_rca_new, compute=sum, variable=Main, by=c(uRCA))
main_dam_per_basin
side_dam_per_basin <- pivot(data=beaver_points_to_rca_new, compute=sum, variable=Side, by=c(uRCA))
side_dam_per_basin
off_dam_per_basin <- pivot(data=beaver_points_to_rca_new, compute=sum, variable=Off, by=c(uRCA))
off_dam_per_basin
burned_numerical <- pivot(data=beaver_points_to_rca_new, compute=mean, variable=Burned, by=c(uRCA))
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
burned_numerical <- burned_numerical %>% 
  mutate(Burned_mean = ifelse(Burned_mean>0, 1, 0))
#beaver_points_to_vb_new$Time_Since_Burn <- as.numeric(beaver_points_to_vb_new$Time_Since_Burn)
time_since_burn <- pivot(data=beaver_points_to_rca_new, compute=min, variable=Time_Since_Burn, by=c(uRCA)) 
time_since_burn
#basin_size <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Basin_AreaSqKm, by=c(uRCA))
#basin_size CAN UPDATE THIS USING THE NETMAP DATA
beaver_points_to_vb_new$Z <- na_if(beaver_points_to_rca_new$Z, "<Null>")
beaver_points_to_vb_new$Z<-as.numeric(beaver_points_to_rca_new$Z)
dam_elevation_mean <- pivot(data=beaver_points_to_rca_new, compute=mean, variable=Z, by=c(uRCA))
dam_elevation_mean #THIS IS PROBABLY UNECCESSARY, I CAN USE THE NETMAP DATA

#Combine all these matricies
RCA_sums <- data.frame(c(dams_per_basin, burned_dams_per_basin, dam_area_per_basin_mean, main_dam_per_basin, side_dam_per_basin, off_dam_per_basin, burned_numerical, time_since_burn, dam_elevation_mean)) 
#View(Basin_sums)

#get rid of the useless columns
RCA_sums<-RCA_sums[,-c(3:5,7:11,13,15:17,19:21,23:27,29:31,33:35)] #23:28 gets rid of the burned numerical

#Replace Infinite values ("Inf") with NA
RCA_sums <- do.call(data.frame,                
                   lapply(RCA_sums,
                          function(x) replace(x, is.infinite(x), NA)))
RCA_sums <- RCA_sums %>%  dplyr::rename(Dams_Per_Basin = Join_Count_n)
RCA_sums <- RCA_sums %>%  dplyr::rename(Burned_numerical = Burned_mean.1)
RCA_sums <- RCA_sums %>%  dplyr::rename(uRCA = c.uRCA.)

head(RCA_sums)
str(RCA_sums)
length(unique(RCA_sums$uRCA))

write.csv(RCA_sums, file = "Output Data Files/RCA_sums.csv")


##Join data zonal statistics data to beaver point data (RCA data)
#Foliar cover (Nawrocki et al.)
RCA_sums_full <- merge(x = RCA_sums, y = foliar_rca, by = "uRCA", all.x = TRUE)
str(RCA_sums_full)
#MTBS Fire Data
RCA_sums_full <- merge(x = RCA_sums_full, y = mtbs_fire_rca, by = "uRCA", all.x = TRUE)
str(RCA_sums_full)
#Solar Data
RCA_sums_full <- merge(x = RCA_sums_full, y = solar_rca, by = "uRCA", all.x = TRUE)
head(RCA_sums_full)
#Plant Functional Type (MaCander and Nelson 2022)
RCA_sums_full <- merge(x = RCA_sums_full, y = pft_2015_rca, by = "uRCA", all.x = TRUE)
head(RCA_sums_full)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
RCA_sums_full <- merge(x = RCA_sums_full, y = ald_mean_rca, by = "uRCA", all.x = TRUE)
head(RCA_sums_full)
###ADD NetMap Streamline Data!!!!!!!!!
RCA_sums_full <- merge(x = RCA_sums_full, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(RCA_sums_full) ####
str(RCA_sums_full, list.len=ncol(RCA_sums_full))
sum(RCA_sums_full$Dams_Per_Basin>="1", na.rm=T)
length(unique(RCA_sums_full$uRCA))

RCA_sums_full <- RCA_sums_full %>% select(-contains("OID"))

RCA_sums_full <- merge(x = RCA_sums_full, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(RCA_sums_full)


write.csv(RCA_sums_full, file = "Output Data Files/RCA_sums_full.csv") #this is for analysis with JUST the basins with beaver ponds. Below, includes the catchments surveyed that didn't have beaver ponds


#Join the basins surveyed with the summarized beaver data
all_rca <- merge(x = rca_basins_surveyed, y = RCA_sums, by = "uRCA", all.x = TRUE)
##Join data zonal statistics data to beaver point data (Valley Bottom)
#Foliar cover (Nawrocki et al.)
all_rca <- merge(x = all_rca, y = foliar_rca, by = "uRCA", all.x = TRUE)
str(all_rca)
#MTBS Fire Data
all_rca <- merge(x = all_rca, y = mtbs_fire_rca, by = "uRCA", all.x = TRUE)
str(all_rca)
#Solar Data
all_rca <- merge(x = all_rca, y = solar_rca, by = "uRCA", all.x = TRUE)
head(all_rca)
#Plant Functional Type (MaCander and Nelson 2022)
all_rca <- merge(x = all_rca, y = pft_2015_rca, by = "uRCA", all.x = TRUE)
head(all_rca)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
all_rca <- merge(x = all_rca, y = ald_mean_rca, by = "uRCA", all.x = TRUE)
head(all_rca)
###ADD NetMap Streamline Data!!!!!!!!!
all_rca <- merge(x = all_rca, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(all_rca) ####
str(all_rca, list.len=ncol(all_rca))
sum(all_rca$Dams_Per_Basin>="1", na.rm=T)
length(unique(all_rca$uRCA))

all_rca <- all_rca %>% select(-contains("OID"))

all_rca <- merge(x = all_rca, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(all_rca)

write.csv(all_rca, file = "Output Data Files/all_rca.csv")



###NEED TO DO FOR Y DAMS TOO
bp_rca_Y_new <- beaver_points_to_rca_new %>% 
  filter(Beaver_cre == "Y")
#use pivot function to sum and average data by basin
dams_per_basin <- pivot(data=bp_rca_Y_new, compute=sum, variable=Join_Count, by=c(uRCA))
dams_per_basin
burned_dams_per_basin <- pivot(data=bp_rca_Y_new, compute=mean, variable=Burned, by=c(uRCA))
burned_dams_per_basin
dam_area_per_basin_mean <-  pivot(data=bp_rca_Y_new, compute=mean, variable=Pond_Area_m2, by=c(uRCA))
dam_area_per_basin_mean ###NEED TO UPDAT THE DAM AREA BECAUSE ARCGIS WONT COMPUTE THE AREA FOR ALL THE DAMS
main_dam_per_basin <- pivot(data=bp_rca_Y_new, compute=sum, variable=Main, by=c(uRCA))
main_dam_per_basin
side_dam_per_basin <- pivot(data=bp_rca_Y_new, compute=sum, variable=Side, by=c(uRCA))
side_dam_per_basin
off_dam_per_basin <- pivot(data=bp_rca_Y_new, compute=sum, variable=Off, by=c(uRCA))
off_dam_per_basin
burned_numerical <- pivot(data=bp_rca_Y_new, compute=mean, variable=Burned, by=c(uRCA))
#for this you need to turn it into a categorical variable (1=Burned, 0= unburned)
burned_numerical <- burned_numerical %>% 
  mutate(Burned_mean = ifelse(Burned_mean>0, 1, 0))
burned_numerical
#beaver_points_to_vb_new$Time_Since_Burn <- as.numeric(beaver_points_to_vb_new$Time_Since_Burn)
time_since_burn <- pivot(data=bp_rca_Y_new, compute=min, variable=Time_Since_Burn, by=c(uRCA)) 
time_since_burn
#basin_size <- pivot(data=beaver_points_to_vb_new, compute=mean, variable=Basin_AreaSqKm, by=c(uRCA))
#basin_size CAN UPDATE THIS USING THE NETMAP DATA
bp_rca_Y_new$Z <- na_if(bp_rca_Y_new$Z, "<Null>")
bp_rca_Y_new$Z<-as.numeric(bp_rca_Y_new$Z)
dam_elevation_mean <- pivot(data=bp_rca_Y_new, compute=mean, variable=Z, by=c(uRCA))
dam_elevation_mean #THIS IS PROBABLY UNECCESSARY, I CAN USE THE NETMAP DATA

#Combine all these matricies
RCA_Y_sums <- data.frame(c(dams_per_basin, burned_dams_per_basin, dam_area_per_basin_mean, main_dam_per_basin, side_dam_per_basin, off_dam_per_basin, burned_numerical, time_since_burn, dam_elevation_mean)) 
#View(Basin_sums)

#get rid of the useless columns
RCA_Y_sums<-RCA_Y_sums[,-c(3:5,7:11,13,15:17,19:21,23:27,29:31,33:35)] #23:28 gets rid of the burned numerical

#Replace Infinite values ("Inf") with NA
RCA_Y_sums <- do.call(data.frame,                
                     lapply(RCA_Y_sums,
                            function(x) replace(x, is.infinite(x), NA)))
RCA_Y_sums <- RCA_Y_sums %>%  dplyr::rename(Dams_Per_Basin = Join_Count_n)
RCA_Y_sums <- RCA_Y_sums %>%  dplyr::rename(burned_numerical = Burned_mean.1)
RCA_Y_sums <- RCA_Y_sums %>%  dplyr::rename(uRCA = c.uRCA.)

head(RCA_Y_sums)
length(unique(RCA_Y_sums$uRCA))


write.csv(RCA_Y_sums, file = "Output Data Files/RCA_Y_sums.csv")



##Join data zonal statistics data to beaver point data (RCA data)
#Foliar cover (Nawrocki et al.)
RCA_Y_sums_full <- merge(x = RCA_Y_sums, y = foliar_rca, by = "uRCA", all.x = TRUE)
str(RCA_Y_sums_full)
#MTBS Fire Data
RCA_Y_sums_full <- merge(x = RCA_Y_sums_full, y = mtbs_fire_rca, by = "uRCA", all.x = TRUE)
str(RCA_Y_sums_full)
#Solar Data
RCA_Y_sums_full <- merge(x = RCA_Y_sums_full, y = solar_rca, by = "uRCA", all.x = TRUE)
head(RCA_Y_sums_full)
#Plant Functional Type (MaCander and Nelson 2022)
RCA_Y_sums_full <- merge(x = RCA_Y_sums_full, y = pft_2015_rca, by = "uRCA", all.x = TRUE)
head(RCA_Y_sums_full)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
RCA_Y_sums_full <- merge(x = RCA_Y_sums_full, y = ald_mean_rca, by = "uRCA", all.x = TRUE)
head(RCA_Y_sums_full)
###ADD NetMap Streamline Data!!!!!!!!!
RCA_Y_sums_full <- merge(x = RCA_Y_sums_full, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(RCA_Y_sums_full) ####
str(RCA_Y_sums_full, list.len=ncol(RCA_Y_sums_full))
sum(RCA_Y_sums_full$Dams_Per_Basin>="1", na.rm=T)
length(unique(RCA_Y_sums_full$uRCA))

RCA_Y_sums_full <- RCA_Y_sums_full %>% select(-contains("OID"))

RCA_Y_sums_full <- merge(x = RCA_Y_sums_full, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(RCA_Y_sums_full)


write.csv(RCA_Y_sums_full, file = "Output Data Files/RCA_Y_sums_full.csv") #this is for analysis with JUST the basins with beaver ponds. Below, includes the catchments surveyed that didn't have beaver ponds




#Join the basins surveyed with the summarized beaver data
all_RCA_Y <- merge(x = rca_basins_surveyed, y = RCA_Y_sums, by = "uRCA", all.x = TRUE)
##Join data zonal statistics data to beaver point data (Valley Bottom)
#Foliar cover (Nawrocki et al.)
all_RCA_Y <- merge(x = all_RCA_Y, y = foliar_rca, by = "uRCA", all.x = TRUE)
str(all_RCA_Y)
#MTBS Fire Data
all_RCA_Y <- merge(x = all_RCA_Y, y = mtbs_fire_rca, by = "uRCA", all.x = TRUE)
str(all_RCA_Y)
#Solar Data
all_RCA_Y <- merge(x = all_RCA_Y, y = solar_rca, by = "uRCA", all.x = TRUE)
head(all_RCA_Y)
#Plant Functional Type (MaCander and Nelson 2022)
all_RCA_Y <- merge(x = all_RCA_Y, y = pft_2015_rca, by = "uRCA", all.x = TRUE)
head(all_RCA_Y)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
all_RCA_Y <- merge(x = all_RCA_Y, y = ald_mean_rca, by = "uRCA", all.x = TRUE)
head(all_RCA_Y)
###ADD NetMap Streamline Data!!!!!!!!!
all_RCA_Y <- merge(x = all_RCA_Y, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(all_RCA_Y) ####
str(all_RCA_Y, list.len=ncol(all_RCA_Y))
sum(all_RCA_Y$Dams_Per_Basin>="1", na.rm=T)
length(unique(all_RCA_Y$uRCA))


all_RCA_Y <- all_RCA_Y %>% select(-contains("OID"))

all_RCA_Y <- merge(x = all_RCA_Y, y = pft_2015_vb[,c(2,9)], by = "uRCA", all.x = TRUE)
head(all_RCA_Y)

write.csv(all_RCA_Y, file = "Output Data Files/all_RCA_Y.csv")



#_______________________________________________________________________________





# Blank data (no beaver) for predictions ----------------------------------

#Do it for Valley Bottoms
prediction_blank_VB <- merge(x = foliar_vb, y = mtbs_fire_vb, by = "uRCA", all.x = TRUE)
str(prediction_blank_VB)
#Solar Data
prediction_blank_VB <- merge(x = prediction_blank_VB, y = solar_vb, by = "uRCA", all.x = TRUE)
head(prediction_blank_VB)
#Plant Functional Type (MaCander and Nelson 2022)
prediction_blank_VB <- merge(x = prediction_blank_VB, y = pft_2015_vb, by = "uRCA", all.x = TRUE)
head(prediction_blank_VB)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
prediction_blank_VB <- merge(x = prediction_blank_VB, y = ald_mean_vb, by = "uRCA", all.x = TRUE)
head(prediction_blank_VB)
###ADD NetMap Streamline Data!!!!!!!!!
prediction_blank_VB <- merge(x = prediction_blank_VB, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(prediction_blank_VB) ####
str(prediction_blank_VB, list.len=ncol(all_VB))
length(unique(prediction_blank_VB$uRCA))

which(duplicated(names(prediction_blank_VB)))
prediction_blank_VB <- prediction_blank_VB[,-c(40)]

write.csv(prediction_blank_VB, file = "Output Data Files/Prediction_blank_VB.csv") 


#Do it for RCAs
prediction_blank_RCA <- merge(x = foliar_rca, y = mtbs_fire_rca, by = "uRCA", all.x = TRUE)
str(prediction_blank_RCA)
#Solar Data
prediction_blank_RCA <- merge(x = prediction_blank_RCA, y = solar_rca, by = "uRCA", all.x = TRUE)
head(prediction_blank_RCA)
#Plant Functional Type (MaCander and Nelson 2022)
prediction_blank_RCA <- merge(x = prediction_blank_RCA, y = pft_2015_rca, by = "uRCA", all.x = TRUE)
head(prediction_blank_RCA)
#Active Layer Depth (permafrost from Terrestrial Ecosystem Model, Helene Genet)
prediction_blank_RCA <- merge(x = prediction_blank_RCA, y = ald_mean_rca, by = "uRCA", all.x = TRUE)
head(prediction_blank_RCA)
###ADD NetMap Streamline Data!!!!!!!!!
prediction_blank_RCA <- merge(x = prediction_blank_RCA, y = netmap_sums, by = "uRCA", all.x = TRUE)
head(prediction_blank_RCA) ####
str(prediction_blank_RCA, list.len=ncol(all_VB))
length(unique(prediction_blank_RCA$uRCA))

which(duplicated(names(prediction_blank_RCA)))
prediction_blank_RCA <- prediction_blank_RCA[,-c(40)]

write.csv(prediction_blank_RCA, file = "Output Data Files/Prediction_blank_RCA.csv") 




# Regression Predictors ---------------------------------------------------

str(RCA_sums_full, list.len=ncol(all_RCA))
###Organized Model Predictors

##BEAVER DATA
#Dams per Km2                 Dams_Per_Basin/AREA_SQKM_mean
#Mean Pond area (m2) per Km2  Pond_Area_m2_mean/AREA_SQKM_mean   
#Percent of Main Ponds        Main_n/Dams_Per_Basin
#Percent of Side Ponds        Side_n/Dams_Per_Basin
#Percent of Off Ponds         Off_n/Dams_Per_Basin
  #(Netmap extra)
  #BeavHab_min

##WILDFIRE
#Percent Burned (RCA, VB)     Fire_Event_Area_Percent_of_uRCA
#Time Since Last Burn         Time_Since_Burn_min
#Percent Sever Burn           BS4_AreaKm2/Area_km2_VB
#Percent Moderate Burn        BS3_AreaKm2/Area_km2_VB
#Percent Mild Burn            BS2_AreaKm2/Area_km2_VB
#Percent Mild Burn            BS1_AreaKm2/Area_km2_VB
#Majority of Burn Severity    ??????????????????????????????????????????????????

##GEOMORPHOLOGY
#Stream Gradient (mean)       GRADIENT_mean
#Stream Gradient (max)        MAX_GRAD_D_max
#Floodplain Width             FP_WIDTH_mean         Probably similar to VW and VWI
#Valley Width                 VAL_WIDTH_mean
#Valley Width Index           VWI_Floor_mean
#Drainage Area                Area_km2_VB or Area_km2_RCA
#Sinuosity                    SINUOSITY_mean
#Stream Order                 STRM_ORDER_max
#Stream Width                 WIDTH_M_mean
#Stream Depth                 DEPTH_M_mean
#Elevation                    ELEV_M_mean
#Valley Constraint (mean)     ValCnstrnt_mean       Probably the same as VWI
#Aspect                       AZIMTH_DEG_mean       Maybe important? Maybe Solar_Mean is better?
#General Erosion Potential    GEP_mean              Probably not that necessary

##HYDROLOGY
#Stream Power                 StrmPow_mean             
#Flow Velocity                FlowVel_mean
#Mean Annual Discharge        MEANANNCMS_mean       Maybe the best?
#Bank Full Streamflow         BFQ_mean              This has been significant
#Lake?                        LAKE_max

corr <- round(cor(full.dat[c(1:97),c(39:61)]), 1) #NEED TO UPDATE
ggcorrplot(corr, type = "lower", lab = TRUE)
ggcorrplot(corr, type = "lower", lab = TRUE, method = "circle")

  
##VEGETATION
str(pft_2015_rca) #PFT_... : Percent plant functional type for year 2015. This is a LandSat based, 30m resolution dataset from Macander and Nelson (2022).
#Dominant Vegetation Class    ####NEED TO FIGURE OUT HOW TO SELECT THE HIGHEST????? PERCENTAGE, JUST LIKE BURN SEVERITY
#Vegetation Density           Sum all the other catagories together?????????????
#Percent Conifer              ConiferTree_2015_MEAN
#Percent Broadleaf            BroadleafTree_2015_MEAN
#Percent Deciduous Shrub      DeciduousShrub_2015_MEAN
#Percent Evergreen Shrub      EvergreenShrub_2015_MEAN
#Percent Forb                 Forb_2015_MEAN
#Percent Grammoid             Graminoid_2015_MEAN
#Percent Lichen               tmLichenLight_2015_MEAN

str(foliar_rca) #FOLIAR_... : Percent foliar cover for multiple species. This is a 10m resolution dataset from Nawrocki (2020) and uses satellite data from 2017-2020.
#Percent Deciduous Tree       dectre_PCT_MEAN 
#Deciduous:Conifer Ratio    (salshr_PCT_MEAN + alnus_PCT_MEAN + bettre_PCT_MEAN + betshr_PCT_MEAN+ vaculi_PCT_MEAN)/(picgla_PCT_MEAN + picmar_PCT_MEAN)
#Percent Willow               salshr_PCT_MEAN 
#Percent Alder                alnus_PCT_MEAN 
#Percent Birch Tree           bettre_PCT_MEAN             Combine birch shrubs and trees
#Percent Birch Shrub          betshr_PCT_MEAN
#Percent Aspen                NA
#Percent White Spruce         picgla_PCT_MEAN
#Percent Black Spruce         picmar_PCT_MEAN
#Percent Cottonwood           NA
#Percent Dryas                dryas_PCT_MEAN
#Percent Crowberry            empnig_PCT_MEAN
#Percent Tussok Cottongrass   erivag_PCT_MEAN
#Percent Rhododendron shrubs  rhoshr_PCT_MEAN
#Percent Sphagnum Moss        sphagn_PCT_MEAN
#Percent Blueberry shrub      vaculi_PCT_MEAN
#Percent Lowbush Cranberry    vacvit_PCT_MEAN
#Percent Wetland Sedge        ???????????????

### do a histogram of these, eliminate small categories

##SOLAR                       Watts per Square Meter (WH/m2)
#Solar min
#Solar max                    
#Solar range
#Solar mean                   Solar radiation
#Solar St.Dev
#Solar sum

##PERMAFROST
#Active Layer Depth           ALD_2015_MEAN

##INTERACTIVE EFFECTS
#Proximity to Another Pond??
#Number of Ponds within X m??
#Sub-Basin Beaver Pond Density??
#Number of ponds in the downstream RCA? Upstream RCA?   



###Need to do for all_RCA, all_RCA_Y, all_VB, all_VB_Y, VB_sums, VB_Y_sums, RCA_sums, RCA_Y_sums
VB_Y_sums_full <- VB_Y_sums_full %>% 
  ##BEAVER DATA
  mutate(dams_per_km2 = (Dams_Per_Basin/Area_km2_VB)) %>%
  mutate(dams_per_km_linear = (Dams_Per_Basin/LENGTH_M_sum)) %>%
  mutate(mean_pond_area_m2perkm2 = round((Pond_Area_m2_mean/Area_km2_VB))) %>% 
  mutate(percent_main_ponds = ((Main_n/Dams_Per_Basin)*100)) %>% #This is not calcuating right
  mutate(percent_side_ponds = ((Side_n/Dams_Per_Basin)*100)) %>% #This is not calcuating right
  mutate(percent_off_ponds = ((Off_n/Dams_Per_Basin)*100)) %>% #This is not calcuating right
  ##WILDFIRE
  #mutate(percent_burned = ((BS_sum_AreaKm2/Area_km2_VB)*100)) %>%
  
  #Define which RCAs and VBs are burned
  mutate(Burned_basin = ifelse(Fire_Event_Area_Percent_of_uRCA >= 10, 1, 0)) %>% 
  mutate(Burned_basin = ifelse(is.na(Fire_Event_Area_Percent_of_uRCA), 0, 1)) %>%

  mutate(percent_sever_burn = ((BS4_AreaKm2/Area_km2_VB)*100)) %>%     
  mutate(percent_moderate_burn = ((BS3_AreaKm2/Area_km2_VB)*100)) %>%  
  mutate(percent_mild_burn = ((BS2_AreaKm2/Area_km2_VB)*100))  %>%  
  mutate(percent_no_burn = ((BS1_AreaKm2/Area_km2_VB)*100))  %>%     
  #mutate(majority_burn_severity = (????????????????????????????)) %>% ###How to calculate this?
  ##GEOMORPHOLOGY
    #No edited needed
  ##HYDROLOGY
    #No edited needed
  ##VEGETATION
  mutate("percent_woody_cover" = (sumrow = salshr_PCT_MEAN+alnus_PCT_MEAN+bettre_PCT_MEAN+betshr_PCT_MEAN+ picgla_PCT_MEAN+picmar_PCT_MEAN+vaculi_PCT_MEAN)) %>% 
  mutate("decidous_conifer_ratio" = ((salshr_PCT_MEAN + alnus_PCT_MEAN + bettre_PCT_MEAN + betshr_PCT_MEAN+ vaculi_PCT_MEAN)/(picgla_PCT_MEAN + picmar_PCT_MEAN))) #%>% 
   #mutate(dominant_vegetation_class = (????????????????????????????)) %>% ??????????????
  ##SOLAR
    #No edited needed
  ##PERMAFROST
    #No edited needed
  ##INTERACTIVE EFFECTS
    #Proximity to Another Pond??
    #Number of Ponds within X m??
    #Sub-Basin Beaver Pond Density??
    #Number of ponds in the downstream RCA? Upstream RCA? 


#VB_Y_sums_full$Burned_basin <- as.factor(VB_Y_sums_full$Burned_basin)
#VB_Y_sums_full$LAKE_max <- as.factor(VB_Y_sums_full$LAKE_max)
#VB_Y_sums_full$picmar_PCT_MEAN <- as.numeric(VB_Y_sums_full$picmar_PCT_MEAN)


write.csv(VB_Y_sums_full, file = "Output Data Files/VB_Y_sums_full.csv") 





###
###REPEAT FOR: all_RCA_Y, all_VB, all_VB_Y, VB_sums, VB_Y_sums, RCA_sums, RCA_Y_sums
###



all_VB_Y <- all_VB_Y %>% 
  ##BEAVER DATA
  mutate(Dams_Per_Basin = ifelse(is.na(Dams_Per_Basin), 0, Dams_Per_Basin)) %>%
  mutate(Pond_Area_m2_mean = ifelse(is.na(Pond_Area_m2_mean), 0, Pond_Area_m2_mean)) %>%
    mutate(Main_n = ifelse(is.na(Main_n), 0, Main_n)) %>%
  mutate(Side_n = ifelse(is.na(Side_n), 0, Side_n)) %>%
  mutate(Off_n = ifelse(is.na(Off_n), 0, Off_n)) %>%
  
                    
  mutate(dams_per_km2 = (Dams_Per_Basin/Area_km2_VB)) %>%
  mutate(dams_per_km_linear = (Dams_Per_Basin/LENGTH_M_sum)) %>%
  mutate(mean_pond_area_m2perkm2 = (Pond_Area_m2_mean/Area_km2_VB)) %>% #This is not calculating right
  mutate(percent_main_ponds = ((Main_n/Dams_Per_Basin)*100)) %>% #This is not calculating right
  mutate(percent_side_ponds = ((Side_n/Dams_Per_Basin)*100)) %>% #This is not calculating right
  mutate(percent_off_ponds = ((Off_n/Dams_Per_Basin)*100)) %>% #This is not calculating right
  ##WILDFIRE
  #mutate(percent_burned = ((BS_sum_AreaKm2/Area_km2_VB)*100)) %>% Doesn't calculate right
  mutate(Burned_basin = ifelse(Fire_Event_Area_Percent_of_uRCA >= 10, 1, 0)) %>% 
  mutate(Burned_basin = ifelse(is.na(Fire_Event_Area_Percent_of_uRCA), 0, 1)) %>%
  
  mutate(percent_sever_burn = ((BS4_AreaKm2/Area_km2_VB)*100)) %>%     
  mutate(percent_moderate_burn = ((BS3_AreaKm2/Area_km2_VB)*100)) %>%  
  mutate(percent_mild_burn = ((BS2_AreaKm2/Area_km2_VB)*100))  %>%  
  mutate(percent_no_burn = ((BS1_AreaKm2/Area_km2_VB)*100))  %>%     
  #mutate(majority_burn_severity = (????????????????????????????)) %>% ###How to calculate this?
  ##GEOMORPHOLOGY
  #No edited needed
  ##HYDROLOGY
  #No edited needed
  ##VEGETATION
  mutate("percent_woody_cover" = (sumrow = salshr_PCT_MEAN+alnus_PCT_MEAN+bettre_PCT_MEAN+betshr_PCT_MEAN+ picgla_PCT_MEAN+picmar_PCT_MEAN+vaculi_PCT_MEAN)) %>% 
  mutate("decidous_conifer_ratio" = ((salshr_PCT_MEAN + alnus_PCT_MEAN + bettre_PCT_MEAN + betshr_PCT_MEAN+ vaculi_PCT_MEAN)/(picgla_PCT_MEAN + picmar_PCT_MEAN))) #%>% 
#mutate(dominant_vegetation_class = (????????????????????????????)) %>% ??????????????
##SOLAR
#No editing needed
##PERMAFROST
#No edited needed
##INTERACTIVE EFFECTS
#Proximity to Another Pond??
#Number of Ponds within X m??
#Sub-Basin Beaver Pond Density??
#Number of ponds in the downstream RCA? Upstream RCA? 


write.csv(all_VB_Y, file = "Output Data Files/all_VB_Y.csv") 


#all_VB_Y$Burned_basin <- as.factor(all_VB_Y$Burned_basin)
#all_VB_Y$LAKE_max <- as.factor(all_VB_Y$LAKE_max)
#all_VB_Y$picmar_PCT_MEAN <- as.numeric(all_VB_Y$picmar_PCT_MEAN)


# Variance Inflation Factor and Colinearity ------------------------------------

#Remove Weird row on the bottom
str(VB_Y_sums_full)
VB_Y_sums_full <- VB_Y_sums_full[c(-195),]
str(VB_Y_sums_full)


vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}



VIF<-function(X) {
  #Computes Variance Inflation Factors for a Predictor Matrix
  #INPUTS:
  #X is a matrix (or data frame) of the predictors (no column of ones).
  cat("REMINDER: Your input matrix should not include the response\n")
  a<-1/sqrt(dim(X)[1]-1)*scale(X)
  b<-cbind(diag(solve(t(a)%*%a)))
  dimnames(b)<-list(dimnames(X)[[2]],"VIF")
  return(b)
}



##VIF for VB_Y_sums
VB_Y_sums_forVIF <- as.numeric(VB_Y_sums_full)  %>% 
  select(
    #GEOMORPHOLOGY
    "GRADIENT_mean", "MAX_GRAD_D_max", "FP_WIDTH_mean", "VAL_WIDTH_mean", "Area_km2_VB", "SINUOSITY_mean", "STRM_ORDER_max", "WIDTH_M_mean", "DEPTH_M_mean", "ELEV_M_mean","VAL_WIDTH_mean", "ValCnstrnt_mean", "AZIMTH_DEG_mean", "GEP_mean",
    #HYDROLOGY
    "StrmPow_mean", "FlowVel_mean", "MEANANNCMS_mean", "BFQ_mean", "LAKE_max",
    #WILDFIRE
    "Time_Since_Burn_min", "Fire_Event_Area_Percent_of_uRCA", "percent_sever_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn", #majority_burn_severity+Burned_basin+
    #VEGETATION
    #From MaCander and Nelson
    "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN", "DeciduousShrub_2015_MEAN", "EvergreenShrub_2015_MEAN", "Forb_2015_MEAN", "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN", 
    #From Nawrocki et al.
    "percent_woody_cover", "dectre_PCT_MEAN", "salshr_PCT_MEAN", "vaculi_PCT_MEAN", "alnus_PCT_MEAN", "betshr_PCT_MEAN", "bettre_PCT_MEAN", "picgla_PCT_MEAN", "picmar_PCT_MEAN", "dryas_PCT_MEAN", "empnig_PCT_MEAN", "erivag_PCT_MEAN", "rhoshr_PCT_MEAN", "sphagn_PCT_MEAN", "vacvit_PCT_MEAN", 
    #dom_veg_class+
    #decidous_conifer_ratio+ I AM NOT SURE WHY THIS MESSES THE MODEL UP
    #SOLAR
    "SOLAR_MEAN",                   
    #PERMAFROST
    "ALD_2015_MEAN"
)
VB_Y_sums_forVIF


VB_Y_sums_postVIF_func <- vif_func(VB_Y_sums_forVIF)
VB_Y_sums_postVIF_func
#removed:  percent_woody_cover Inf                  Need to address this
#removed:  DEPTH_M_mean 29280.37                    Maybe combine W + D?
#removed:  WIDTH_M_mean 579.6892                    What about all these NetMap predictors?
#removed:  StrmPow_mean 291.7032 
#removed:  FP_WIDTH_mean 260.2996 
#removed:  FlowVel_mean 176.0382 
#removed:  betshr_PCT_MEAN 78.80537                 #Can probably combine Birch Shrubs and Birch Trees
#removed:  ELEV_M_mean 46.21613 
#removed:  vacvit_PCT_MEAN 36.23644 
#removed:  DeciduousShrub_2015_MEAN 33.62433        
#removed:  vaculi_PCT_MEAN 30.10248 
#removed:  ConiferTree_2015_MEAN 29.82068 
#removed:  sphagn_PCT_MEAN 25.13644 
#removed:  Time_Since_Burn_min 18.90076             #This seems important
#removed:  bettre_PCT_MEAN 17.8024 
#removed:  MEANANNCMS_mean 13.16036         #represents catchment area, width, depth
#removed:  picgla_PCT_MEAN 10.85398 


VB_Y_sums_postVIF <- VIF(VB_Y_sums_forVIF)
VB_Y_sums_postVIF



#Kevins old code
#VIF<-function(X) {
  #Computes Variance Inflation Factors for a Predictor Matrix
  #INPUTS:
  #X is a matrix (or data frame) of the predictors (no column of ones).
#  cat("REMINDER: Your input matrix should not include the response\n")
#  a<-1/sqrt(dim(X)[1]-1)*scale(X)
#  b<-cbind(diag(solve(t(a)%*%a)))
#  dimnames(b)<-list(dimnames(X)[[2]],"VIF")
#  return(b)
#}

#.for_VIF <- (full.dat[c(1:97),c(39:61)]) 
#summary(.for_VIF)

#?cor
#VIF.results <- cor(.for_VIF, use="complete")
#summary(VIF.results)
#VIF.results

?corrplot
corrplot(.for_VIF, method = 'number')
corrplot(.for_VIF, type = lower)



corr <- round(cor(full.dat[c(1:97),c(39:61)]), 1) #NEED TO UPDATE
ggcorrplot(corr, type = "lower", lab = TRUE)
ggcorrplot(corr, type = "lower", lab = TRUE, method = "circle")



# Z-Score or standardize distributions? -----------------------------------



ggplot(VB_Y_sums_full, aes(x = dams_per_km2))+ 
  geom_density()+
  #geom_histogram()+
  theme_cowplot()


sapply(data, function(data) (data-mean(data))/sd(data))

sapply(VB_Y_sums_full, function(VB_Y_sums_full) (VB_Y_sums_full-mean(VB_Y_sums_full))/sd(VB_Y_sums_full)) #This is NOT working.......


ggplot(VB_Y_sums_full, aes(x = dams_per_km2))+ 
  geom_density()+
  #geom_histogram()+
  theme_cowplot()

# Models ---------------------------------------------------------------

?glm()
str(VB_Y_sums_full, list.len=ncol(VB_Y_sums_full))
names(VB_Y_sums_full)


#Filter out large sections of the river
#nrow(VB_Y_sums_full)
#VB_Y_sums_full <- VB_Y_sums_full %>% 
#  filter(STRM_ORDER_max <= 5)
#nrow(VB_Y_sums_full)


#Based off of Netmap
#test.model <- glm(dams_per_km2~GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+AREA_SQKM_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+AZIMTH_DEG_mean+GEP_mean+StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+LAKE_max, data = VB_Y_sums_full)
#summary(test.model)

#test.model2 <- glm(dams_per_km2~Time_Since_Burn_min+percent_burned+Fire_Event_Area_Percent_of_uRCA, data = VB_Y_sums_full)
#summary(test.model2)

#test.model3 <- glm(mean_pond_area_m2perkm2~Time_Since_Burn_min+percent_burned+Fire_Event_Area_Percent_of_uRCA, data = VB_Y_sums_full)
#summary(test.model3)


vb.null <- glm(dams_per_km2~ 1, data = VB_Y_sums_full)
summary(vb.null)
nagelkerke(vb.null)
accuracy(list(vb.null))
#Pseudo.R.squared
#McFadden                            0
#Cox and Snell (ML)                  0
#Nagelkerke (Cragg and Uhler)        0
#Efron.r.squared                     0


#So far it seems like NetMap and Wildfire explain the vast majority of variance


#global model for VB polygons WITH beaver ponds (no 0 catchments)
global.vb.full <- glm(dams_per_km2~ 
  #GEOMORPHOLOGY
    GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+AZIMTH_DEG_mean+GEP_mean+
  #HYDROLOGY
    StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+#LAKE_max+
  #WILDFIRE
    Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+#majority_burn_severity+Burned_basin+
  #VEGETATION
    #From MaCander and Nelson
    ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
    #From Nawrocki et al.
    percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN+
    #dom_veg_class+
    #decidous_conifer_ratio+ I AM NOT SURE WHY THIS MESSES THE MODEL UP
  #SOLAR
    SOLAR_MEAN+                   
  #PERMAFROST
    ALD_2015_MEAN
  #INTERACTIVE EFFECTS???
    , data = VB_Y_sums_full)
summary(global.vb.full)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(global.vb.full)
accuracy(list(global.vb.full))
#Pseudo.R.squared                               When I include a random slope....
#McFadden                            0.918      0.962
#Cox and Snell (ML)                  1          1
#Nagelkerke (Cragg and Uhler)        1          1
#Efron.r.squared                     0.978      0.994





#UPDATED global model for VB polygons WITH beaver ponds (no 0 catchments)
global.vb.full.updated <- glm(dams_per_km2~ 
                        #GEOMORPHOLOGY
                        GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                        #HYDROLOGY
                        StrmPow_mean+MEANANNCMS_mean+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+
                        #VEGETATION
                        #From MaCander and Nelson
                        ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
                        #From Nawrocki et al.
                        #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
                        
                      , data = VB_Y_sums_full)

summary(global.vb.full.updated)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(global.vb.full.updated)
accuracy(list(global.vb.full.updated))
#Pseudo.R.squared                               
#McFadden                            0.843      
#Cox and Snell (ML)                  1          
#Nagelkerke (Cragg and Uhler)        1          
#Efron.r.squared                     0.819     



#It looks like it did looks some r2 with less predictors, but not that much, only about 3%, which is worth it for eliminating that many predictors. Still need to reduce the vegetation variables.

#Without Nawrocki, r2 = 0.84, 1, 1, 0.80
#Without Macander, r2 = 0.83, 1, 1, 0.77
#Overall, it looks like the Macander is a way better predictor when looking at the p-values

________________________________________________________________________________

vb.netmap <- glm(dams_per_km2~
                        #GEOMORPHOLOGY
                        GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+AREA_SQKM_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+AZIMTH_DEG_mean+GEP_mean+
                        #HYDROLOGY
                        StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+LAKE_max
                        , data = VB_Y_sums_full)
summary(vb.netmap) #Significant!
nagelkerke(vb.netmap)
accuracy(list(vb.netmap))
#Pseudo.R.squared
#McFadden                            0.035
#Cox and Snell (ML)                  0.23
#Nagelkerke (Cragg and Uhler)        0.23
#Efron.r.squared                     0.233
________________________________________________________________________________

vb.fire <- glm(dams_per_km2~
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA
               +percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn#+majority_burn_severity
                        , data = VB_Y_sums_full) #Significant!
summary(vb.fire)
nagelkerke(vb.fire)
accuracy(list(vb.fire))
#Pseudo.R.squared
#McFadden                            0.791
#Cox and Snell (ML)                  1
#Nagelkerke (Cragg and Uhler)        1
#Efron.r.squared                     0.213
________________________________________________________________________________

vb.veg <- glm(dams_per_km2~
                        #VEGETATION
                #MaCander and Nelson
                ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
                #From Nawrocki et al.
                percent_woody_cover+decidous_conifer_ratio+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
                #dom_veg_class+
                       , data = VB_Y_sums_full)
summary(vb.veg) 
nagelkerke(vb.veg)
accuracy(list(vb.veg))
#Pseudo.R.squared
#McFadden                            0.062
#Cox and Snell (ML)                  0.378
#Nagelkerke (Cragg and Uhler)        0.378
#Efron.r.squared                     0.355

#OUT OF THE TWO, THE NAWROCKI EXPLAINS MUCH MORE VARIANCE
________________________________________________________________________________

vb.solar <- glm(dams_per_km2~
                        #SOLAR
                        SOLAR_MEAN                
                        , data = VB_Y_sums_full)
summary(vb.solar)
nagelkerke(vb.solar)
accuracy(list(vb.solar))
#Pseudo.R.squared
#McFadden                            0.005
#Cox and Snell (ML)                  0.036
#Nagelkerke (Cragg and Uhler)        0.036
#Efron.r.squared                     0.0001
________________________________________________________________________________

vb.permafrost <- glm(dams_per_km2~
                        #PERMAFROST
                        ALD_2015_MEAN
                        , data = VB_Y_sums_full)
summary(vb.permafrost) #Significant!
nagelkerke(vb.permafrost)
accuracy(list(vb.permafrost))
#Pseudo.R.squared
#McFadden                            0.005
#Cox and Snell (ML)                  0.039
#Nagelkerke (Cragg and Uhler)        0.039
#Efron.r.squared                     0.004
________________________________________________________________________________

vb.netmap.fire <- glm(dams_per_km2~
                        #GEOMORPHOLOGY
                        GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+AREA_SQKM_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+AZIMTH_DEG_mean+GEP_mean+
                        #HYDROLOGY
                        StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+LAKE_max+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn#+majority_burn_severity+
                        #VEGETATION
                        #ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+#dom_veg_class+veg_density+perc_veg_cover+ ###ADD NAWROCKI ET AL DATA alnus_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN etc........................
                        #SOLAR
                        #SOLAR_MEAN+SOLAR_MAX+                    
                        #PERMAFROST
                        #ALD_2015_MEAN
                      #INTERACTIVE EFFECTS???
                      , data = VB_Y_sums_full)
summary(vb.netmap.fire) #geomorphological parameraters and percent burned both significant!
nagelkerke(vb.netmap.fire)
accuracy(list(vb.netmap.fire))
#Pseudo.R.squared
#McFadden                            0.826
#Cox and Snell (ML)                  1
#Nagelkerke (Cragg and Uhler)        1
#Efron.r.squared                     0.709

plot(dams_per_km2~GRADIENT_mean, data = VB_Y_sums_full)
plot(dams_per_km2~ValCnstrnt_mean, data = VB_Y_sums_full)
plot(dams_per_km2~percent_burned, data = VB_Y_sums_full)

________________________________________________________________________________

#Model informed by hypotheses in polygons WITH beaver ponds (no 0 catchments)
options(na.action = "na.omit")
informed.vb.full <- glm(dams_per_km2~
                        #GEOMORPHOLOGY
                        GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+#GEP_mean+#Area_km2_VB+AZIMTH_DEG_mean+
                        #HYDROLOGY
                        StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+#LAKE_max+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+#Fire_Event_Area_Percent_of_uRCA+majority_burn_severity+
                        #VEGETATION
                        #From MaCander and Nelson
                        #ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
                        #From Nawrocki et al.
                        percent_woody_cover+dectre_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+dryas_PCT_MEAN+erivag_PCT_MEAN+sphagn_PCT_MEAN+#+picgla_PCT_MEAN+picmar_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+rhoshr_PCT_MEAN+vacvit_PCT_MEAN+empnig_PCT_MEAN+
                        #dom_veg_class+
                        #decidous_conifer_ratio+ I AM NOT SURE WHY THIS MESSES THE MODEL UP
                        #SOLAR
                        SOLAR_MEAN+                   
                        #PERMAFROST
                        ALD_2015_MEAN
                      #INTERACTIVE EFFECTS???
                      , data = VB_Y_sums_full)
summary(informed.vb.full)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(informed.vb.full)
accuracy(list(informed.vb.full))
#Pseudo.R.squared
#McFadden                            0.83
#Cox and Snell (ML)                  1
#Nagelkerke (Cragg and Uhler)        1
#Efron.r.squared                     0.763
par(mfrow = c(2, 2))
plot(informed.vb.full)
install.packages("ModEvA")
library(ModEvA)
plotGLM(informed.vb.full)

par(mfrow = c(1,1))
plot(informed.vb.full, 4) #Looks like there are a decent number of outliers, row 13 is really bad (200), also row 38, 106, 107.... Doesn't mean we should neccessarily exlude them all....
#To remove row 13, use this code and then re-run the model VV
#VB_Y_sums_full <- VB_Y_sums_full[-c(13),] 


ggsave(plot= model_assessment,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Beaver_Project_2/Figures/model assessment.jpeg",
       dpi = 2000, 
       height = 6,
       width = 6,
       units = "in")




plot(informed.vb.full$dams_per_km2, informed.vb.full$dams_per_km_linear)

plot(informed.vb.full$dams_per_km2, informed.vb.full$mean_pond_area_m2perkm2)


#Maybe try model selection with MuMIn???

#exclude only pairs of variables having cor. coefficient r > 0.7
#smat <- abs(cor(GRADIENT_mean, MAX_GRAD_D_max, FP_WIDTH_mean, VAL_WIDTH_mean, VWI_Floor_mean, SINUOSITY_mean, STRM_ORDER_max, WIDTH_M_mean, DEPTH_M_mean, ELEV_M_mean, ValCnstrnt_mean, StrmPow_mean, FlowVel_mean, MEANANNCMS_mean, BFQ_mean, Time_Since_Burn_min, percent_burned, percent_sever_burn, percent_moderate_burn, percent_mild_burn, percent_no_burn,percent_woody_cover, dectre_PCT_MEAN, betshr_PCT_MEAN, bettre_PCT_MEAN, dryas_PCT_MEAN, erivag_PCT_MEAN, sphagn_PCT_MEAN, SOLAR_MEAN, ALD_2015_MEAN)) <= .7
#smat[!lower.tri(smat)] <- NA
#?dredge
options(na.action = "na.fail")
dredge(informed.vb.full, trace = 2, evaluate = TRUE, extra = c("R^2", "adjR^2"))# extra = c("R^2", "adjR^2"), subset = smat), beta = "none")
#performs an automated model selection with subsets of the supplied 'global' model and optional choices of other model properties (such as different link functions). The set of models can be generated with 'all possible' combinations or tailored according to the conditions specified.
#Error in dredge(global.vb.full) :number of non-fixed predictors [50] exceeds the allowed maximum of 31 (with 1 variants)
model.sel(informed.vb.full) #creates a model selection table from selected models
AICc(informed.vb.full)
options(na.action = "na.omit")


________________________________________________________________________________

#This is with ALLLL RCAs including the ones with no beaver ponds
informed.vb.all <- glm(dams_per_km2~
                          #GEOMORPHOLOGY
                          GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+#GEP_mean+#Area_km2_VB+AZIMTH_DEG_mean+
                          #HYDROLOGY
                          StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+#LAKE_max+
                          #WILDFIRE
                          #Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+#Fire_Event_Area_Percent_of_uRCA+majority_burn_severity+
                          #VEGETATION
                          #From MaCander and Nelson
                          #ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
                          #From Nawrocki et al.
                          percent_woody_cover+dectre_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+dryas_PCT_MEAN+erivag_PCT_MEAN+sphagn_PCT_MEAN+#+picgla_PCT_MEAN+picmar_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+rhoshr_PCT_MEAN+vacvit_PCT_MEAN+empnig_PCT_MEAN+
                          #dom_veg_class+
                          #decidous_conifer_ratio+ I AM NOT SURE WHY THIS MESSES THE MODEL UP
                          #SOLAR
                          SOLAR_MEAN+                   
                          #PERMAFROST
                          ALD_2015_MEAN
                        #INTERACTIVE EFFECTS???
                        , data = all_VB_Y)
summary(informed.vb.all)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(informed.vb.all)
accuracy(list(informed.vb.all))
#Pseudo.R.squared                                     Without Wildfire....
#McFadden                            0.989            0.021
#Cox and Snell (ML)                  1                0.103
#Nagelkerke (Cragg and Uhler)        1                0.104
#Efron.r.squared                     0.799            0.024




________________________________________________________________________________


#Multivariate Model with Dam Numbers and Pond Area
mult.var.informed.vb.full <- glm(dams_per_km2+mean_pond_area_m2perkm2~
                          #GEOMORPHOLOGY
                          GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+#GEP_mean+#Area_km2_VB+AZIMTH_DEG_mean+
                          #HYDROLOGY
                          StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+#LAKE_max+
                          #WILDFIRE
                          Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+#Fire_Event_Area_Percent_of_uRCA+majority_burn_severity+
                          #VEGETATION
                          #From MaCander and Nelson
                          #ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
                          #From Nawrocki et al.
                          percent_woody_cover+dectre_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+dryas_PCT_MEAN+erivag_PCT_MEAN+sphagn_PCT_MEAN+#+picgla_PCT_MEAN+picmar_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+rhoshr_PCT_MEAN+vacvit_PCT_MEAN+empnig_PCT_MEAN+
                          #dom_veg_class+
                          #decidous_conifer_ratio+ I AM NOT SURE WHY THIS MESSES THE MODEL UP
                          #SOLAR
                          SOLAR_MEAN+                   
                          #PERMAFROST
                          ALD_2015_MEAN
                        #INTERACTIVE EFFECTS???
                        , data = VB_Y_sums_full)
summary(mult.var.informed.vb.full)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(mult.var.informed.vb.full)
accuracy(list(mult.var.informed.vb.full))
#Pseudo.R.squared
#McFadden                            0.766
#Cox and Snell (ML)                  1
#Nagelkerke (Cragg and Uhler)        1
#Efron.r.squared                     0.805 



________________________________________________________________________________



#Informed model without fire parameters
informed.vb.full.nofire <- glm(dams_per_km2~
                          #GEOMORPHOLOGY
                          GRADIENT_mean+MAX_GRAD_D_max+FP_WIDTH_mean+VAL_WIDTH_mean+VWI_Floor_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+ValCnstrnt_mean+#GEP_mean+#Area_km2_VB+AZIMTH_DEG_mean+
                          #HYDROLOGY
                          StrmPow_mean+FlowVel_mean+MEANANNCMS_mean+BFQ_mean+#LAKE_max+
                          #WILDFIRE
                          #Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_sever_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+#Fire_Event_Area_Percent_of_uRCA+majority_burn_severity+
                          #VEGETATION
                          #From MaCander and Nelson
                          #ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN+
                          #From Nawrocki et al.
                          percent_woody_cover+dectre_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+dryas_PCT_MEAN+erivag_PCT_MEAN+sphagn_PCT_MEAN+#+picgla_PCT_MEAN+picmar_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+rhoshr_PCT_MEAN+vacvit_PCT_MEAN+empnig_PCT_MEAN+
                          #dom_veg_class+
                          #decidous_conifer_ratio+ I AM NOT SURE WHY THIS MESSES THE MODEL UP
                          #SOLAR
                          SOLAR_MEAN+                   
                          #PERMAFROST
                          ALD_2015_MEAN
                        #INTERACTIVE EFFECTS???
                        , data = VB_Y_sums_full)
summary(informed.vb.full.nofire)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(informed.vb.full.nofire)
accuracy(list(informed.vb.full.nofire))
#Pseudo.R.squared
#McFadden                            0.072
#Cox and Snell (ML)                  0.433
#Nagelkerke (Cragg and Uhler)        0.433
#Efron.r.squared                     0.412 



# Model Averaging ---------------------------------------------------------

?AICcmodavg::modavg()

Cand.mod <- list(global.vb.full, vb.netmap,  vb.veg, vb.solar, vb.permafrost, vb.netmap.fire, informed.vb.full) #vb.fire,

Modnames <- c("global.vb.full", "vb.netmap",  "vb.veg", "vb.solar", "vb.permafrost", "vb.netmap.fire", "informed.vb.full") #"vb.fire",

aictab(Cand.mod, Modnames)

AICcmodavg::modavg(parm = c("Time_Since_Burn_min"), cand.set = Cand.mod, modnames = Modnames)







anova(informed.vb.full, informed.vb.full.nofire)



library(MASS)
library(car)
?Anova
# Consider other modeling options?  ---------------------------------------

#GLM? (Generalized Linear Model) 
#GLMM? (Generalized Linear Mixed Model)
#LMEM? (Linear Mixed Effect Model)
#GAMM?
install.packages("brms")
library(brms)
library(lme4)
library(Jags)



# Random and Fixed Effects ------------------------------------------------
(1|uRCA)
(1|Area_km2_VB)

# Transformations ---------------------------------------------------------


  
# Add Interactions? -------------------------------------------------------
# AICc --------------------------------------------------------------------




# Leave One Out Cross Validation ------------------------------------------





# Predict a Beaver Habitat Index  across the Study Area ------------------

#prediction_blank_VB
#informed.vb.full
str(prediction_blank_VB, list.len=ncol(prediction_blank_VB))


#Add Time Since Burn
summary(prediction_blank_VB$Fire_Year)

prediction_blank_VB$Fire_Event_Ignition_Date <- as.character(prediction_blank_VB$Fire_Event_Ignition_Date)
prediction_blank_VB$Fire_Year <- substr(prediction_blank_VB$Fire_Event_Ignition_Date, 1, 4)
prediction_blank_VB$Fire_Year <- as.numeric(prediction_blank_VB$Fire_Year)



prediction_blank_VB <- prediction_blank_VB %>% 
  mutate("Time_Since_Burn_min" =  2017-Fire_Year) %>%
  mutate("Burned_basin" = ifelse(Fire_Event_Area_Percent_of_uRCA >= 10, 1, 0)) %>% 
  mutate("Burned_basin" = ifelse(is.na(Fire_Event_Area_Percent_of_uRCA), 0, 1)) %>%
  mutate("percent_sever_burn" = ((BS4_AreaKm2/Area_km2_VB)*100)) %>%     
  mutate("percent_moderate_burn" = ((BS3_AreaKm2/Area_km2_VB)*100)) %>%  
  mutate("percent_mild_burn" = ((BS2_AreaKm2/Area_km2_VB)*100))  %>%  
  mutate("percent_no_burn" = ((BS1_AreaKm2/Area_km2_VB)*100))  %>%     
  mutate("percent_woody_cover" = (sumrow = salshr_PCT_MEAN+alnus_PCT_MEAN+bettre_PCT_MEAN+betshr_PCT_MEAN+ picgla_PCT_MEAN+picmar_PCT_MEAN+vaculi_PCT_MEAN)) %>% 
  mutate("decidous_conifer_ratio" = ((salshr_PCT_MEAN + alnus_PCT_MEAN + bettre_PCT_MEAN + betshr_PCT_MEAN+ vaculi_PCT_MEAN)/(picgla_PCT_MEAN + picmar_PCT_MEAN)))


#Predict dam density across the whole study area

#How to deal with NAs?????????????????
colnames(prediction_blank_VB)[apply(prediction_blank_VB, 2, anyNA)]

### na.pass vs
beaver_density_pred_VB1 <- cbind(informed.vb.full,
                    "dams_per_km2_pred"=predict(informed.vb.full, newdata = prediction_blank_VB, type="response", na.action = na.pass))
beaver_density_pred_VB1
beaver_density_pred_VB1$dams_per_km2_pred
nrow(prediction_blank_VB)
nrow(beaver_density_pred_VB1)


write.csv(beaver_density_pred_VB1, file = "Output Data Files/beaver_density_pred_VB1.csv") 





### NO FIRE, because thats where most of the NA's come from
beaver_density_pred_VB2 <- cbind(prediction_blank_VB,
                    "dams_per_km2_pred"=predict(informed.vb.full.nofire, newdata =  prediction_blank_VB, type="response", na.action = na.pass))
                                 
                                 
beaver_density_pred_VB2
beaver_density_pred_VB2$dams_per_km2_pred
nrow(prediction_blank_VB)
nrow(beaver_density_pred_VB2)
                                 
                                 
write.csv(beaver_density_pred_VB2, file = "Output Data Files/beaver_density_pred_VB2.csv") 
                                 


# Plots -------------------------------------------------------------------
str(VB_Y_sums_full, list.len=ncol(all_RCA))
names(VB_Y_sums_full)




#Example
ggplot(beaver_density_pred_VB2, aes(x = SOLAR_MEAN, y = dams_per_km2_pred) )+
  geom_point()+
  geom_smooth(method = "lm", alpha = .15)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylim(120, -120)
#theme(text = element_text(size = 30)) 








#Filter out large sections of the river
nrow(all_VB_Y)
all_VB_Y <- all_VB_Y %>% 
  filter(STRM_ORDER_max <= 5)
nrow(all_VB_Y)


##Consider log correcting




all_VB_Y$Burned_basin <- as.factor(all_VB_Y$Burned_basin)

p1 <- ggplot(data = all_VB_Y, aes(x = VWI_Floor_mean, y = dams_per_km2, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm', se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  theme(legend.position="bottom")
p1 
  
p2 <- ggplot(data = all_VB_Y, aes(x = GRADIENT_mean, y = dams_per_km2, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm', se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  theme(legend.position="bottom")+
  scale_x_continuous(limits = c(0, 0.12))#, breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000))
p2

p3 <- ggplot(data = all_VB_Y, aes(x = STRM_ORDER_max, y = dams_per_km2, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm', se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  theme(legend.position="bottom")
p3

p4 <- ggplot(data = all_VB_Y, aes(x = MEANANNCMS_mean, y = dams_per_km2, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm', se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  theme(legend.position="bottom")
p4


p5 <- ggplot(data = all_VB_Y, aes(x = BeavHab_min, y = dams_per_km2, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm', se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  theme(legend.position="bottom")
p5

p6 <- ggplot(data = all_VB_Y, aes(x = SOLAR_MEAN, y = dams_per_km2, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm', se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  theme(legend.position="bottom")+
  scale_x_continuous(limits = c(530000, 650000))#, breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000))
p6


box_p1 <-ggplot(all_VB_Y, aes(x=Burned_basin,y=dams_per_km2, fill=Burned_basin))+
  geom_boxplot(color = "black")+
  geom_jitter()+
  theme_classic()+
  scale_fill_manual(values=c("red", "darkgreen"))+
  #scale_y_continuous(limits = c(0, 15000), breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000))+
  #labs(title = "B", x = "", y = y_lab2 )+
  theme(
    legend.position = "none")#,
  #  plot.title = element_text(family = "serif", size = 24),
  #  axis.title = element_text(family = "serif", face = "bold", size = 20),
  #  axis.text = element_text(family = "serif", size = 18, color="black"))+#color = "grey29"
  #annotate(geom="text", x=2.1, y=15000, label="p = 0.015*",
   #        color="black", size = 7, family = "serif")
#annotate(geom="text", x=2, y=50, label="italic('p') = 0.013",
#      color="black", size = 8, family = "serif", parse = TRUE)
#annotate(geom="text",label=paste0("italic('p')~'='",p),parse=T,x=4.5,y=25,size=8)
#annotate("text", x = 4.5, y = 2.2, size = 5,
#         label = "italic(p)~=~0.013",
#        parse = TRUE)
box_p1

box_p2 <- ggplot(all_VB_Y, aes(x=Burned_basin,y=mean_pond_area_m2perkm2, fill=Burned_basin))+
  geom_boxplot(color = "black")+
  geom_jitter()+
  theme_classic()+
  scale_fill_manual(values=c("red", "darkgreen"))+
  #scale_y_continuous(limits = c(0, 15000), breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000))+
  #labs(title = "B", x = "", y = y_lab2 )+
  theme(
    legend.position = "none")#,
#  plot.title = element_text(family = "serif", size = 24),
#  axis.title = element_text(family = "serif", face = "bold", size = 20),
#  axis.text = element_text(family = "serif", size = 18, color="black"))+#color = "grey29"
#annotate(geom="text", x=2.1, y=15000, label="p = 0.015*",
#        color="black", size = 7, family = "serif")
#annotate(geom="text", x=2, y=50, label="italic('p') = 0.013",
#      color="black", size = 8, family = "serif", parse = TRUE)
#annotate(geom="text",label=paste0("italic('p')~'='",p),parse=T,x=4.5,y=25,size=8)
#annotate("text", x = 4.5, y = 2.2, size = 5,
#         label = "italic(p)~=~0.013",
#        parse = TRUE)
box_p2


first_looks_5_stm_ord <- p1+p2+p3+p4+p5+p6

first_looks_boxplots_5_stm_ord <- box_p1+box_p2

ggsave(plot= first_looks_5_stm_ord,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Beaver_Project_2/Figures/first_looks_5_stm_ord.jpeg",
       dpi = 2000, 
       height = 8,
       width = 10,
       units = "in")

ggsave(plot= first_looks_boxplots_5_stm_ord,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Beaver_Project_2/Figures/first_looks_boxplots_5_stm_ord.jpeg",
       dpi = 2000, 
       height = 6,
       width = 8,
       units = "in")






##Prediction plots

str(beaver_density_pred_VB2, list.len=ncol(beaver_density_pred_VB2))

beaver_density_pred_VB2$Burned_basin <- as.factor(beaver_density_pred_VB2$Burned_basin)


pp1 <- ggplot(data = beaver_density_pred_VB2, aes(x = VWI_Floor_mean, y = dams_per_km2_pred, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm')+#, se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  ylim(120, -120)+
  theme(legend.position="bottom")
pp1



pp2 <- ggplot(data = beaver_density_pred_VB2, aes(x = GRADIENT_mean, y = dams_per_km2_pred, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm')+#, se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  #scale_y_continuous()
  ylim(120, -120)+
  theme(legend.position="bottom")
pp2




pp3 <- ggplot(data = beaver_density_pred_VB2, aes(x = STRM_ORDER_max, y = dams_per_km2_pred, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm')+#, se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  #scale_y_continuous()
  ylim(120, -120)+
  theme(legend.position="bottom")
pp3



pp4 <- ggplot(data = beaver_density_pred_VB2, aes(x = MEANANNCMS_mean, y = dams_per_km2_pred, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm')+#, se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  #scale_y_continuous()
  ylim(120, -120)+
  theme(legend.position="bottom")
pp4




pp5 <- ggplot(data = beaver_density_pred_VB2, aes(x = BeavHab_min, y = dams_per_km2_pred, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm')+#, se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  #scale_y_continuous()
  ylim(120, -120)+
  theme(legend.position="bottom")
pp5





pp6 <- ggplot(data = beaver_density_pred_VB2, aes(x = SOLAR_MEAN, y = dams_per_km2_pred, color=Burned_basin))+
  geom_point()+ #aes(color = Burned_numerical, group_by=Burned_numerical)
  geom_smooth(method='lm')+#, se = FALSE)+ #aes(color = Burned_numerical, group_by=Burned_numerical), 
  theme_cowplot()+
  scale_color_manual(values=c("darkgreen", "red"))+
  #scale_y_continuous()
  ylim(120, -120)+
  theme(legend.position="bottom")
pp6







pred_panel <- pp1+pp2+pp3+pp4+pp5+pp6
pred_panel


ggsave(plot= pred_panel,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Final Beaver-Fire R Scripts/Beaver_Project_2/Figures/model predictions.jpeg",
       dpi = 2000, 
       height = 6,
       width = 8,
       units = "in")




