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


#all_VB <- read.csv("Output Data Files/all_VB.csv")

all_VB_Y <- read.csv("Output Data Files/all_VB_Y.csv")

#all_RCA <- read.csv("Output Data Files/all_RCA.csv")

#all_RCA_Y <- read.csv("Output Data Files/all_RCA_Y.csv")

#VB_sums_full <- read.csv("Output Data Files/VB_sums_full.csv")

VB_Y_sums_full <- read.csv("Output Data Files/VB_Y_sums_full_edited.csv")
#Fix time since burn issue (burns after 2017)
VB_Y_sums_full <- VB_Y_sums_full %>% 
  mutate(Time_Since_Burn_min = ifelse(Time_Since_Burn_min < 0, NA, Time_Since_Burn_min),
         Fire_Event_ID = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_ID),
         Fire_Event_Area_in_uRCA_km2 = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_Area_in_uRCA_km2),
         Fire_Event_Area_Percent_of_uRCA = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_Area_Percent_of_uRCA),
         Fire_Event_Ignition_Date = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_Ignition_Date),
         BS1_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS1_AreaKm2),
         BS2_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS2_AreaKm2),
         BS3_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS3_AreaKm2),
         BS4_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS4_AreaKm2),
         DNBR_MEAN = ifelse(Time_Since_Burn_min < 0, NA, DNBR_MEAN),
         RDNBR_MEAN = ifelse(Time_Since_Burn_min < 0, NA, RDNBR_MEAN)) %>% 
  mutate(FP_WIDTH_mean = ifelse(FP_WIDTH_mean == -9999, NA, FP_WIDTH_mean))


#RCA_sums_full <- read.csv("Output Data Files/RCA_sums_full.csv")

#RCA_Y_sums_full <- read.csv("Output Data Files/RCA_Y_sums_full.csv")






# Summary statistics for manuscript ---------------------------------------

burned_VB <- VB_Y_sums_full %>% 
  filter(Burned_numerical == 1)

unburned_VB <- VB_Y_sums_full %>% 
  filter(Burned_numerical == 0)


#All basins
mean(VB_Y_sums_full$Pond_Area_m2_mean) #2803.2 m2
sd(VB_Y_sums_full$Pond_Area_m2_mean)   #4803.1 m2

#Burned
mean(burned_VB$Pond_Area_m2_mean) #2013.8 m2
sd(burned_VB$Pond_Area_m2_mean)   #2374.2 m2

#Unburned
mean(unburned_VB$Pond_Area_m2_mean) #3598.7 m2
sd(unburned_VB$Pond_Area_m2_mean)   #6327.5 m2

t.test(burned_VB$Pond_Area_m2_mean, unburned_VB$Pond_Area_m2_mean)
#Significant!! T = -2.2759, DF = 120.53, p = 0.02462





#All basins
mean(VB_Y_sums_full$dams_per_km2) #7.61
sd(VB_Y_sums_full$dams_per_km2)   #10.79 m2

#Burned
mean(burned_VB$dams_per_km2) #8.45 
sd(burned_VB$dams_per_km2)   #10.62 

#Unburned
mean(unburned_VB$dams_per_km2) #6.75 
sd(unburned_VB$dams_per_km2)   #10.95


t.test(burned_VB$dams_per_km2, unburned_VB$dams_per_km2)
#Not significant. T = 1.1039, DF = 192.27, p = 0.271



#Burned
mean(burned_VB$Area_km2_VB) #0.495 km2
sd(burned_VB$Area_km2_VB)   #0.376 km2

#Unburned
mean(unburned_VB$Area_km2_VB) #0.764 km2
sd(unburned_VB$Area_km2_VB)   #1.015 km2


t.test(burned_VB$Area_km2_VB, unburned_VB$Area_km2_VB)
#Not significant. T = -2.44, DF = 119.92, p = 0.0161


#Burned
mean(burned_VB$Time_Since_Burn_min, na.rm = T) #27 years
sd(burned_VB$Time_Since_Burn_min, na.rm = T)   #20 years




#Burned
mean(burned_VB$percent_woody_cover, na.rm = T) #65.5%
sd(burned_VB$percent_woody_cover, na.rm = T) #8.06%

mean(burned_VB$BroadleafTree_2015_MEAN, na.rm = T) #7.05%
sd(burned_VB$BroadleafTree_2015_MEAN, na.rm = T) #5.21%

mean(burned_VB$ConiferTree_2015_MEAN, na.rm = T) #16.0%
sd(burned_VB$ConiferTree_2015_MEAN, na.rm = T) #7.98%

mean(burned_VB$DeciduousShrub_2015_MEAN, na.rm = T) #28.4%
sd(burned_VB$DeciduousShrub_2015_MEAN, na.rm = T) #8.37%

mean(burned_VB$EvergreenShrub_2015_MEAN, na.rm = T) #6.7%
sd(burned_VB$EvergreenShrub_2015_MEAN, na.rm = T) #3.06%


#Unburned
mean(unburned_VB$percent_woody_cover, na.rm = T) #67.8%
sd(unburned_VB$percent_woody_cover, na.rm = T) #12.41%

mean(unburned_VB$BroadleafTree_2015_MEAN, na.rm = T) #6.6%
sd(unburned_VB$BroadleafTree_2015_MEAN, na.rm = T) #6.70%

mean(unburned_VB$ConiferTree_2015_MEAN, na.rm = T) #21.8%
sd(unburned_VB$ConiferTree_2015_MEAN, na.rm = T) #6.89%

mean(unburned_VB$DeciduousShrub_2015_MEAN, na.rm = T) #23.5%
sd(unburned_VB$DeciduousShrub_2015_MEAN, na.rm = T) #8.27%

mean(unburned_VB$EvergreenShrub_2015_MEAN, na.rm = T) #6.04%
sd(unburned_VB$EvergreenShrub_2015_MEAN, na.rm = T) #3.22%


t.test(burned_VB$percent_woody_cover, unburned_VB$percent_woody_cover)
#Not significant.

t.test(burned_VB$BroadleafTree_2015_MEAN, unburned_VB$BroadleafTree_2015_MEAN)
#Not significant. 

t.test(burned_VB$ConiferTree_2015_MEAN, unburned_VB$ConiferTree_2015_MEAN)
#Significant!! T = -5.3887, DF = 189.07, p < 0.0001

t.test(burned_VB$DeciduousShrub_2015_MEAN, unburned_VB$DeciduousShrub_2015_MEAN)
#Not significant. T = 4.1133, DF = 191.98, p < 0.0001

t.test(burned_VB$EvergreenShrub_2015_MEAN, unburned_VB$EvergreenShrub_2015_MEAN)
#Not significant. 



str(VB_Y_sums_full, list.len = ncol(VB_Y_sums_full))

ggplot(VB_Y_sums_full, aes(x = log(Time_Since_Burn_min), y = dams_per_km2))+
  geom_point()+
  theme_cowplot()

Fire_Event_Area_in_uRCA_km2    
ggplot(VB_Y_sums_full, aes(x = log(Fire_Event_Area_in_uRCA_km2/Area_km2_RCA), y = dams_per_km2))+
  geom_point()+
  theme_cowplot()

ggplot(VB_Y_sums_full, aes(x = log(percent_severe_burn), y = dams_per_km2))+
  geom_point()+
  theme_cowplot()



library(plotly)


fig1 <- plot_ly(VB_Y_sums_full, x = ~Time_Since_Burn_min, 
                y = ~dams_per_km2, 
                z = ~percent_severe_burn
                #color = ~Sex, colors = c('#BF382A', '#0C4B8E')
                #size = ~fork_length, #This adjusts the size by fork length, but how do I make them bigger overall???
) #%>% 
  #layout(list(xaxis = list(zeroline=TRUE, showline = TRUE, linewidth = 10),
  #            yaxis = list(zeroline=TRUE, showline = TRUE),
  #            zaxis = list(zeroline=TRUE, showline = TRUE))) %>% This doesn't seem to do anything...
  #sizes = fork_length) %>% 
  #add_markers()  %>% #fig, marker = list(size = 5)) 
  #layout(scene = list(xaxis = list(title = 'Posterior Dorsal Height'),
                      yaxis = list(title = 'Pelvic Fin Length'),
                      zaxis = list(title = 'Pectoral Fin Height'))) 

fig1




print(discrete(VB_Y_sums_full$Time_Since_Burn_min))

table(VB_Y_sums_full$Time_Since_Burn_min)


# Use the table() function to summarize the counts
summary_table <- table(VB_Y_sums_full$Time_Since_Burn_min)

# Calculate the percentage of the total
percentage_table <- prop.table(summary_table) * 100

# Combine the count and percentage tables
result <- cbind(Count = summary_table, Percentage = percentage_table)

# Print the combined table
print(result)
#TSLB Count Percentage
#0      1   1.086957
#4      9   9.782609
#13    41  44.565217 From the 2004 fire
#14     2   2.173913 57.59% happened since 2000
#31     2   2.173913
#34    11  11.956522
#46     1   1.086957
#48     5   5.434783
#49     1   1.086957
#51     2   2.173913
#59    16  17.391304
#67     1   1.086957



mean(VB_Y_sums_full$percent_severe_burn, na.rm = T) #3.75
sd(VB_Y_sums_full$percent_severe_burn, na.rm = T) #5.34
max(VB_Y_sums_full$percent_severe_burn, na.rm = T) #34.67


mean(VB_Y_sums_full$percent_moderate_burn, na.rm = T) #15.84
sd(VB_Y_sums_full$percent_moderate_burn, na.rm = T) #13.82
max(VB_Y_sums_full$percent_moderate_burn, na.rm = T) #50.61



VB_Y_sums_full$dectre_PCT_MEAN


summary(lm(dectre_PCT_MEAN ~ ELEV_M_mean, data = VB_Y_sums_full))

ggplot(VB_Y_sums_full, aes(log(dectre_PCT_MEAN), log(ELEV_M_mean)))+
  geom_point()



mismatch <- VB_Y_sums_full %>%
  filter(Time_Since_Burn_min %in% c(0, 1, 2))
mismatch

sum(mismatch$Fire_Event_Area_Percent_of_uRCA, na.rm = T)
nrow(mismatch)



min(VB_Y_sums_full$MEANANNCMS_mean)
max(VB_Y_sums_full$MEANANNCMS_mean)




mean(VB_Y_sums_full$GRADIENT_mean) #1.6%
sd(VB_Y_sums_full$GRADIENT_mean) #1.6%




# Discuss model predictors ------------------------------------------------


#dams_per_km2 = response variable

#Geomorphology
#GRADIENT_mean
#FP_WIDTH_mean
#Area_km2_VB
#SINUOSITY_mean
#STRM_ORDER_max
#WIDTH_M_mean
#DEPTH_M_mean
#ELEV_M_mean

#HYDROLOGY
#StrmPow_mean
#MEANANNCMS_mean

#WILDFIRE
#Time_Since_Burn_min
#Fire_Event_Area_Percent_of_uRCA
#percent_severe_burn
#percent_moderate_burn
#percent_mild_burn
#percent_no_burn      hmmmm not sure if I really need to include this....

#VEGETATION
#From MaCander and Nelson
#ConiferTree_2015_MEAN
#BroadleafTree_2015_MEAN
#DeciduousShrub_2015_MEAN
#EvergreenShrub_2015_MEAN
#Forb_2015_MEAN
#Graminoid_2015_MEAN
#tmLichenLight_2015_MEAN



#There's more predictors in this dataframe, but these are the 23 that I have whittled down to be the most important.


#which columns have NA's? These will cause issues in the models 
colnames(VB_Y_sums_full)[apply(VB_Y_sums_full, 2, function(x) any(is.na(x)))]

#It looks like only the wildfire parameters have NA values, which we can work on 
any(is.na(VB_Y_sums_full$Time_Since_Burn_min))
any(is.na(VB_Y_sums_full$Fire_Event_Area_Percent_of_uRCA))
any(is.na(VB_Y_sums_full$percent_severe_burn))
any(is.na(VB_Y_sums_full$percent_moderate_burn))
any(is.na(VB_Y_sums_full$percent_mild_burn))
any(is.na(VB_Y_sums_full$percent_no_burn))


VB_Y_sums_full <- VB_Y_sums_full %>% 
  mutate("Time_Since_Burn_min" = 
           ifelse(is.na(Time_Since_Burn_min), 100, Time_Since_Burn_min)) %>% 
  mutate("Fire_Event_Area_Percent_of_uRCA" = 
           ifelse(is.na(Fire_Event_Area_Percent_of_uRCA), 0, Fire_Event_Area_Percent_of_uRCA)) %>% 
  mutate("percent_severe_burn" = ifelse(is.na(percent_severe_burn), 0, percent_severe_burn)) %>% 
  mutate("percent_moderate_burn" = ifelse(is.na(percent_moderate_burn), 0, percent_moderate_burn)) %>% 
  mutate("percent_mild_burn" = ifelse(is.na(percent_mild_burn), 0, percent_mild_burn)) %>% 
  mutate("percent_no_burn" = 100-(percent_severe_burn+percent_moderate_burn+percent_mild_burn))

#Hmmmm when I do this it includes all the variables (n=195) instead of just the burned drainages (n=55), 
#it tanks the R2 in the models (from like 0.90 to 0.34)




# Regression Models -------------------------------------------------------


#Global model (23 predictors, n = 195)
global.vb.reduced <- glm(dams_per_km2~ 
                                #GEOMORPHOLOGY
                                GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                                #HYDROLOGY
                                StrmPow_mean+MEANANNCMS_mean+
                                #WILDFIRE
                                Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+
                                #VEGETATION
                                #From MaCander and Nelson
                                percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                                #From Nawrocki et al.
                                #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
                                
                                , data = VB_Y_sums_full)

summary(global.vb.reduced)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(global.vb.reduced)
accuracy(list(global.vb.reduced))


#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.845      
#Cox and Snell (ML)                  1          
#Nagelkerke (Cragg and Uhler)        1          
#Efron.r.squared                     0.814



#_______________________________________________________________________________


#NetMap model 
netmap.vb <- glm(dams_per_km2~ 
                                #GEOMORPHOLOGY
                                GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                                #HYDROLOGY
                                StrmPow_mean+MEANANNCMS_mean
                      
                                , data = VB_Y_sums_full)

summary(netmap.vb)

nagelkerke(netmap.vb)
accuracy(list(netmap.vb))
#Pseudo.R.squared                               
#McFadden                            0.088     
#Cox and Snell (ML)                  0.502         
#Nagelkerke (Cragg and Uhler)        0.502           
#Efron.r.squared                     0.134

#_______________________________________________________________________________


#Fire model 
fire.vb <- glm(dams_per_km2~
                                #WILDFIRE
                                Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                    
                                , data = VB_Y_sums_full)

summary(fire.vb)

nagelkerke(fire.vb)
accuracy(list(fire.vb))
#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.79      
#Cox and Snell (ML)                  1          
#Nagelkerke (Cragg and Uhler)        1          
#Efron.r.squared                     0.213  

#Including all valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.00498      
#Cox and Snell (ML)                  0.0371        
#Nagelkerke (Cragg and Uhler)        0.0371        
#Efron.r.squared                     0.0371

#_______________________________________________________________________________


#Vegetation model 
veg.vb <- glm(dams_per_km2~ 
                                #VEGETATION
                                #From MaCander and Nelson
                                percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                                #From Nawrocki et al.
                                #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
                                
                                , data = VB_Y_sums_full)

summary(veg.vb)

nagelkerke(veg.vb)
accuracy(list(veg.vb))
#Pseudo.R.squared                               
#McFadden                            0.038      
#Cox and Snell (ML)                  0.251         
#Nagelkerke (Cragg and Uhler)        0.251           
#Efron.r.squared                     0.223  

#_______________________________________________________________________________



#Model selected version
#options(na.action = "na.fail")
#dredge.table <- as.data.frame(MuMIn::dredge(global.vb.reduced, trace = 2, evaluate = TRUE, extra = c("R^2", "adjR^2"), rank = "AICc"))
#dredge.table
#options(na.action = "na.omit")

#Dredge preferred model with the unburned catchments excluded

#Model selection table 
#           (Int) Are_km2_VB DEP_M_men ELE_M_men FP_WID_men  GRA_men  MEANA_men SIN_men STR_ORD_max StP_men
#131586  -13.14000  -4.319                      -0.001688                        21.680                        
#514       9.74100  -4.188                      -0.001629                                                     
#4194818   9.07600  -4.197                      -0.001687                                                     
#4610      9.46600  -4.200                      -0.001673            0.17340000                                
#655874  -11.32000  -4.418                      -0.001698                        20.360             -0.00330000
#135682  -10.19000  -4.308                      -0.001704            0.09434000  18.740                        
#524802   10.35000  -4.339                      -0.001648                                           -0.00467600
#1538     10.40000  -4.311                      -0.001649   -35.6100                                            
#4325890 -10.12000  -4.304                      -0.001707                        18.520                        
#393730  -14.94000  -4.336                      -0.001679                        24.280   -0.27930            

#         WID_M_men     R^2     adjR^2    df  logLik  AICc   delta weight
#131586             0.1975000 0.19760000  5 -718.608 1447.5  0.00  0.041
#514                0.1870000 0.18710000  4 -719.873 1448.0  0.42  0.034
#4194818  0.2095000 0.1923000 0.19240000  5 -719.243 1448.8  1.27  0.022
#4610               0.1920000 0.19210000  5 -719.277 1448.9  1.34  0.021
#655874             0.1989000 0.19900000  6 -718.437 1449.3  1.79  0.017
#135682             0.1988000 0.19890000  6 -718.453 1449.4  1.82  0.017
#524802             0.1899000 0.19000000  5 -719.525 1449.4  1.83  0.017
#1538               0.1898000 0.18990000  5 -719.539 1449.4  1.86  0.016
#4325890  0.0991200 0.1985000 0.19860000  6 -718.493 1449.4  1.90  0.016
#393730             0.1982000 0.19830000  6 -718.518 1449.5  1.95  0.016

dredge.vb <- glm(dams_per_km2~ 
                   #GEOMORPHOLOGY
                   GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+MEANANNCMS_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+
                   #HYDROLOGY
                   StrmPow_mean
                   
                 , data = VB_Y_sums_full)

summary(dredge.vb)

nagelkerke(dredge.vb)
accuracy(list(dredge.vb))
#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.087      
#Cox and Snell (ML)                  0.497          
#Nagelkerke (Cragg and Uhler)        0.497         
#Efron.r.squared                     0.125




###Dredge preferred model with the ALL catchments INCLUDED




#Combine the various models


netmap.fire.vb <- glm(dams_per_km2~ 
                           #GEOMORPHOLOGY
                           GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                           #HYDROLOGY
                           StrmPow_mean+MEANANNCMS_mean+
                           #WILDFIRE
                           Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                          
                         , data = VB_Y_sums_full)

summary(netmap.fire.vb)

nagelkerke(netmap.fire.vb)
accuracy(list(netmap.fire.vb))

#Pseudo.R.squared                               
#McFadden                            0.827      
#Cox and Snell (ML)                  1.00     
#Nagelkerke (Cragg and Uhler)        1.00      
#Efron.r.squared                     0.693  






netmap.veg.vb <- glm(dams_per_km2~ 
                       #GEOMORPHOLOGY
                       GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                       #HYDROLOGY
                       StrmPow_mean+MEANANNCMS_mean+
                       
                       #VEGETATION
                       #From MaCander and Nelson
                       percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                     
                     
                     , data = VB_Y_sums_full)

summary(netmap.veg.vb)

nagelkerke(netmap.veg.vb)
accuracy(list(netmap.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.112      
#Cox and Snell (ML)                  0.587      
#Nagelkerke (Cragg and Uhler)        0.587      
#Efron.r.squared                     0.283 







veg.fire.vb <- glm(dams_per_km2~ 
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+
                        #VEGETATION
                        #From MaCander and Nelson
                        percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                      
                      
                      , data = VB_Y_sums_full)

summary(veg.fire.vb)

nagelkerke(veg.fire.vb)
accuracy(list(veg.fire.vb))

#Pseudo.R.squared                               
#McFadden                            0.811      
#Cox and Snell (ML)                  1.0     
#Nagelkerke (Cragg and Uhler)        1.0      
#Efron.r.squared                     0.52





geo.fire.vb <- glm(dams_per_km2~ 
                        #GEOMORPHOLOGY
                        GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                        
                      , data = VB_Y_sums_full)

summary(geo.fire.vb)

nagelkerke(geo.fire.vb)
accuracy(list(geo.fire.vb))

#Pseudo.R.squared                               
#McFadden                            0.819      
#Cox and Snell (ML)                  1.0     
#Nagelkerke (Cragg and Uhler)        1.0      
#Efron.r.squared                     0.612  








hydro.fire.vb <- glm(dams_per_km2~ 
                        #HYDROLOGY
                        StrmPow_mean+MEANANNCMS_mean+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                       
                      , data = VB_Y_sums_full)

summary(hydro.fire.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(hydro.fire.vb)
accuracy(list(hydro.fire.vb))


#Pseudo.R.squared                               
#McFadden                            0.796      
#Cox and Snell (ML)                  1.0      
#Nagelkerke (Cragg and Uhler)        1.0    
#Efron.r.squared                     0.261










geo.veg.vb <- glm(dams_per_km2~ 
                        #GEOMORPHOLOGY
                        GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                        #VEGETATION
                        #From MaCander and Nelson
                        percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                      
                      
                      , data = VB_Y_sums_full)

summary(geo.veg.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(geo.veg.vb)
accuracy(list(geo.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.111      
#Cox and Snell (ML)                  0.585      
#Nagelkerke (Cragg and Uhler)        0.585      
#Efron.r.squared                     0.279







hydro.veg.vb <- glm(dams_per_km2~ 
                        #HYDROLOGY
                        StrmPow_mean+MEANANNCMS_mean+
                        #VEGETATION
                        #From MaCander and Nelson
                        percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                        
                      , data = VB_Y_sums_full)

summary(hydro.veg.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(hydro.veg.vb)
accuracy(list(hydro.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.039      
#Cox and Snell (ML)                  0.258      
#Nagelkerke (Cragg and Uhler)        0.258  
#Efron.r.squared                     0.231






















# Model Averaging ---------------------------------------------------------



Cand.mod <- list(global.vb.reduced, netmap.vb, fire.vb, veg.vb, dredge.vb, netmap.fire.vb, veg.fire.vb, netmap.veg.vb, geo.fire.vb, hydro.fire.vb, geo.veg.vb, hydro.veg.vb
) 

Modnames <- c("global.vb.reduced", "netmap.vb",  "fire.vb", "veg.vb", "dredge.vb", "netmap.fire.vb", "veg.fire.vb", "netmap.veg.vb", "geo.fire.vb", "hydro.fire.vb", "geo.veg.vb", "hydro.veg.vb") 

aictab(Cand.mod, Modnames)


AICcmodavg::modavg(parm = c("GRADIENT_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -58.95
#Unconditional SE: 99.28
#90% Unconditional confidence interval: -222.25, 104.35

AICcmodavg::modavg(parm = c("FP_WIDTH_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -0.02
#Unconditional SE:  0.02
#90% Unconditional confidence interval: -0.05, 0

AICcmodavg::modavg(parm = c("Area_km2_VB"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -8.09
#Unconditional SE:  2.29
#90% Unconditional confidence interval: -11.87, -4.32

AICcmodavg::modavg(parm = c("SINUOSITY_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -49.07
#Unconditional SE:  21.91
#90% Unconditional confidence interval: -85.11, -13.03

AICcmodavg::modavg(parm = c("STRM_ORDER_max"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 0.11
#Unconditional SE:  1.06
#90% Unconditional confidence interval: -1.64, 1.85

AICcmodavg::modavg(parm = c("WIDTH_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 5.59
#Unconditional SE:  3.36
#90% Unconditional confidence interval: 0.06, 11.11

AICcmodavg::modavg(parm = c("DEPTH_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -101.18
#Unconditional SE:  68.36
#90% Unconditional confidence interval: -213.62, 11.26

AICcmodavg::modavg(parm = c("ELEV_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 0
#Unconditional SE:  0.01
#90% Unconditional confidence interval: -0.02, 0.01

AICcmodavg::modavg(parm = c("StrmPow_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 0.01
#Unconditional SE:  0.01
#90% Unconditional confidence interval: 0, 0.03

AICcmodavg::modavg(parm = c("MEANANNCMS_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -4.71
#Unconditional SE:  2.26
#90% Unconditional confidence interval: -8.43, -0.98

AICcmodavg::modavg(parm = c("Time_Since_Burn_min"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 0.33
#Unconditional SE:  0.23
#90% Unconditional confidence interval: -0.05, 0.7

AICcmodavg::modavg(parm = c("Fire_Event_Area_Percent_of_uRCA"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -0.03
#Unconditional SE:  0.03
#90% Unconditional confidence interval: -0.09, 0.02

AICcmodavg::modavg(parm = c("percent_severe_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 0.38
#Unconditional SE:  0.11
#90% Unconditional confidence interval: 0.2, 0.57

AICcmodavg::modavg(parm = c("percent_moderate_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -0.18
#Unconditional SE:  0.06
#90% Unconditional confidence interval: -0.27, -0.08

AICcmodavg::modavg(parm = c("percent_mild_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 0.04
#Unconditional SE:  0.05
#90% Unconditional confidence interval: -0.04, 0.12

AICcmodavg::modavg(parm = c("percent_no_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -0.02
#Unconditional SE:  0.02
#90% Unconditional confidence interval: -0.05, 0.02


#NOT INCLUDED IN THE TOP RANKED MODELS

AICcmodavg::modavg(parm = c("percent_woody_cover"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval: 


AICcmodavg::modavg(parm = c("ConiferTree_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("BroadleafTree_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("DeciduousShrub_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("EvergreenShrub_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("Forb_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("Graminoid_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval:

AICcmodavg::modavg(parm = c("tmLichenLight_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate:
#Unconditional SE:  
#90% Unconditional confidence interval: 









#Trying to do this ^^ with a for loop, couldn't get it to work



attach(VB_Y_sums_full)
detach(VB_Y_sums_full)

parm <- list("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "SINUOSITY_mean", "STRM_ORDER_max", "WIDTH_M_mean", "DEPTH_M_mean", "ELEV_M_mean", "StrmPow_mean", "MEANANNCMS_mean", "Time_Since_Burn_min", "Fire_Event_Area_Percent_of_uRCA", "percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN", "DeciduousShrub_2015_MEAN", "EvergreenShrub_2015_MEAN", "Forb_2015_MEAN", "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN")
parm               


VB_Y_sums_full_select <- VB_Y_sums_full %>% 
  select("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "SINUOSITY_mean", "STRM_ORDER_max", "WIDTH_M_mean", "DEPTH_M_mean", "ELEV_M_mean", "StrmPow_mean", "MEANANNCMS_mean", "Time_Since_Burn_min", "Fire_Event_Area_Percent_of_uRCA", "percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN", "DeciduousShrub_2015_MEAN", "EvergreenShrub_2015_MEAN", "Forb_2015_MEAN", "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN")



result_table <- data.frame()


col_names <- c("mod_avg_est", "uncon_SE")
result_table <- data.frame(matrix(ncol = length(colnames), nrow = 0))


for (parm in VB_Y_sums_full_select) {
  
  AICcmodavg::modavg(parm = parm, cand.set = Cand.mod, modnames = Modnames)
  
  # Store the results in the result table
  result_table[i, "Column"] <- column
  result_table[i, "AICc"] <- aicc
}














# Testing using ALL RCAs, not just the ones with beaver ponds -------------



# Repeat the above model averaging, but separate burned/unburned RCAs --------

#BURNED RCAS

VB_Y_sums_full <- read.csv("Output Data Files/VB_Y_sums_full_edited.csv")
#Fix time since burn issue (burns after 2017)
VB_Y_sums_full <- VB_Y_sums_full %>% 
  mutate(Time_Since_Burn_min = ifelse(Time_Since_Burn_min < 0, NA, Time_Since_Burn_min),
         Fire_Event_ID = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_ID),
         Fire_Event_Area_in_uRCA_km2 = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_Area_in_uRCA_km2),
         Fire_Event_Area_Percent_of_uRCA = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_Area_Percent_of_uRCA),
         Fire_Event_Ignition_Date = ifelse(Time_Since_Burn_min < 0, NA, Fire_Event_Ignition_Date),
         BS1_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS1_AreaKm2),
         BS2_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS2_AreaKm2),
         BS3_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS3_AreaKm2),
         BS4_AreaKm2 = ifelse(Time_Since_Burn_min < 0, NA, BS4_AreaKm2),
         DNBR_MEAN = ifelse(Time_Since_Burn_min < 0, NA, DNBR_MEAN),
         RDNBR_MEAN = ifelse(Time_Since_Burn_min < 0, NA, RDNBR_MEAN)) %>% 
  mutate(FP_WIDTH_mean = ifelse(FP_WIDTH_mean == -9999, NA, FP_WIDTH_mean))




str(VB_Y_sums_full, n.col = VB_Y_sums_full)

burned_VB <- VB_Y_sums_full %>% 
  filter((complete.cases(Time_Since_Burn_min >= 0)))

unburned_VB <- VB_Y_sums_full %>% 
  filter(is.na(Time_Since_Burn_min))

# Regression Models -------------------------------------------------------


#Global model (23 predictors, n = 195)
global.vb.reduced <- glm(dams_per_km2~ 
                           #GEOMORPHOLOGY
                           GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                           #HYDROLOGY
                           StrmPow_mean+MEANANNCMS_mean+
                           #WILDFIRE
                           Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+
                           #VEGETATION
                           #From MaCander and Nelson
                           percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                         #From Nawrocki et al.
                         #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
                         
                         , data = burned_VB)

summary(global.vb.reduced)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(global.vb.reduced)
accuracy(list(global.vb.reduced))


#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.669      
#Cox and Snell (ML)                  0.999          
#Nagelkerke (Cragg and Uhler)        0.999      
#Efron.r.squared                     0.814


#_______________________________________________________________________________


#NetMap model 
netmap.vb <- glm(dams_per_km2~ 
                   #GEOMORPHOLOGY
                   GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                   #HYDROLOGY
                   StrmPow_mean+MEANANNCMS_mean
                 
                 , data = burned_VB)

summary(netmap.vb)

nagelkerke(netmap.vb)
accuracy(list(netmap.vb))
#Pseudo.R.squared                               
#McFadden                            0.212     
#Cox and Snell (ML)                  0.823         
#Nagelkerke (Cragg and Uhler)        0.823          
#Efron.r.squared                     0.375

#_______________________________________________________________________________


#Fire model 
fire.vb <- glm(dams_per_km2~
                 #WILDFIRE
                 Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
               
               , data = burned_VB)

summary(fire.vb)

nagelkerke(fire.vb)
accuracy(list(fire.vb))
#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.56      
#Cox and Snell (ML)                  0.999          
#Nagelkerke (Cragg and Uhler)        0.999          
#Efron.r.squared                     0.214


#_______________________________________________________________________________


#Vegetation model 
veg.vb <- glm(dams_per_km2~ 
                #VEGETATION
                #From MaCander and Nelson
                percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
              #From Nawrocki et al.
              #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
              
              , data = burned_VB)

summary(veg.vb)

nagelkerke(veg.vb)
accuracy(list(veg.vb))
#Pseudo.R.squared                               
#McFadden                            0.087      
#Cox and Snell (ML)                  0.485         
#Nagelkerke (Cragg and Uhler)        0.485           
#Efron.r.squared                     0.445  

#_______________________________________________________________________________



#Model selected version
#options(na.action = "na.fail")
#dredge.table <- as.data.frame(MuMIn::dredge(global.vb.reduced, trace = 2, evaluate = TRUE, extra = c("R^2", "adjR^2"), rank = "AICc"))
#dredge.table
#options(na.action = "na.omit")

#Dredge preferred model with the unburned catchments excluded

#Model selection table 
#           (Int) Are_km2_VB DEP_M_men ELE_M_men FP_WID_men  GRA_men  MEANA_men SIN_men STR_ORD_max StP_men
#131586  -13.14000  -4.319                      -0.001688                        21.680                        
#514       9.74100  -4.188                      -0.001629                                                     
#4194818   9.07600  -4.197                      -0.001687                                                     
#4610      9.46600  -4.200                      -0.001673            0.17340000                                
#655874  -11.32000  -4.418                      -0.001698                        20.360             -0.00330000
#135682  -10.19000  -4.308                      -0.001704            0.09434000  18.740                        
#524802   10.35000  -4.339                      -0.001648                                           -0.00467600
#1538     10.40000  -4.311                      -0.001649   -35.6100                                            
#4325890 -10.12000  -4.304                      -0.001707                        18.520                        
#393730  -14.94000  -4.336                      -0.001679                        24.280   -0.27930            

#         WID_M_men     R^2     adjR^2    df  logLik  AICc   delta weight
#131586             0.1975000 0.19760000  5 -718.608 1447.5  0.00  0.041
#514                0.1870000 0.18710000  4 -719.873 1448.0  0.42  0.034
#4194818  0.2095000 0.1923000 0.19240000  5 -719.243 1448.8  1.27  0.022
#4610               0.1920000 0.19210000  5 -719.277 1448.9  1.34  0.021
#655874             0.1989000 0.19900000  6 -718.437 1449.3  1.79  0.017
#135682             0.1988000 0.19890000  6 -718.453 1449.4  1.82  0.017
#524802             0.1899000 0.19000000  5 -719.525 1449.4  1.83  0.017
#1538               0.1898000 0.18990000  5 -719.539 1449.4  1.86  0.016
#4325890  0.0991200 0.1985000 0.19860000  6 -718.493 1449.4  1.90  0.016
#393730             0.1982000 0.19830000  6 -718.518 1449.5  1.95  0.016

dredge.vb <- glm(dams_per_km2~ 
                   #GEOMORPHOLOGY
                   GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+MEANANNCMS_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+
                   #HYDROLOGY
                   StrmPow_mean
                 
                 , data = burned_VB)

summary(dredge.vb)

nagelkerke(dredge.vb)
accuracy(list(dredge.vb))
#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.203     
#Cox and Snell (ML)                  0.810          
#Nagelkerke (Cragg and Uhler)        0.810          
#Efron.r.squared                     0.33


#Combine the various models


netmap.fire.vb <- glm(dams_per_km2~ 
                        #GEOMORPHOLOGY
                        GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                        #HYDROLOGY
                        StrmPow_mean+MEANANNCMS_mean+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                      
                      , data = burned_VB)

summary(netmap.fire.vb)

nagelkerke(netmap.fire.vb)
accuracy(list(netmap.fire.vb))

#Pseudo.R.squared                               
#McFadden                            0.632      
#Cox and Snell (ML)                  0.999      
#Nagelkerke (Cragg and Uhler)        0.999      
#Efron.r.squared                     0.693 




netmap.veg.vb <- glm(dams_per_km2~ 
                       #GEOMORPHOLOGY
                       GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                       #HYDROLOGY
                       StrmPow_mean+MEANANNCMS_mean+
                       
                       #VEGETATION
                       #From MaCander and Nelson
                       percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                     
                     
                     , data = burned_VB)

summary(netmap.veg.vb)

nagelkerke(netmap.veg.vb)
accuracy(list(netmap.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.260    
#Cox and Snell (ML)                  0.881      
#Nagelkerke (Cragg and Uhler)        0.881      
#Efron.r.squared                     0.578 



veg.fire.vb <- glm(dams_per_km2~ 
                     #WILDFIRE
                     Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn+
                     #VEGETATION
                     #From MaCander and Nelson
                     percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                   
                   
                   , data = burned_VB)

summary(veg.fire.vb)

nagelkerke(veg.fire.vb)
accuracy(list(veg.fire.vb))

#Pseudo.R.squared                               
#McFadden                            0.598      
#Cox and Snell (ML)                  0.999      
#Nagelkerke (Cragg and Uhler)        0.999     
#Efron.r.squared                     0.52  





geo.fire.vb <- glm(dams_per_km2~ 
                     #GEOMORPHOLOGY
                     GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                     #WILDFIRE
                     Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                   
                   , data = burned_VB)

summary(geo.fire.vb)

nagelkerke(geo.fire.vb)
accuracy(list(geo.fire.vb))

#Pseudo.R.squared                               
#McFadden                            0.614     
#Cox and Snell (ML)                  0.999      
#Nagelkerke (Cragg and Uhler)        0.999      
#Efron.r.squared                     0.612  








hydro.fire.vb <- glm(dams_per_km2~ 
                       #HYDROLOGY
                       StrmPow_mean+MEANANNCMS_mean+
                       #WILDFIRE
                       Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                     
                     , data = burned_VB)

summary(hydro.fire.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(hydro.fire.vb)
accuracy(list(hydro.fire.vb))


#Pseudo.R.squared                               
#McFadden                            0.566    
#Cox and Snell (ML)                  0.999  
#Nagelkerke (Cragg and Uhler)        0.999      
#Efron.r.squared                     0.261










geo.veg.vb <- glm(dams_per_km2~ 
                    #GEOMORPHOLOGY
                    GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                    #VEGETATION
                    #From MaCander and Nelson
                    percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                  
                  
                  , data = burned_VB)

summary(geo.veg.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(geo.veg.vb)
accuracy(list(geo.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.239      
#Cox and Snell (ML)                  0.859    
#Nagelkerke (Cragg and Uhler)        0.859      
#Efron.r.squared                     0.503







hydro.veg.vb <- glm(dams_per_km2~ 
                      #HYDROLOGY
                      StrmPow_mean+MEANANNCMS_mean+
                      #Vegetation
                      #From MaCander and Nelson
                      percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                    
                    , data = burned_VB)

summary(hydro.veg.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(hydro.veg.vb)
accuracy(list(hydro.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.089      
#Cox and Snell (ML)                  0.493      
#Nagelkerke (Cragg and Uhler)        0.493      
#Efron.r.squared                     0.453






Cand.mod <- list(global.vb.reduced, netmap.vb, fire.vb, veg.vb, dredge.vb, netmap.fire.vb, veg.fire.vb, netmap.veg.vb, geo.fire.vb, hydro.fire.vb, geo.veg.vb, hydro.veg.vb) 

Modnames <- c("global.vb.reduced", "netmap.vb",  "fire.vb", "veg.vb", "dredge.vb", "netmap.fire.vb", "veg.fire.vb", "netmap.veg.vb", "geo.fire.vb", "hydro.fire.vb", "geo.veg.vb", "hydro.veg.vb") 

aictab(Cand.mod, Modnames)

#Only include the models that take up > 5% of the AICc weight

Cand.mod <- list(netmap.fire.vb, geo.fire.vb) 

Modnames <- c("netmap.fire.vb", "geo.fire.vb") 

aictab(Cand.mod, Modnames)

AICcmodavg::modavg(parm = c("GRADIENT_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -58.95 
#Unconditional SE: 99.28
#90% Unconditional confidence interval: -222.25, 104.35

AICcmodavg::modavg(parm = c("FP_WIDTH_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.02
#Unconditional SE:  0.02
#90% Unconditional confidence interval: -0.05, 0.00

AICcmodavg::modavg(parm = c("Area_km2_VB"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -8.09
#Unconditional SE:  2.29
#90% Unconditional confidence interval: -11.87, -4.32

AICcmodavg::modavg(parm = c("SINUOSITY_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -49.07
#Unconditional SE:  21.91
#90% Unconditional confidence interval: -85.11, -13.03

AICcmodavg::modavg(parm = c("STRM_ORDER_max"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.11
#Unconditional SE:  1.06
#90% Unconditional confidence interval: -1.64, 1.85

AICcmodavg::modavg(parm = c("WIDTH_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 5.59
#Unconditional SE:  3.36
#90% Unconditional confidence interval: -0.06, 11.11

AICcmodavg::modavg(parm = c("DEPTH_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -101.18
#Unconditional SE:  68.36
#90% Unconditional confidence interval: -213.62, 11.26

AICcmodavg::modavg(parm = c("ELEV_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0
#Unconditional SE:  0.01
#90% Unconditional confidence interval: -0.02, 0.01

AICcmodavg::modavg(parm = c("StrmPow_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.01
#Unconditional SE:  0.01
#90% Unconditional confidence interval: 0, 0.03

AICcmodavg::modavg(parm = c("MEANANNCMS_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -4.71
#Unconditional SE:  2.26
#90% Unconditional confidence interval: -8.43, -0.99

AICcmodavg::modavg(parm = c("Time_Since_Burn_min"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.33
#Unconditional SE:  0.23
#90% Unconditional confidence interval: -0.04, 0.7

AICcmodavg::modavg(parm = c("Fire_Event_Area_Percent_of_uRCA"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.03
#Unconditional SE:  0.03
#90% Unconditional confidence interval: -0.09, 0.02

AICcmodavg::modavg(parm = c("percent_severe_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.38
#Unconditional SE:  0.11
#90% Unconditional confidence interval: 0.2, 0.57

AICcmodavg::modavg(parm = c("percent_moderate_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.18
#Unconditional SE:  0.06
#90% Unconditional confidence interval: -0.27, -0.08

AICcmodavg::modavg(parm = c("percent_mild_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.04
#Unconditional SE:  0.05
#90% Unconditional confidence interval: -0.04, 0.12

AICcmodavg::modavg(parm = c("percent_no_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.02
#Unconditional SE:  0.02
#90% Unconditional confidence interval: -0.05, 0.02


AICcmodavg::modavg(parm = c("percent_woody_cover"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: 
#Unconditional SE:  
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("ConiferTree_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.11
#Unconditional SE:  0.37
#90% Unconditional confidence interval: -0.61, 0.83

AICcmodavg::modavg(parm = c("BroadleafTree_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.23
#Unconditional SE:  0.52
#90% Unconditional confidence interval: -0.79, 1.25

AICcmodavg::modavg(parm = c("DeciduousShrub_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.18
#Unconditional SE:  0.2
#90% Unconditional confidence interval: -0.22, 0.57

AICcmodavg::modavg(parm = c("EvergreenShrub_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.24
#Unconditional SE:  0.91
#90% Unconditional confidence interval: -2.02, 1.55

AICcmodavg::modavg(parm = c("Forb_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 2.25
#Unconditional SE:  1.33
#90% Unconditional confidence interval: -0.37, 4.86

AICcmodavg::modavg(parm = c("Graminoid_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: -0.24
#Unconditional SE:  0.34
#90% Unconditional confidence interval: -0.43, 0.91

AICcmodavg::modavg(parm = c("tmLichenLight_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Burned
#Model-averaged estimate: 0.91
#Unconditional SE:  1.43
#90s% Unconditional confidence interval: -1.88, 3.71






# Repeat with the UNBURNED catchments -------------------------------------

#UNBURNED RCAS

#Global model (23 predictors, n = 195)
global.vb.reduced <- glm(dams_per_km2~ 
                           #GEOMORPHOLOGY
                           GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                           #HYDROLOGY
                           StrmPow_mean+MEANANNCMS_mean+
                           #WILDFIRE
                           #VEGETATION
                           #From MaCander and Nelson
                           percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                         #From Nawrocki et al.
                         #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
                         
                         , data = unburned_VB)

summary(global.vb.reduced)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(global.vb.reduced)
accuracy(list(global.vb.reduced))


#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.074      
#Cox and Snell (ML)                  0.436         
#Nagelkerke (Cragg and Uhler)        0.436    
#Efron.r.squared                     0.397


#_______________________________________________________________________________


#NetMap model 
netmap.vb <- glm(dams_per_km2~ 
                   #GEOMORPHOLOGY
                   GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                   #HYDROLOGY
                   StrmPow_mean+MEANANNCMS_mean
                 
                 , data = unburned_VB)

summary(netmap.vb)

nagelkerke(netmap.vb)
accuracy(list(netmap.vb))
#Pseudo.R.squared                               
#McFadden                            0.032      
#Cox and Snell (ML)                  0.218         
#Nagelkerke (Cragg and Uhler)        0.218          
#Efron.r.squared                     0.164

#_______________________________________________________________________________



#Vegetation model 
veg.vb <- glm(dams_per_km2~ 
                #VEGETATION
                #From MaCander and Nelson
                percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
              #From Nawrocki et al.
              #percent_woody_cover+dectre_PCT_MEAN+salshr_PCT_MEAN+vaculi_PCT_MEAN+alnus_PCT_MEAN+betshr_PCT_MEAN+bettre_PCT_MEAN+picgla_PCT_MEAN+picmar_PCT_MEAN+dryas_PCT_MEAN+empnig_PCT_MEAN+erivag_PCT_MEAN+rhoshr_PCT_MEAN+sphagn_PCT_MEAN+vacvit_PCT_MEAN
              
              , data = unburned_VB)

summary(veg.vb)

nagelkerke(veg.vb)
accuracy(list(veg.vb))
#Pseudo.R.squared                               
#McFadden                            0.038     
#Cox and Snell (ML)                  0.253        
#Nagelkerke (Cragg and Uhler)        0.253           
#Efron.r.squared                     0.253

#_______________________________________________________________________________



#Model selected version
#options(na.action = "na.fail")
#dredge.table <- as.data.frame(MuMIn::dredge(global.vb.reduced, trace = 2, evaluate = TRUE, extra = c("R^2", "adjR^2"), rank = "AICc"))
#dredge.table
#options(na.action = "na.omit")

#Dredge preferred model with the unburned catchments excluded

#Model selection table 
#           (Int) Are_km2_VB DEP_M_men ELE_M_men FP_WID_men  GRA_men  MEANA_men SIN_men STR_ORD_max StP_men
#131586  -13.14000  -4.319                      -0.001688                        21.680                        
#514       9.74100  -4.188                      -0.001629                                                     
#4194818   9.07600  -4.197                      -0.001687                                                     
#4610      9.46600  -4.200                      -0.001673            0.17340000                                
#655874  -11.32000  -4.418                      -0.001698                        20.360             -0.00330000
#135682  -10.19000  -4.308                      -0.001704            0.09434000  18.740                        
#524802   10.35000  -4.339                      -0.001648                                           -0.00467600
#1538     10.40000  -4.311                      -0.001649   -35.6100                                            
#4325890 -10.12000  -4.304                      -0.001707                        18.520                        
#393730  -14.94000  -4.336                      -0.001679                        24.280   -0.27930            

#         WID_M_men     R^2     adjR^2    df  logLik  AICc   delta weight
#131586             0.1975000 0.19760000  5 -718.608 1447.5  0.00  0.041
#514                0.1870000 0.18710000  4 -719.873 1448.0  0.42  0.034
#4194818  0.2095000 0.1923000 0.19240000  5 -719.243 1448.8  1.27  0.022
#4610               0.1920000 0.19210000  5 -719.277 1448.9  1.34  0.021
#655874             0.1989000 0.19900000  6 -718.437 1449.3  1.79  0.017
#135682             0.1988000 0.19890000  6 -718.453 1449.4  1.82  0.017
#524802             0.1899000 0.19000000  5 -719.525 1449.4  1.83  0.017
#1538               0.1898000 0.18990000  5 -719.539 1449.4  1.86  0.016
#4325890  0.0991200 0.1985000 0.19860000  6 -718.493 1449.4  1.90  0.016
#393730             0.1982000 0.19830000  6 -718.518 1449.5  1.95  0.016

dredge.vb <- glm(dams_per_km2~ 
                   #GEOMORPHOLOGY
                   GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+MEANANNCMS_mean+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+
                   #HYDROLOGY
                   StrmPow_mean
                 
                 , data = unburned_VB)

summary(dredge.vb)

nagelkerke(dredge.vb)
accuracy(list(dredge.vb))
#Only with burned valley bottoms:
#Pseudo.R.squared                               
#McFadden                            0.029      
#Cox and Snell (ML)                  0.199          
#Nagelkerke (Cragg and Uhler)        0.199          
#Efron.r.squared                     0.144


#Combine the various models




netmap.veg.vb <- glm(dams_per_km2~ 
                       #GEOMORPHOLOGY
                       GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                       #HYDROLOGY
                       StrmPow_mean+MEANANNCMS_mean+
                       
                       #VEGETATION
                       #From MaCander and Nelson
                       percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                     
                     
                     , data = unburned_VB)

summary(netmap.veg.vb)

nagelkerke(netmap.veg.vb)
accuracy(list(netmap.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.074    
#Cox and Snell (ML)                  0.436      
#Nagelkerke (Cragg and Uhler)        0.436      
#Efron.r.squared                     0.397 








geo.veg.vb <- glm(dams_per_km2~ 
                    #GEOMORPHOLOGY
                    GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                    #VEGETATION
                    #From MaCander and Nelson
                    percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                  
                  
                  , data = unburned_VB)

summary(geo.veg.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(geo.veg.vb)
accuracy(list(geo.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.073      
#Cox and Snell (ML)                  0.431      
#Nagelkerke (Cragg and Uhler)        0.431      
#Efron.r.squared                     0.391







hydro.veg.vb <- glm(dams_per_km2~ 
                      #HYDROLOGY
                      StrmPow_mean+MEANANNCMS_mean+
                      #Vegetation
                      #From MaCander and Nelson
                      percent_woody_cover+ConiferTree_2015_MEAN+BroadleafTree_2015_MEAN+DeciduousShrub_2015_MEAN+EvergreenShrub_2015_MEAN+Forb_2015_MEAN+Graminoid_2015_MEAN+tmLichenLight_2015_MEAN
                    
                    
                    , data = unburned_VB)

summary(hydro.veg.vb)
#Pseudo R-squared values are not directly comparable to the R-squared for OLS models. Nor can they be interpreted as the proportion of the variability in the dependent variable that is explained by model. Instead pseudo R-squared measures are relative measures among similar models indicating how well the model explains the data.
nagelkerke(hydro.veg.vb)
accuracy(list(hydro.veg.vb))


#Pseudo.R.squared                               
#McFadden                            0.040      
#Cox and Snell (ML)                  0.264      
#Nagelkerke (Cragg and Uhler)        0.264      
#Efron.r.squared                     0.264







Cand.mod <- list(netmap.vb, veg.vb, dredge.vb, netmap.veg.vb, geo.veg.vb, hydro.veg.vb) 

Modnames <- c("netmap.vb", "veg.vb", "dredge.vb", "netmap.veg.vb",  "geo.veg.vb", "hydro.veg.vb") 

aictab(Cand.mod, Modnames)




AICcmodavg::modavg(parm = c("GRADIENT_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -11.8
#Unconditional SE: 90.59
#90% Unconditional confidence interval: -160.81, 137.21

AICcmodavg::modavg(parm = c("FP_WIDTH_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 0
#Unconditional SE: 0.02
#90% Unconditional confidence interval: -0.03, 0.02

AICcmodavg::modavg(parm = c("Area_km2_VB"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -3.38
#Unconditional SE: 1.24
#90% Unconditional confidence interval: -5.41, -1.34

AICcmodavg::modavg(parm = c("SINUOSITY_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 32.84
#Unconditional SE: 22.56
#90% Unconditional confidence interval: -4.27, 69.95

AICcmodavg::modavg(parm = c("STRM_ORDER_max"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -0.2
#Unconditional SE: 1.59
#90% Unconditional confidence interval: -2.81, 2.41

AICcmodavg::modavg(parm = c("WIDTH_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 0.43
#Unconditional SE: 0.95
#90% Unconditional confidence interval: -1.13, 2

AICcmodavg::modavg(parm = c("DEPTH_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -42.69
#Unconditional SE: 33.47
#90% Unconditional confidence interval: -97.74, 12.36

AICcmodavg::modavg(parm = c("ELEV_M_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -0.02
#Unconditional SE: 0.01
#90% Unconditional confidence interval: -0.04, -0.01

AICcmodavg::modavg(parm = c("StrmPow_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 0
#Unconditional SE: 0.01
#90% Unconditional confidence interval: -0.02, 0.02

AICcmodavg::modavg(parm = c("MEANANNCMS_mean"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -0.55
#Unconditional SE: 0.71
#90% Unconditional confidence interval: -1.72, 0.61

AICcmodavg::modavg(parm = c("Time_Since_Burn_min"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("Fire_Event_Area_Percent_of_uRCA"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("percent_severe_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("percent_moderate_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("percent_mild_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 

AICcmodavg::modavg(parm = c("percent_no_burn"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 
#Unconditional SE: 
#90% Unconditional confidence interval: 


AICcmodavg::modavg(parm = c("percent_woody_cover"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Model-averaged estimate: -0.08
#Unconditional SE:  0.17
#90% Unconditional confidence interval: -0.36, 0.19


AICcmodavg::modavg(parm = c("ConiferTree_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -0.87
#Unconditional SE: 0.29
#90% Unconditional confidence interval: -1.35, -0.39

AICcmodavg::modavg(parm = c("BroadleafTree_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -0.65
#Unconditional SE: 0.26
#90% Unconditional confidence interval: -1.12, -0.18

AICcmodavg::modavg(parm = c("DeciduousShrub_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -0.92
#Unconditional SE: 0.33
#90% Unconditional confidence interval: -1.47, -0.37

AICcmodavg::modavg(parm = c("EvergreenShrub_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: 0.64
#Unconditional SE: 0.75
#90% Unconditional confidence interval: -0.59, 1.87

AICcmodavg::modavg(parm = c("Forb_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -1.98
#Unconditional SE: 1.11
#90% Unconditional confidence interval: -3.8, -0.16

AICcmodavg::modavg(parm = c("Graminoid_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -1.73
#Unconditional SE: 0.46
#90% Unconditional confidence interval: -2.48, -0.97

AICcmodavg::modavg(parm = c("tmLichenLight_2015_MEAN"), cand.set = Cand.mod, modnames = Modnames, conf.level = 0.90)
#Unburned
#Model-averaged estimate: -3.68
#Unconditional SE: 1.25
#90% Unconditional confidence interval: -5.74, -1.63













# Plots -------------------------------------------------------------------
'data.frame':	195 obs. of  166 variables:
  $ X.2                            : int  1 2 3 4 5 6 7 8 9 10 ...
$ X.1                            : int  1 2 3 4 5 6 7 8 9 10 ...
$ uRCA                           : int  328 352 362 373 376 392 696 725 758 787 ...
$ Dams_Per_Basin                 : int  2 1 1 1 5 5 1 1 1 1 ...
$ Burned_n                       : int  2 1 1 1 5 5 1 1 1 1 ...
$ Pond_Area_m2_mean              : num  1723 1920 2087 15540 630 ...
$ Main_n                         : int  2 1 1 1 5 5 1 1 1 1 ...
$ Side_n                         : int  2 1 1 1 5 5 1 1 1 1 ...
$ Off_n                          : int  2 1 1 1 5 5 1 1 1 1 ...
$ Burned_numerical               : int  1 1 1 1 0 0 0 0 0 0 ...
$ Time_Since_Burn_min            : int  48 14 48 14 NA NA NA NA NA NA ...
$ Z_mean                         : num  398 341 458 337 469 ...
$ alnus_PCT_MEAN                 : num  11.08 3.13 7.38 4.83 6.67 ...
$ betshr_PCT_MEAN                : num  6.2 15.7 2.31 14.39 17.26 ...
$ bettre_PCT_MEAN                : num  8.41 2.64 4.34 3.42 3.62 ...
$ dectre_PCT_MEAN                : num  10.95 4.98 5.92 5.8 4.52 ...
$ dryas_PCT_MEAN                 : num  2.773 1.189 2.556 0.157 0.335 ...
$ empnig_PCT_MEAN                : num  0.261 3.213 0.743 3.247 8.181 ...
$ erivag_PCT_MEAN                : num  0.139 18.751 0.207 16.146 2.117 ...
$ picgla_PCT_MEAN                : num  27.78 1.68 24.55 2.29 5.57 ...
$ picmar_PCT_MEAN                : num  8.17 7.34 8.75 3.94 16.96 ...
$ rhoshr_PCT_MEAN                : num  3.73 13.77 5.4 12.81 10.79 ...
$ salshr_PCT_MEAN                : num  18.1 20.9 15 21.8 18.5 ...
$ sphagn_PCT_MEAN                : num  0.0783 9.0034 0.1681 10.1852 3.7149 ...
$ vaculi_PCT_MEAN                : num  3.53 9.9 3.91 8.85 13.4 ...
$ vacvit_PCT_MEAN                : num  3.91 4.97 4.66 6.58 4.76 ...
$ uRCA_Area_km2                  : num  NA 1.464 NA 0.686 0.223 ...
$ Fire_Event_ID                  : int  NA 0 NA 0 NA NA NA NA NA NA ...
$ Fire_Event_Area_in_uRCA_km2    : num  NA 0.814 NA 0.664 NA ...
$ Fire_Event_Area_Percent_of_uRCA: num  NA 55.6 NA 96.8 NA ...
$ Fire_Event_Ignition_Date       : int  NA 20030614 NA 20030614 NA NA NA NA NA NA ...
$ BS1_AreaKm2                    : num  NA 0.1692 NA 0.0675 NA ...
$ BS2_AreaKm2                    : num  NA 0.431 NA 0.26 NA ...
$ BS3_AreaKm2                    : num  NA 0.18 NA 0.14 NA ...
$ BS4_AreaKm2                    : num  NA 0 NA 0 NA NA NA NA NA NA ...
$ BS_sum_AreaKm2                 : num  NA 0.7803 NA 0.4671 0.0684 ...
$ DNBR_MEAN                      : num  NA 270 NA 289 NA ...
$ RDNBR_MEAN                     : num  NA 276 NA 286 NA ...
$ uRCA_int                       : int  328 352 362 373 376 392 696 725 758 787 ...
$ SOLAR_COUNT                    : int  248 1639 1176 773 254 1309 453 343 85 1231 ...
$ SOLAR_MIN                      : num  586480 476481 437020 468432 534265 ...
$ SOLAR_MAX                      : num  625382 703638 715640 699110 663882 ...
$ SOLAR_RANGE                    : int  38902 227157 278621 230678 129617 136652 229691 173033 167177 243579 ...
$ SOLAR_MEAN                     : num  604605 596274 605643 601761 613573 ...
$ SOLAR_STD                      : num  7547 20595 32925 17010 18699 ...
$ SOLAR_SUM                      : num  149942060 977293013 712236025 465161259 155847559 ...
$ tmLichenLight_2015_MEAN        : num  0.0826 0.7864 1.8202 0.8488 0.484 ...
$ BroadleafTree_2015_MEAN        : num  13.99 4.97 4.31 7.86 7.07 ...
$ ConiferTree_2015_MEAN          : num  34.2 10.9 34.5 5.5 16.1 ...
$ DeciduousShrub_2015_MEAN       : num  16.1 22.8 12.3 26.5 37.4 ...
$ EvergreenShrub_2015_MEAN       : num  0.0413 10.8072 3.4953 9.6467 7.428 ...
$ Forb_2015_MEAN                 : num  3.012 1.133 1.426 1.476 0.316 ...
$ LENGTH_M_sum                   : num  856 4011 3827 1407 2377 ...
$ AREA_SQKM_mean                 : num  308.8 66 234.3 26.7 13.4 ...
$ OUT_DIST_min                   : num  121.2 73.1 131.6 71.8 76.6 ...
$ GRADIENT_mean                  : num  0.003 0.005 0.008 0.003 0.031 0.01 0.06 0.02 0.036 0.011 ...
$ MNANPRC_M_mean                 : num  0.425 0.336 0.444 0.383 0.409 0.367 0.528 0.4 0.392 0.417 ...
$ MEANANNCMS_mean                : num  1.873 0.264 1.477 0.119 0.063 ...
$ WIDTH_M_mean                   : num  5.306 2 4.714 1.349 0.979 ...
$ DEPTH_M_mean                   : num  0.165 0.095 0.154 0.076 0.063 0.332 0.051 0.059 0.037 0.403 ...
$ MAX_GRAD_D_max                 : num  0.03 0.022 0.042 0.023 0.04 0.028 0.166 0.034 0.056 0.026 ...
$ LAKE_max                       : int  0 0 0 0 0 0 1 0 0 0 ...
$ STRM_ORDER_max                 : int  4 4 4 3 3 4 4 4 3 4 ...
$ AZIMTH_DEG_mean                : num  257 109 264 182 215 ...
$ SINUOSITY_mean                 : num  1.26 1.13 1.1 1.05 1.03 ...
$ VAL_WIDTH_mean                 : num  70.59 17.04 41.29 14.74 9.38 ...
$ FP_WIDTH_mean                  : num  82.8 51 65.1 63.5 43.4 ...
$ DROPMAX_max                    : num  0 0 2.3 0 0 ...
$ Fish_min                       : int  1 1 1 1 1 1 0 1 1 1 ...
$ Basin_ID_min                   : int  5031603 5031705 5031603 5031706 5031607 3070110 5031302 5050701 5050701 3070103 ...
$ VWI_Floor_mean                 : num  13.3 8.58 8.76 10.89 9.65 ...
$ ValCnstrnt_mean                : num  15.6 25.6 13.8 47.2 44.4 ...
$ FlowVel_mean                   : num  0.449 0.409 0.672 0.296 0.866 ...
$ BFQ_mean                       : num  0.392 0.077 0.488 0.03 0.053 ...
$ StrmPow_mean                   : num  33.54 6.72 77.45 2.2 16.33 ...
$ BeavHab_min                    : int  1 1 0 1 0 1 0 1 0 0 ...
$ FISH_RESID_min                 : int  1 1 1 1 1 1 1 1 1 1 ...
$ GEP_Cum_mean                   : num  0.304 0.274 0.328 0.301 0.338 0.213 0.544 0.281 0.314 0.303 ...
$ GEP_mean                       : num  0.075 0.165 0.213 0.089 0.227 0.123 0.297 0.187 0.153 0.168 ...
$ IP_CHIN2_mean                  : num  0.852 0.444 0.741 0.342 0.154 0.317 0.05 0.217 0.083 0.436 ...
$ ELEV_M_mean                    : num  397 342 468 336 465 ...
$ Shape_Area.x                   : num  222680 5669609 7088819 1128514 3782073 ...
$ Area_km2_RCA                   : num  0.223 5.67 7.089 1.129 3.782 ...
$ Shape_Area.y                   : num  219239 1023228 667771 573267 279664 ...
$ Area_km2_VB                    : num  0.219 1.023 0.668 0.573 0.28 ...
$ Graminoid_2015_MEAN            : num  0.62 18.818 0.672 21.506 2.172 ...
$ dams_per_km2                   : num  9.122 0.977 1.498 1.744 17.879 ...
$ dams_per_km_linear             : num  0.002335 0.000249 0.000261 0.000711 0.002103 ...
$ mean_pond_area_m2perkm2        : int  7859 1877 3126 27107 2254 1121 4685 3102 12915 2731 ...
$ percent_main_ponds             : int  100 100 100 100 100 100 100 100 100 100 ...
$ percent_side_ponds             : int  100 100 100 100 100 100 100 100 100 100 ...
$ percent_off_ponds              : int  100 100 100 100 100 100 100 100 100 100 ...
$ Burned_basin                   : int  0 1 0 1 1 0 0 0 0 1 ...
$ percent_severe_burn            : num  NA 0 NA 0 0 ...
$ percent_moderate_burn          : num  NA 17.591 NA 24.334 0.322 ...
$ percent_mild_burn              : num  NA 42.13 NA 45.37 4.83 ...
$ percent_no_burn                : num  NA 16.5 NA 11.8 19.3 ...
$ percent_woody_cover            : num  83.2 61.3 66.2 59.5 81.9 ...
$ decidous_conifer_ratio         : num  1.315 5.794 0.989 8.561 2.637 ...


str(VB_Y_sums_full, list.len = ncol(VB_Y_sums_full))

p1<- ggplot(VB_Y_sums_full, aes(x = percent_severe_burn, y = log(dams_per_km2)) )+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()
  
 
p2<- ggplot(VB_Y_sums_full, aes(x =  Fire_Event_Area_Percent_of_uRCA, y = log(dams_per_km2)) )+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()


p3<- ggplot(VB_Y_sums_full, aes(x =  percent_moderate_burn, y = log(dams_per_km2)) )+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()


p4<- ggplot(VB_Y_sums_full, aes(x =  Time_Since_Burn_min, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p5<- ggplot(VB_Y_sums_full, aes(x =  Burned_numerical, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()



p6<- ggplot(VB_Y_sums_full, aes(x =  MEANANNCMS_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()


p7<- ggplot(VB_Y_sums_full, aes(x =  BFQ_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p8<- ggplot(VB_Y_sums_full, aes(x =  VAL_WIDTH_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p9<- ggplot(VB_Y_sums_full, aes(x =  DEPTH_M_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p10<- ggplot(VB_Y_sums_full, aes(x =  WIDTH_M_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()


p11<- ggplot(VB_Y_sums_full, aes(x =  SINUOSITY_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()


p12<- ggplot(VB_Y_sums_full, aes(x =  GRADIENT_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()


p13<- ggplot(VB_Y_sums_full, aes(x =  ELEV_M_mean, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p14<- ggplot(VB_Y_sums_full, aes(x =  ConiferTree_2015_MEAN, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p15<- ggplot(VB_Y_sums_full, aes(x =  percent_woody_cover, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()

p16<- ggplot(VB_Y_sums_full, aes(x =  percent_woody_cover, y = log(dams_per_km2)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.5)+
  theme_cowplot()



plot_list <- list(p1, p2, p3, p4, p6, p8, p9, p10, p11, p12, p13, p15)

# Combine the plots in a 4x4 panel using the patchwork library
combined_plots <- 
  plot_list[[1]] +  plot_list[[2]] +  plot_list[[3]] +
  plot_list[[4]] +  plot_list[[5]] +  plot_list[[6]] +
  plot_list[[7]] +  plot_list[[8]] +  plot_list[[9]] +
  plot_list[[10]] +  plot_list[[11]] +  plot_list[[12]]+
  plot_layout(nrow = 4, ncol = 3)

combined_plots



combined_plots2 <- 
  plot_list[[1]] +  plot_list[[2]] +  plot_list[[3]] +
  plot_list[[4]] +  plot_list[[5]] +  plot_list[[6]] +
  plot_list[[7]] +  plot_list[[8]] +  plot_list[[9]] +
  plot_list[[10]] +  plot_list[[11]] +  plot_list[[12]]+
  plot_layout(nrow = 3, ncol = 4)

combined_plots2




ggsave(plot= combined_plots,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/covariates.jpeg",
       dpi = 500, 
       height = 13,
       width = 9,
       units = "in")



ggsave(plot= combined_plots2,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/covariates2.jpeg",
       dpi = 500, 
       height = 9,
       width = 13,
       units = "in")




# Model Prediction Plot ---------------------------------------------------

library(broom)
library(purrr)
library(zoo)
#install.packages("showtext")
#library(showtext)
#font_add("Times New Roman", regular = "path/to/Times_New_Roman.ttf")

library(extrafont)
font_import() 

loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  




model_data <- data.frame(dams_per_km2 = VB_Y_sums_full$dams_per_km2, 
                         GRADIENT_mean = VB_Y_sums_full$GRADIENT_mean, 
                         FP_WIDTH_mean = VB_Y_sums_full$FP_WIDTH_mean, 
                         Area_km2_VB = VB_Y_sums_full$Area_km2_VB, 
                         SINUOSITY_mean = VB_Y_sums_full$SINUOSITY_mean, 
                         STRM_ORDER_max = VB_Y_sums_full$STRM_ORDER_max, 
                         WIDTH_M_mean = VB_Y_sums_full$WIDTH_M_mean, 
                         DEPTH_M_mean = VB_Y_sums_full$DEPTH_M_mean, 
                         ELEV_M_mean = VB_Y_sums_full$ELEV_M_mean, 
                         StrmPow_mean = VB_Y_sums_full$StrmPow_mean, 
                         MEANANNCMS_mean = VB_Y_sums_full$MEANANNCMS_mean, 
                         Time_Since_Burn_min = VB_Y_sums_full$Time_Since_Burn_min, 
                         Fire_Event_Area_Percent_of_uRCA = VB_Y_sums_full$Fire_Event_Area_Percent_of_uRCA, 
                         percent_severe_burn = VB_Y_sums_full$percent_severe_burn, 
                         percent_moderate_burn = VB_Y_sums_full$percent_moderate_burn, 
                         percent_mild_burn = VB_Y_sums_full$percent_mild_burn,
                         percent_no_burn = VB_Y_sums_full$percent_no_burn)




model_data <- na.aggregate(model_data)




netmap.fire.vb <- glm(dams_per_km2~ 
                        #GEOMORPHOLOGY
                        GRADIENT_mean+FP_WIDTH_mean+Area_km2_VB+SINUOSITY_mean+STRM_ORDER_max+WIDTH_M_mean+DEPTH_M_mean+ELEV_M_mean+
                        #HYDROLOGY
                        StrmPow_mean+MEANANNCMS_mean+
                        #WILDFIRE
                        Time_Since_Burn_min+Fire_Event_Area_Percent_of_uRCA+percent_severe_burn+percent_moderate_burn+percent_mild_burn+percent_no_burn
                      
                      , data = model_data)

summary(netmap.fire.vb)


#create vector of variable names
( mod_vars = all.vars( formula(netmap.fire.vb) )[-1] )




###function to create prediction datasets
preddat_fun = function(data, allvars, var) {
  sums = summarise_at(data, 
                      vars( one_of(allvars), -one_of(var) ), 
                      median) 
  cbind( select_at(data, var), sums)
}

pred_dat = mod_vars %>%
  set_names() %>%
  map( ~preddat_fun(model_data, mod_vars, .x) )


preds = pred_dat %>%
  map(~augment(netmap.fire.vb, newdata = .x, se_fit = TRUE) ) %>%
  map(~mutate(.x, 
              lower = .fitted - 2*.se.fit,
              upper = .fitted + 2*.se.fit))



pred_plot = function(data, variable, xlab) {
  ggplot(data, aes(x = .data[[variable]], y = .fitted) ) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
    #geom_point(alpha = 0.45) +
    theme_bw(base_size = 14) +
    labs(x = xlab,
         y = "Beaver Pond Density")+
    theme_cowplot()
  #ylim(-10,30)
}


xlabs = c("Gradient", "Floodplain Width", "Basin Area (km^2)", 
          "Sinuosity", "Stream Order", "Stream Width", "Stream Depth", "Elevation",
          "Stream Power", "Mean Annual Discharge", "Years Since Last Burn",
          "Percent Burned", "Percent Severe Burn", "Percent Moderate Burn",
          "Percent Mild Burn", "Percent Ubnurned")



#this lets you check out individual plots
pred_plot(preds[[2]], mod_vars[2], xlabs[2])

##put all plots together
all_plots = pmap( list(preds, mod_vars, xlabs), pred_plot)
all_plots

#plot in a grid format
model_predictions <- cowplot::plot_grid(plotlist = all_plots,
                   labels = "AUTO",
                   align = "hv")
model_predictions


ggsave(plot= model_predictions,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/model_predictions_no_outliers.jpeg",
       dpi = 300, 
       height = 10,
       width = 10,
       units = "in")







p1 <- pred_plot(preds[[3]], mod_vars[3], xlabs[3])
p1

ggsave(plot= p1,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/basin_area.jpeg",
       dpi = 300, 
       height = 3,
       width = 3,
       units = "in")



p2 <- pred_plot(preds[[4]], mod_vars[4], xlabs[4])
p2

ggsave(plot= p2,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/sinuosity.jpeg",
       dpi = 300, 
       height = 3,
       width = 3,
       units = "in")



p3 <- pred_plot(preds[[5]], mod_vars[5], xlabs[5])
p3

ggsave(plot= p3,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/stream_order.jpeg",
       dpi = 300, 
       height = 3,
       width = 3,
       units = "in")


p4 <- pred_plot(preds[[8]], mod_vars[8], xlabs[8])
p4

ggsave(plot= p4,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/elevation.jpeg",
       dpi = 300, 
       height = 3,
       width = 3,
       units = "in")


p5 <- pred_plot(preds[[13]], mod_vars[13], xlabs[13])
p5

ggsave(plot= p5,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/severe_burn.jpeg",
       dpi = 300, 
       height = 3,
       width = 3,
       units = "in")


p6 <- pred_plot(preds[[14]], mod_vars[14], xlabs[14])
p6

ggsave(plot= p6,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/moderate_burn.jpeg",
       dpi = 300, 
       height = 3,
       width = 3,
       units = "in")




library(gridExtra)



grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 6)


CASC_plots <- grid.arrange(p5, p6, p1, p2, p3, p4, ncol = 6)
CASC_plots



ggsave(plot= CASC_plots,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/CASC_plots2.jpeg",
       dpi = 300, 
       height = 2.5,
       width = 15,
       units = "in")






test <- glm(dams_per_km2~ Area_km2_VB, data = model_data)

summary(test)

ggplot(VB_Y_sums_full, aes(dams_per_km2, Area_km2_VB))+
  geom_point()

ggplot(VB_Y_sums_full, aes(log(dams_per_km2), Area_km2_VB))+
  geom_point()

ggplot(VB_Y_sums_full, aes(log(dams_per_km2), log(Area_km2_VB)))+
  geom_point()












