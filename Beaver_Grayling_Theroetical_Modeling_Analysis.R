################################################################################
################ Beaver and Grayling Theoretical Modeling Analysis #############
################################# Will Samuel ##################################
################################################################################


library(tidyverse)
library(ggalt)      #For ellipse function
library(cowplot)
library(patchwork)
library(data.table) #Converts the dataframes to bianary so they take up less space
library(stringr)


#If you continually run into memory issues, you can use this function to increase the memory allocation to R. Just use this as a last resort 
#memory.limit(size = 12000)


setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2")

#you only need these for the data manipulation section, they are huge files so try not to use them
#full.dat <- fread("Theoretical Passage Data Files/full_length_by_reach_and_poprate.csv")

#habitat.dat <- fread("Theoretical Passage Data Files/habitat_length_by_reach_and_poprate.csv")

#HUC12.dat <- fread("Theoretical Passage Data Files/huc12_reaches.csv")



#You might need to use these to generate datasets if you want something other than a POP of 20, 50, 80, or 100
#full.dat.huc <- fread("Theoretical Passage Data Files/full.dat.huc.csv")

#habitat.dat.huc <- fread("Theoretical Passage Data Files/habitat.dat.huc.csv")

#HUC.sums.full <- fread("Theoretical Passage Data Files/HUC.sums.full.csv")

#HUC.sums.habitat <- fread("Theoretical Passage Data Files/HUC.sums.habitat.csv")



POP.20HUC.HAB.dat <- fread("Theoretical Passage Data Files/POP.20HUC.HAB.dat.csv")

POP.50HUC.HAB.dat <- fread("Theoretical Passage Data Files/POP.50HUC.HAB.dat.csv")

POP.80HUC.HAB.dat <- fread("Theoretical Passage Data Files/POP.80HUC.HAB.dat.csv")

POP.100HUC.HAB.dat <- fread("Theoretical Passage Data Files/POP.100HUC.HAB.dat.csv")



#read in the HUC beaver density data
#beaver.dat <- fread("Theoretical Passage Data Files/tribs_full_with_data.csv")


streams.dat <- fread("Theoretical Passage Data Files/netmap_reach_uID.csv")


VB.beaver.fire.dat <- fread("Theoretical Passage Data Files/VB_with_beaver_fire_data.csv")



#I might be able to pull the beaver density and fire data from this datsheet
#Remeber its summarized by VB/RCA....
#VB.dat <- read.csv("Input Data Files/VB_Y_sums_full_EDITED.csv")


#fire.dat <- read.csv("Input Data Files/............")

#beaver.dams.2km <- read.csv("Input Data Files/NetMap_2km_to_Beaver_Points.csv")

#beaver.dams.edges <- read.csv("Input Data Files/EDGES_to_Beaver_Points.csv")



# Data manipulation -------------------------------------------------------

#Skip this section unless you specifically want to change the data...



full.dat <- full.dat %>%  dplyr::rename(uID = origin)

#Need to join the reach data to each HUC
full.dat.huc <- left_join(x = full.dat, y = HUC12.dat, by = "uID") 

#Create a column for each basin
full.dat.huc$basin <- str_extract(full.dat.huc$uID, "[A-Za-z]+")



#Do that for the habitat dataset
habitat.dat <- habitat.dat %>%  dplyr::rename(uID = origin)

#This one is really big so I need to break it up to perform these functions
str(habitat.dat)
habitat.dat.1 <- habitat.dat[c(1:11437524),]
habitat.dat.2 <- habitat.dat[c(11437525:22875048),]


habitat.dat.huc.1 <- left_join(x = habitat.dat.1, y = HUC12.dat, by = "uID")
habitat.dat.huc.2 <- left_join(x = habitat.dat.2, y = HUC12.dat, by = "uID")

habitat.dat.huc.1$basin <- str_extract(habitat.dat.huc.1$uID, "[A-Za-z]+")
habitat.dat.huc.2$basin <- str_extract(habitat.dat.huc.2$uID, "[A-Za-z]+")


#Now rejoin the datasets
habitat.dat.huc <- rbind(habitat.dat.huc.1, habitat.dat.huc.2)
View(habitat.dat.huc)




#Quick plotting
ggplot(HUC.sums.full) +
  geom_boxplot(aes(x = "TOTAL_LENGTH_KM", y = TOTAL_LENGTH_KM), position = position_dodge(width = 0.3), col = "darkblue", fill = "blue", alpha = 0.5) +
  geom_boxplot(aes(x = "PASS_LENGTH_KM", y = PASS_LENGTH_KM), position = position_dodge(width = 0.3), col = "darkred", fill = "red", alpha = 0.5) +
  #geom_jitter(aes(x = "TOTAL_LENGTH_KM", y = TOTAL_LENGTH_KM), position = position_jitter(width = 0.35), col = "darkblue", fill = "blue", alpha = 0.5) +
  #geom_jitter(aes(x = "PASS_LENGTH_KM", y = PASS_LENGTH_KM), position = position_jitter(width = 0.35), col = "darkred", fill = "red", alpha = 0.5) +
  geom_violin(aes(x = "TOTAL_LENGTH_KM", y = TOTAL_LENGTH_KM), position = position_dodge(width = 0.8), col = "darkblue", fill = "blue", alpha = 0.5) +
  geom_violin(aes(x = "PASS_LENGTH_KM", y = PASS_LENGTH_KM), position = position_dodge(width = 0.8), col = "darkred", fill = "red", alpha = 0.5) +
  theme_cowplot()


#Group by HUC
HUC.sums.full <- full.dat.huc %>% 
  group_by(HUC12, POP_rate) %>% 
  summarize("TOTAL_LENGTH_KM" = sum(TOTAL_LENGTH_KM),
            "PASS_LENGTH_KM" = sum(PASS_LENGTH_KM), 
            "basin" = unique(basin))


HUC.sums.habitat <- habitat.dat.huc %>% 
  group_by(HUC12, POP_rate) %>% 
  summarize("TOTAL_LENGTH_KM" = sum(TOTAL_LENGTH_HAB_KM),
            "PASS_LENGTH_KM" = sum(PASS_LENGTH_HAB_KM), 
            "basin" = unique(basin))




#looks good, lets go with that
write.csv(full.dat.huc, "Theoretical Passage Data Files/full.dat.huc.csv")

write.csv(habitat.dat.huc, "Theoretical Passage Data Files/habitat.dat.huc.csv")

write.csv(HUC.sums.full, "Theoretical Passage Data Files/HUC.sums.full.csv")

write.csv(HUC.sums.habitat, "Theoretical Passage Data Files/HUC.sums.habitat.csv")





#Summarize data by each POP for each streamline and HUC ---------------


# Define the range of POP_rate values
pop_rates <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)

# Create separate data frames for each POP rate
for (pop_rate in pop_rates) {
  # Filter the full.dat.huc dataset based on the current POP_rate
  filtered_data <- full.dat.huc %>% filter(POP_rate == pop_rate)
  
  # Create a new data frame object with a name corresponding to the POP rate
  assign(paste0("POP.", pop_rate, ".dat"), filtered_data)
}


# repeat this for the habitat dataset
for (pop_rate in pop_rates) {
  filtered_data <- habitat.dat.huc %>% filter(POP_rate == pop_rate)
  
  # Create a new data frame object with a name corresponding to the POP rate
  assign(paste0("POP.", pop_rate, "hab.dat"), filtered_data)
}


# repeat this for the HUC sums
for (pop_rate in pop_rates) {
  # Filter the full.dat.huc dataset based on the current POP_rate
  filtered_data <- HUC.sums.full %>% filter(POP_rate == pop_rate)
  
  # Create a new data frame object with a name corresponding to the POP rate
  assign(paste0("POP.", pop_rate, "HUC.dat"), filtered_data)
}


# repeat this for the HUC sums
for (pop_rate in pop_rates) {
  # Filter the full.dat.huc dataset based on the current POP_rate
  filtered_data <- HUC.sums.habitat %>% filter(POP_rate == pop_rate)
  
  # Create a new data frame object with a name corresponding to the POP rate
  assign(paste0("POP.", pop_rate, "HUC.HAB.dat"), filtered_data)
}




write.csv(POP.20HUC.HAB.dat, "Theoretical Passage Data Files/POP.20HUC.HAB.dat.csv")
write.csv(POP.50HUC.HAB.dat, "Theoretical Passage Data Files/POP.50HUC.HAB.dat.csv")
write.csv(POP.80HUC.HAB.dat, "Theoretical Passage Data Files/POP.80HUC.HAB.dat.csv")
write.csv(POP.100HUC.HAB.dat, "Theoretical Passage Data Files/POP.100HUC.HAB.dat.csv")






#I am also gonna read out the some data so I can use those for plotting in ArcGIS

#First I need to assign the original uIDs back to each dataset so I can use this for mapping
VB.beaver.dat

str(streams.dat)


setequal(streams.dat$uID, POP.0.dat$uID)


write.csv(POP.20.dat, "Theoretical Passage Data Files/POP.20.dat.csv")
write.csv(POP.50.dat, "Theoretical Passage Data Files/POP.50.dat.csv")
write.csv(POP.80.dat, "Theoretical Passage Data Files/POP.80.dat.csv")
write.csv(POP.100.dat, "Theoretical Passage Data Files/POP.100.dat.csv")
#Habitat data
write.csv(POP.20hab.dat, "Theoretical Passage Data Files/POP.20.hab.dat.csv")
write.csv(POP.50hab.dat, "Theoretical Passage Data Files/POP.50.hab.dat.csv")
write.csv(POP.80hab.dat, "Theoretical Passage Data Files/POP.80.hab.dat.csv")
write.csv(POP.100hab.dat, "Theoretical Passage Data Files/POP.100.hab.dat.csv")






# Compare the NetMap streamlines, graying distribution model, and available habitat ----



#First use the full dataset (all streams > 2nd order)
#combined_data <- rbind(
#  transform(POP.100HUC.dat, POP = "POP.100HUC.dat"),
#  transform(POP.80HUC.dat, POP = "POP.80HUC.dat"),
#  transform(POP.50HUC.dat, POP = "POP.50HUC.dat"),
#  transform(POP.20HUC.dat, POP = "POP.20HUC.dat")
#)

#mean(POP.100HUC.dat$PASS_LENGTH_KM) #11257.8
#mean(POP.20HUC.dat$PASS_LENGTH_KM) #9964.616
#About 12% difference

#t.test(POP.100HUC.dat$PASS_LENGTH_KM, POP.20HUC.dat$PASS_LENGTH_KM) #Not a significant difference overall... p = 0.1169


#ggplot(combined_data) +
#  geom_violin(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)),
#              width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
#  geom_boxplot(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)),
#               width = 5, position = position_dodge(width = 0.8), alpha = 0.8) +
#  scale_fill_manual(values = c("20" = "darkred", "50" = "darkblue", "80" = "darkgreen")) +
#  geom_hline(aes(yintercept = 11257), linetype = "dashed", color = "black", lwd = 1) +
#  geom_hline(aes(yintercept = 9964), linetype = "dashed", color = "darkred", lwd = 1) +
#  scale_x_continuous(breaks = seq(0, 100, 10))+
#  labs(x = "Probability of Passage", y = "Available Habitat")+
#  theme_cowplot()+
#  theme(legend.position = "none")



#ggplot(combined_data) +
#  geom_violin(aes(x = POP_rate, y=PASS_LENGTH_KM, group = factor(POP_rate)),
#              fill = "lightgrey",width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
#  geom_point(aes(x = POP_rate, y = PASS_LENGTH_KM), fill = "black", alpha = 0.3)+
#  geom_smooth(aes(x = POP_rate, y = PASS_LENGTH_KM), method = "lm")+
#  scale_x_continuous(breaks = seq(0, 100, 10))+
#  labs(x = "Probability of Passage", y = "Available Habitat")+
#  theme_bw()+
#  theme(legend.position = "none")




#Compare among basins (e.g., Chena, Chatinika, etc?)
#ggplot(combined_data) +
#  geom_violin(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)), width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
#  geom_boxplot(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)),
#               width = 5, position = position_dodge(width = 0.8), alpha = 0.8) +
#  scale_fill_manual(values = c("20" = "darkred", "50" = "darkblue", "80" = "darkgreen")) +
  #geom_hline(aes(yintercept = 11257), linetype = "dashed", color = "black", lwd = 1) +
  #geom_hline(aes(yintercept = 9964), linetype = "dashed", color = "darkred", lwd = 1) +
#  scale_x_continuous(breaks = seq(0, 100, 20))+
#  labs(x = "Probability of Passage", y = "Available Habitat")+
#  theme_cowplot()+
#  theme(legend.position = "none")+
#  facet_wrap(~ basin)





Total_Length <- POP.100HUC.HAB.dat$TOTAL_LENGTH_KM

POP.100HUC.HAB.dat <- cbind(POP.100HUC.HAB.dat, Total_Length)
POP.80HUC.HAB.dat <- cbind(POP.80HUC.HAB.dat, Total_Length)
POP.50HUC.HAB.dat <- cbind(POP.50HUC.HAB.dat, Total_Length)
POP.20HUC.HAB.dat <- cbind(POP.20HUC.HAB.dat, Total_Length)

POP.100HUC.HAB.dat <- POP.100HUC.HAB.dat[,-c(1,2)]
POP.80HUC.HAB.dat <- POP.80HUC.HAB.dat[,-c(1,2)]
POP.50HUC.HAB.dat <- POP.50HUC.HAB.dat[,-c(1,2)]
POP.20HUC.HAB.dat <- POP.20HUC.HAB.dat[,-c(1,2)]



#Then repeat with the grayling habitat distribution model

combined_hab_data <- rbind(
  transform(POP.100HUC.HAB.dat, POP = "POP.100HUC.dat"),
  transform(POP.80HUC.HAB.dat, POP = "POP.80HUC.dat"),
  transform(POP.50HUC.HAB.dat, POP = "POP.50HUC.dat"),
  transform(POP.20HUC.HAB.dat, POP = "POP.20HUC.dat")
)


#Calculate the proportion of habitat reduced
combined_hab_data <- combined_hab_data %>% 
  mutate(percent_available_habitat = (PASS_LENGTH_KM/Total_Length)*100)



mean(POP.100HUC.HAB.dat$PASS_LENGTH_KM) #22515.61
mean(POP.20HUC.HAB.dat$PASS_LENGTH_KM) #19929.23

t.test(POP.100HUC.HAB.dat$PASS_LENGTH_KM, POP.20HUC.HAB.dat$PASS_LENGTH_KM) #Not a significant difference overall... p = 0.1169





# Check for duplicate column names
#duplicated_cols <- duplicated(names(combined_hab_data))
#duplicated_cols <- duplicated_cols | duplicated(names(combined_hab_data), fromLast = TRUE)
#duplicated_cols <- names(combined_hab_data)[duplicated_cols]
#print(duplicated_cols)

#combined_hab_data <- combined_hab_data[,-1]


#Colorblind friendly version:   scale_fill_manual(values = c("20" = "#D55E00", "50" = "#E69F00", "80" = "#009E73")) +


ggplot(combined_hab_data) +
  geom_violin(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)),
              width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
  geom_boxplot(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)),
               width = 5, position = position_dodge(width = 0.8), alpha = 0.8) +
  scale_fill_manual(values = c("20" = "darkred", "50" = "#E69F00", "80" = "darkgreen", "100" = "darkblue")) +
  geom_hline(aes(yintercept = 22515), linetype = "dashed", color = "black", lwd = 1) +
  geom_hline(aes(yintercept = 19929), linetype = "dashed", color = "darkred", lwd = 1) +
  scale_x_continuous(breaks = seq(0, 100, 10))+
  labs(x = "Probability of Passage", y = "Available Habitat")+
  theme_cowplot()+
  theme(legend.position = "none")


ggplot(combined_hab_data) +
  geom_violin(aes(x = POP_rate, y=percent_available_habitat, fill = factor(POP_rate)),
              width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
  geom_boxplot(aes(x = POP_rate, y=percent_available_habitat, fill = factor(POP_rate)),
               width = 5, position = position_dodge(width = 0.8), alpha = 0.8) +
  scale_fill_manual(values = c("20" = "darkred", "50" = "#E69F00", "80" = "darkgreen", "100" = "darkblue")) +
  scale_x_continuous(breaks = seq(0, 100, 10))+
  labs(x = "Probability of Passage", y = "Available Habitat")+
  theme_cowplot()+
  theme(legend.position = "none")









ggplot(combined_hab_data) +
  geom_violin(aes(x = POP_rate, y=PASS_LENGTH_KM, group = factor(POP_rate)), 
              fill = "lightgrey", width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
  geom_point(aes(x = POP_rate, y = PASS_LENGTH_KM), fill = "black", alpha = 0.3)+
  geom_smooth(aes(x = POP_rate, y = PASS_LENGTH_KM), method = "lm")+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  labs(x = "Probability of Passage", y = "Available Habitat")+
  theme_bw()+
  theme(legend.position = "none")


ggplot(combined_hab_data) +
  geom_violin(aes(x = POP_rate, y=percent_available_habitat, group = factor(POP_rate)), 
              fill = "lightgrey", width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
  geom_point(aes(x = POP_rate, y = percent_available_habitat), fill = "black", alpha = 0.3)+
  geom_smooth(aes(x = POP_rate, y = percent_available_habitat), method = "lm")+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  labs(x = "Probability of Passage", y = "Available Habitat")+
  theme_bw()+
  theme(legend.position = "none")






ggplot(combined_hab_data) +
  geom_violin(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)), width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
  geom_boxplot(aes(x = POP_rate, y=PASS_LENGTH_KM, fill = factor(POP_rate)),
               width = 5, position = position_dodge(width = 0.8), alpha = 0.8) +
  scale_fill_manual(values = c("20" = "darkred", "50" = "#E69F00", "80" = "darkgreen", "100" = "darkblue")) +
  #geom_hline(aes(yintercept = 11257), linetype = "dashed", color = "black", lwd = 1) +
  #geom_hline(aes(yintercept = 9964), linetype = "dashed", color = "darkred", lwd = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20))+
  labs(x = "Probability of Passage", y = "Available Habitat")+
  theme_cowplot()+
  theme(legend.position = "none")+
  facet_wrap(~ basin)




ggplot(combined_hab_data) +
  geom_violin(aes(x = POP_rate, y=percent_available_habitat, fill = factor(POP_rate)), width = 15, position = position_dodge(width = 0.8), alpha = 0.3) +
  #geom_boxplot(aes(x = POP_rate, y=percent_available_habitat, fill = factor(POP_rate)),
  #             width = 5, position = position_dodge(width = 0.8), alpha = 0.8) +
  scale_fill_manual(values = c("20" = "darkred", "50" = "#E69F00", "80" = "darkgreen", "100" = "darkblue")) +
  #geom_hline(aes(yintercept = 11257), linetype = "dashed", color = "black", lwd = 1) +
  #geom_hline(aes(yintercept = 9964), linetype = "dashed", color = "darkred", lwd = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20))+
  labs(x = "Probability of Passage", y = "Available Habitat")+
  theme_cowplot()+
  theme(legend.position = "none")+
  facet_wrap(~ basin)






#Do some analysis...

#Use ANOVA to evaluate differences between POP
combined_data <- rbind(POP.20HUC.HAB.dat, POP.50HUC.HAB.dat, POP.80HUC.HAB.dat, POP.100HUC.HAB.dat)

model <- aov(PASS_LENGTH_KM ~ factor(POP_rate), data = combined_data)
summary(model)
TukeyHSD(model)


mean(POP.100HUC.HAB.dat$PASS_LENGTH_KM) #22515.61
mean(POP.20HUC.HAB.dat$PASS_LENGTH_KM) #19929.23


sum(POP.100HUC.HAB.dat$PASS_LENGTH_KM) #4525637
sum(POP.80HUC.HAB.dat$PASS_LENGTH_KM) #4473918
sum(POP.50HUC.HAB.dat$PASS_LENGTH_KM) #4245006
sum(POP.20HUC.HAB.dat$PASS_LENGTH_KM) #4005775



#Summary for Table 4
#Chatanika
dat100 <- combined_hab_data %>% 
  filter(basin == "chat", POP == "POP.100HUC.dat")
sum(dat100$PASS_LENGTH_KM) #Potential Fish Habitat = 932905.8

dat80 <- combined_hab_data %>% 
  filter(basin == "chat", POP == "POP.80HUC.dat")
sum(dat80$PASS_LENGTH_KM) #Potential Fish Habitat = 927252.7

dat50 <- combined_hab_data %>% 
  filter(basin == "chat", POP == "POP.50HUC.dat")
sum(dat50$PASS_LENGTH_KM) #Potential Fish Habitat = 812877.5

dat20 <- combined_hab_data %>% 
  filter(basin == "chat", POP == "POP.20HUC.dat")
sum(dat20$PASS_LENGTH_KM) #Potential Fish Habitat = 749704.7


#Chena
dat100 <- combined_hab_data %>% 
  filter(basin == "chen", POP == "POP.100HUC.dat")
sum(dat100$PASS_LENGTH_KM) #Potential Fish Habitat = 1368488

dat80 <- combined_hab_data %>% 
  filter(basin == "chen", POP == "POP.80HUC.dat")
sum(dat80$PASS_LENGTH_KM) #Potential Fish Habitat = 1323765

dat50 <- combined_hab_data %>% 
  filter(basin == "chen", POP == "POP.50HUC.dat")
sum(dat50$PASS_LENGTH_KM) #Potential Fish Habitat = 1239530

dat20 <- combined_hab_data %>% 
  filter(basin == "chen", POP == "POP.20HUC.dat")
sum(dat20$PASS_LENGTH_KM) #Potential Fish Habitat = 1181095



#Salcha
dat100 <- combined_hab_data %>% 
  filter(basin == "salc", POP == "POP.100HUC.dat")
sum(dat100$PASS_LENGTH_KM) #Potential Fish Habitat = 1139103

dat80 <- combined_hab_data %>% 
  filter(basin == "salc", POP == "POP.80HUC.dat")
sum(dat80$PASS_LENGTH_KM) #Potential Fish Habitat = 1138513

dat50 <- combined_hab_data %>% 
  filter(basin == "salc", POP == "POP.50HUC.dat")
sum(dat50$PASS_LENGTH_KM) #Potential Fish Habitat = 1120476

dat20 <- combined_hab_data %>% 
  filter(basin == "salc", POP == "POP.20HUC.dat")
sum(dat20$PASS_LENGTH_KM) #Potential Fish Habitat = 1040521



#Goodpaster
dat100 <- combined_hab_data %>% 
  filter(basin == "good", POP == "POP.100HUC.dat")
sum(dat100$PASS_LENGTH_KM) #Potential Fish Habitat = 607,049.3

dat80 <- combined_hab_data %>% 
  filter(basin == "good", POP == "POP.80HUC.dat")
sum(dat80$PASS_LENGTH_KM) #Potential Fish Habitat = 607,047.3

dat50 <- combined_hab_data %>% 
  filter(basin == "good", POP == "POP.50HUC.dat")
sum(dat50$PASS_LENGTH_KM) #Potential Fish Habitat = 600,717.2

dat20 <- combined_hab_data %>% 
  filter(basin == "good", POP == "POP.20HUC.dat")
sum(dat20$PASS_LENGTH_KM) #Potential Fish Habitat = 591,138





#Shaw Creek
dat100 <- combined_hab_data %>% 
  filter(basin == "shaw", POP == "POP.100HUC.dat")
sum(dat100$PASS_LENGTH_KM) #Potential Fish Habitat = 478,091.2

dat80 <- combined_hab_data %>% 
  filter(basin == "shaw", POP == "POP.80HUC.dat")
sum(dat80$PASS_LENGTH_KM) #Potential Fish Habitat = 477338.4

dat50 <- combined_hab_data %>% 
  filter(basin == "shaw", POP == "POP.50HUC.dat")
sum(dat50$PASS_LENGTH_KM) #Potential Fish Habitat = 471404.9

dat20 <- combined_hab_data %>% 
  filter(basin == "shaw", POP == "POP.20HUC.dat")
sum(dat20$PASS_LENGTH_KM) #Potential Fish Habitat = 443316.6





#Check for differences in certain basins
chat <- aov(PASS_LENGTH_KM ~ factor(POP_rate), 
             data = combined_data, subset=(basin=="chat"))
summary(chat)
TukeyHSD(chat)

#chat_pass <- combined_data %>%
#  filter(basin == "chat")


#Total available habitat:

chen <- aov(PASS_LENGTH_KM ~ factor(POP_rate), 
            data = combined_data, subset=(basin=="chen"))
summary(chen)
TukeyHSD(chen)


salc <- aov(PASS_LENGTH_KM ~ factor(POP_rate), 
            data = combined_data, subset=(basin=="salc"))
summary(salc)
TukeyHSD(salc)


shaw <- aov(PASS_LENGTH_KM ~ factor(POP_rate), 
            data = combined_data, subset=(basin=="shaw"))
summary(shaw)
TukeyHSD(shaw)

good <- aov(PASS_LENGTH_KM ~ factor(POP_rate), 
            data = combined_data, subset=(basin=="good"))
summary(good)
TukeyHSD(good)
#Overall, there are more differences in habitat availability in the Chena and Chatanika rivers, but none of them are statistically significant (p < 0.05). This is unsurprising because the Chena and Chatanika have the most beaver ponds. 





# Compare results against beaver density, summarized by HUC12 -------------



####____________________________________________________________________________
#Consider this type of plot that Josh and I discussed:
#https://seaborn.pydata.org/tutorial/distributions.html
#Figure located below the python code: sns.displot(penguins, x="bill_length_mm", y="bill_depth_mm", hue="species", kind="kde")

#example of how to plot...?
# Creating the theoretical data 
set.seed(1)  # Setting a seed for reproducibility
n <- 400  # Total number of data points

# Creating available habitat values for each group with a negative relationship
available_habitat <- c(
  rnorm(n/4, mean = 23, sd = 5),
  rnorm(n/4, mean = 20, sd = 3),
  rnorm(n/4, mean = 15, sd = 3),
  rnorm(n/4, mean = 10, sd = 3)
)

# Creating beaver density values for each group with a negative relationship
beaver_density <- c(
  rnorm(n/4, mean = 0, sd = 0),
  rnorm(n/4, mean = 25, sd = 7),
  rnorm(n/4, mean = 50, sd = 7),
  rnorm(n/4, mean = 75, sd = 7)
)

# Creating the group variable
group <- rep(1:4, each = n/4)

df <- data.frame(available_habitat, beaver_density, POP = factor(group))


ggplot(df, aes(x = beaver_density, y = available_habitat))+  
  geom_point(alpha = 0.6, size = 2, col = "darkgrey")+
  geom_smooth(method="lm")+
  scale_y_continuous(breaks=seq(0, 35, 5))+
  theme_cowplot()

ggplot(df)+  
  geom_point(aes(x= beaver_density, y=available_habitat, col = POP), 
             alpha = 0.5, size = 2, show.legend = FALSE)+
  geom_encircle(aes(x = beaver_density, y = available_habitat, fill = POP, color = NA),                  expand = 0.03, show.legend = FALSE, alpha = 0.15) +
  scale_color_manual(values = c("darkblue", "darkgreen", "darkorange", "darkred"))+
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred"))+
  #geom_smooth(aes(x=available_habitat, y= beaver_density), method="lm", col = "black")+
  scale_y_continuous(breaks=seq(0, 35, 5))+
  theme_cowplot()


ggplot(df, aes(x= beaver_density, y=available_habitat, col = POP, fill = POP))+  
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred"))+
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred"))+
  #geom_smooth(aes(x=available_habitat, y= beaver_density), method="lm", col = "black")+
  scale_y_continuous(breaks=seq(0, 35, 5))+
  scale_x_continuous(breaks=seq(0, 100, 10))+
  theme_cowplot()
####____________________________________________________________________________














#There is not necessarily duplicates in this dataframe, but some of the valley bottoms overlap multiple HUCs, not sure how to ammend that, we'll forget about it for now.

#str(VB.beaver.dat)

#Remove duplicates first
#duplicate_rows <- duplicated(VB.beaver.dat)
#duplicate_rows
# Subset the dataframe to exclude duplicate rows
#VB.beaver.dat_unique <- subset(VB.beaver.dat, !duplicate_rows)





str(VB.beaver.fire.dat)


VB.beaver.fire.dat <- VB.beaver.fire.dat %>% 
  select(HUC12, ToHUC, Name, AreaSqKm, RCA_Area_SqKm, Point_Count, Surveyed, Year, Years, Z_Mean, Avg_Slope, Fire_Event_Area_in_uRCA_km2, BS1_AreaKm2,  BS2_AreaKm2,  BS3_AreaKm2,  BS4_AreaKm2, BS_sum_AreaKm2) %>% #ACRES,
  rename(Beaver_Count = Point_Count) %>% 
  mutate("Beaver_Density" = Beaver_Count/RCA_Area_SqKm)
str(VB.beaver.fire.dat)


  


#For this I am summarized beaver density based on the VALLEY BOTTOMS (VB area) not the HUCs. This is consistent with the beaver-fire analysis, and makes more sense than using the HUC area. 
VB.beaver.fire.dat <- VB.beaver.fire.dat %>% 
  group_by(HUC12) %>% 
  summarize(ToHUC = unique(ToHUC),
            Name = unique(Name),
            AreaSqKm = mean(AreaSqKm),
            VB_Area_SqKm = mean(RCA_Area_SqKm),
            Beaver_Count = sum(Beaver_Count),
            Surveyed = mean(Surveyed),
            Year = mean(Year), 
            Years = mean(Years),
            Z_Mean = mean(Z_Mean),
            Avg_Slope = mean(Avg_Slope), 
            #Burned_Acres = mean(ACRES),
            Fire_Event_Area_in_uRCA_km2 = 
              ifelse(is.na(Fire_Event_Area_in_uRCA_km2), 0, sum(Fire_Event_Area_in_uRCA_km2)), 
            BS1_AreaKm2 = ifelse(is.na(BS1_AreaKm2), 0, sum(BS1_AreaKm2)),  
            BS2_AreaKm2 = ifelse(is.na(BS2_AreaKm2), 0, sum(BS2_AreaKm2)),
            BS3_AreaKm2 = ifelse(is.na(BS3_AreaKm2), 0, sum(BS3_AreaKm2)),
            BS4_AreaKm2 = ifelse(is.na(BS4_AreaKm2), 0, sum(BS4_AreaKm2)),
            BS_sum_AreaKm2 = ifelse(is.na(BS_sum_AreaKm2), 0, sum(BS_sum_AreaKm2))) %>% 
 mutate("Beaver_Density" = Beaver_Count/VB_Area_SqKm,
       "Percent_Burned" = (BS_sum_AreaKm2/VB_Area_SqKm)) 







#Do this with the real data
Total_Length <- POP.100HUC.HAB.dat$TOTAL_LENGTH_KM

POP.100HUC.HAB.dat <- cbind(POP.100HUC.HAB.dat, Total_Length)
POP.80HUC.HAB.dat <- cbind(POP.80HUC.HAB.dat, Total_Length)
POP.50HUC.HAB.dat <- cbind(POP.50HUC.HAB.dat, Total_Length)
POP.20HUC.HAB.dat <- cbind(POP.20HUC.HAB.dat, Total_Length)





combined.hab.data <- rbind(
  transform(POP.100HUC.HAB.dat, POP = "POP.100HUC.dat"),
  transform(POP.80HUC.HAB.dat, POP = "POP.80HUC.dat"),
  transform(POP.50HUC.HAB.dat, POP = "POP.50HUC.dat"),
  transform(POP.20HUC.HAB.dat, POP = "POP.20HUC.dat")
)

combined.hab.data <- combined.hab.data[,-c(1,2)]

#Calculate the proportion of habitat reduced
combined.hab.data <- combined.hab.data %>% 
  mutate(percent_available_habitat = (PASS_LENGTH_KM/Total_Length)*100)



combined.hab.data <- left_join(x = combined.hab.data, y = VB.beaver.fire.dat, by = "HUC12")


combined.hab.data <- combined.hab.data %>% filter(Beaver_Density > 0)



# Summary Statistics

stats <- combined.hab.data %>% 
  filter(POP == "POP.100HUC.dat")

min(stats$Beaver_Density) #0.216
max(stats$Beaver_Density) #137.25
mean(stats$Beaver_Density) #23.58
sd(stats$Beaver_Density) #26.49

#Beaver densities
chat_stats <- stats %>% 
  filter(basin == "chat")
mean(chat_stats$Beaver_Density) #33.112

chena_stats <- stats %>% 
  filter(basin == "chen")
mean(chena_stats$Beaver_Density) #30.258

salcha_stats <- stats %>% 
  filter(basin == "salc")
mean(salcha_stats$Beaver_Density) #19.12876

goodpaster_stats <- stats %>% 
  filter(basin == "good")
mean(goodpaster_stats$Beaver_Density) #12.73846

shaw_stats <- stats %>% 
  filter(basin == "shaw")
mean(shaw_stats$Beaver_Density) #9.9967




str(combined.hab.data)

stats.20 <- combined.hab.data %>% 
  filter(POP == "POP.100HUC.dat") 


min(stats.20$percent_available_habitat) #Reduced the available habitat as much as 64.5%!!!!

stats.20[which.min(stats.20$percent_available_habitat),]
min(stats$PASS_LENGTH_KM) #Reduced the available habitat as much as 64.5%!!!!
min(stats.20$PASS_LENGTH_KM) #Reduced the available habitat as much as 64.5%!!!!
View(stats.20)





ggplot(combined.hab.data, aes(x = Beaver_Density, y = PASS_LENGTH_KM))+  
  geom_point(alpha = 0.6, size = 2, col = "darkgrey")+
  geom_smooth(method="lm")+
  scale_y_continuous(breaks=seq(0, 100000, 10000))+
  #scale_x_continuous(breaks=seq(0, 0.1, 0.05))+
  #xlim(0, 0.01) +
  theme_cowplot()


ggplot(combined.hab.data, aes(x = Beaver_Density, y = percent_available_habitat))+  
  geom_point(alpha = 0.6, size = 2, col = "darkgrey")+
  geom_smooth(method="lm")+
  #scale_y_continuous(breaks=seq(0, 100000, 10000))+
  #scale_x_continuous(breaks=seq(0, 0.1, 0.05))+
  #xlim(0, 0.01) +
  theme_cowplot()


#Make sure you only run this function once to get the colors aligned with the plots
combined.hab.data$POP_rate <- factor(combined.hab.data$POP_rate, levels = rev(levels(factor(combined.hab.data$POP_rate))))




ggplot(combined.hab.data, aes(x= Beaver_Density, y=PASS_LENGTH_KM, 
                              col = factor(POP_rate), fill = factor(POP_rate)))+  
  geom_boxplot(width = 0.1, alpha = 0.5)+
  geom_violin(alpha = 0.5)+
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_y_continuous(breaks=seq(0, 100000, 10000))+
  scale_x_continuous(breaks=seq(0, 120, 20))+
  theme_cowplot()+
  theme(legend.position = "bottom")



ggplot(combined.hab.data, aes(x= Beaver_Density, y=percent_available_habitat, 
                              col = factor(POP_rate), fill = factor(POP_rate)))+  
  geom_boxplot(width = 0.1, alpha = 0.5)+
  geom_violin(alpha = 0.5)+
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  #scale_y_continuous(breaks=seq(0, 100000, 10000))+
  scale_x_continuous(breaks=seq(0, 120, 20))+
  theme_cowplot()+
  theme(legend.position = "bottom")






#This plot shows a change in rate (a reduction of habitat availability) as beaver density increases under low POP scenerios
ggplot(combined.hab.data)+  
  geom_point(aes(x= Beaver_Density, y=PASS_LENGTH_KM, col = factor(POP_rate)), alpha = 0.015, size = 3, show.legend = T)+
  #geom_encircle(aes(x = Beaver_Density, y = PASS_LENGTH_KM, fill = POP, color = NA), expand = 0.03, show.legend = FALSE, alpha = 0.15) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  geom_smooth(aes(x=Beaver_Density, y= PASS_LENGTH_KM, group = factor(POP_rate), col = factor(POP_rate)), method="lm", se = F, lwd = 1.5)+
  scale_y_continuous(breaks=seq(0, 100000, 10000))+
  scale_x_continuous(breaks=seq(0, 120, 20))+
  theme_cowplot()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(override.aes = list(alpha = 0.3)))  # Set fixed alpha value for points






p1 <- ggplot(combined.hab.data)+  
  geom_point(aes(x= Beaver_Density, y=percent_available_habitat, col = factor(POP_rate)), alpha = 0.05, size = 3, show.legend = T)+
  #geom_encircle(aes(x = Beaver_Density, y = PASS_LENGTH_KM, fill = POP, color = NA), expand = 0.03, show.legend = FALSE, alpha = 0.15) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  geom_smooth(aes(x=Beaver_Density, y= percent_available_habitat, group = factor(POP_rate), col = factor(POP_rate)), method="lm", se = F, lwd = 1.5)+
  #scale_y_continuous(breaks=seq(0, 100000, 10000))+
  scale_x_continuous(breaks=seq(0, 120, 20))+
  theme_cowplot()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(override.aes = list(alpha = 0.3)))+ # Set fixed alpha value for points
  labs(x = "Beaver Density", y = "Percent Available Habitat", title = "A", fill = "Probability of Passage (%)", color = "Probability of Passage (%)")
p1




#This plot shows the generally shrinking habitat as POP decreases under high beaver density
ggplot(combined.hab.data) + 
  geom_encircle(aes(x = Beaver_Density, y = PASS_LENGTH_KM, fill = factor(POP_rate), color = NA), expand = 0, show.legend = T, alpha = 0.4) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_y_continuous(breaks = seq(0, 100000, 10000)) +
  theme_cowplot() +
  theme(legend.position = "bottom")











p <- ggplot() + 
  geom_point(data = combined.hab.data, aes(x = Beaver_Density, y = PASS_LENGTH_KM, color = POP_rate), fill = NA, alpha = 0.7, size = 2) +
  geom_encircle(data = combined.hab.data, aes(x = Beaver_Density, y = PASS_LENGTH_KM, group = POP_rate, color = POP_rate), fill = NA, expand = 0, show.legend = T, alpha = 0.6, lwd = 5) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_y_continuous(breaks = seq(0, 100000, 10000)) +
  scale_x_continuous(breaks = seq(0, 150, 20)) +
  theme_cowplot() +
  theme(legend.position = "bottom")
p



p<- p+
  geom_polygon(aes(x = c(0,0,50,50), y = c(0,90000,90000, 0)), fill = "lightblue", color = "black", alpha = 0.2) +
  geom_text(aes(x = 25, y = 95000, label = "Core Fish Habitat"), size = 3.2, fontface = "bold")
p

p+
  geom_polygon(aes(x = c(50,50,135,135), y = c(0,90000,90000, 0)), fill = "lightgrey", color = "black", alpha = 0.2) +
  geom_text(aes(x = 95, y = 95000, label = "Beaver-Impacted Fish Habitat"), size = 3.2, fontface = "bold")






# Compare results against wildfire, summarized by HUC12 -------------------

#^^ use the data generated in the previous section

#combined.hab.data <- combined.hab.data %>% filter(Beaver_Density > 0)



ggplot(combined.hab.data, aes(x = Percent_Burned, y = PASS_LENGTH_KM))+  
  geom_point(alpha = 0.6, size = 2, col = "darkgrey")+
  geom_smooth(method="lm")+
  scale_y_continuous(breaks=seq(0, 100000, 10000))+
  #scale_x_continuous(breaks=seq(0, 0.1, 0.05))+
  #xlim(0, 0.01) +
  theme_cowplot()



 #Make sure you only run this function once to get the colors aligned with the plots
combined.hab.data$POP_rate <- factor(combined.hab.data$POP_rate, levels = rev(levels(factor(combined.hab.data$POP_rate))))




ggplot(combined.hab.data, aes(x= Percent_Burned, y=PASS_LENGTH_KM, 
                              col = factor(POP_rate), fill = factor(POP_rate)))+  
  geom_boxplot(width = 0.1, alpha = 0.5)+
  geom_violin(alpha = 0.5)+
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_y_continuous(breaks=seq(0, 100000, 10000))+
  theme_cowplot()+
  theme(legend.position = "bottom")


#This plot shows a change in rate (a reduction of habitat availability) as beaver density increases under low POP scenerios
ggplot(combined.hab.data)+  
  geom_point(aes(x= Percent_Burned, y=PASS_LENGTH_KM, col = factor(POP_rate)), 
             alpha = 0.5, size = 3, show.legend = T)+
  #geom_encircle(aes(x = Percent_Burned, y = PASS_LENGTH_KM, fill = POP, color = NA), expand = 0.03, show.legend = FALSE, alpha = 0.15) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  geom_smooth(aes(x=Beaver_Density, y= PASS_LENGTH_KM, group = factor(POP_rate), col = factor(POP_rate)), method="lm", se = F, lwd = 1.5)+
  #scale_y_continuous(breaks=seq(0, 100000, 10000))+
  scale_x_continuous(limits = c(0,45), breaks=seq(0, 50, 5))+
  theme_cowplot()+
  theme(legend.position = "bottom")




p2 <- ggplot(combined.hab.data)+  
  geom_point(aes(x= Percent_Burned, y=percent_available_habitat, col = POP_rate), 
             alpha = 0.2, size = 3, show.legend = F)+
  #geom_encircle(aes(x = Percent_Burned, y = PASS_LENGTH_KM, fill = POP, color = NA), expand = 0.03, show.legend = FALSE, alpha = 0.15) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  geom_smooth(aes(x=Beaver_Density, y= percent_available_habitat, group = factor(POP_rate), col = POP_rate), method="lm", se = F, lwd = 1.5)+
  scale_y_continuous(limits = c(0,100), breaks=seq(0, 100, 25))+
  scale_x_continuous(limits = c(0,45), breaks=seq(0, 50, 5))+
  theme_cowplot()+
  theme(legend.position = "none")+
  labs(title = "B", x= "Percent Burned", y = "Percent Available Habtiat") #fill = "Probability of Passage (%)", color = "Probability of Passage (%)"

p2






ggplot(combined.hab.data) + 
  geom_encircle(aes(x = ifelse(Percent_Burned >0, Percent_Burned, NA), y = PASS_LENGTH_KM, fill = factor(POP_rate), color = NA), expand = 0, show.legend = T, alpha = 0.4) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_y_continuous(breaks = seq(0, 100000, 10000)) +
  theme_cowplot() +
  theme(legend.position = "bottom")



ggplot() + 
  geom_point(data = combined.hab.data, aes(x = ifelse(Percent_Burned >0, Percent_Burned, NA), y = PASS_LENGTH_KM, color = POP_rate), fill = NA, alpha = 0.7, size = 2) +
  geom_encircle(data = combined.hab.data, aes(x = ifelse(Percent_Burned >0, Percent_Burned, NA), y = PASS_LENGTH_KM, group = POP_rate, color = POP_rate), fill = NA, expand = 0, show.legend = T, alpha = 0.6, lwd = 5) +
  scale_color_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "#E69F00", "darkred")) +
  scale_y_continuous(breaks = seq(0, 100000, 10000)) +
  #scale_x_continuous(breaks = seq(0, 150, 20)) +
  theme_cowplot() +
  theme(legend.position = "bottom")








#Use ANOVA to evaluate differences between POP
#combined_data <- rbind(POP.20HUC.HAB.dat, POP.50HUC.HAB.dat, POP.80HUC.HAB.dat, POP.100HUC.HAB.dat)

combined.hab.data <- combined.hab.data %>% 
  mutate("burn_status" = ifelse(Percent_Burned > 0.5, 2, 1)) %>% 
  mutate("burn_status" = ifelse(Percent_Burned == 0, 0, burn_status)) %>% 
  mutate("Years" = ifelse(Years == 0, 100, Years))
  

fire <- aov(PASS_LENGTH_KM ~ factor(burn_status), data = combined.hab.data)
summary(fire)
TukeyHSD(fire)
#Wildfire appears to have a super significant effect on passage length


fire <- aov(PASS_LENGTH_KM ~ factor(Years), data = combined.hab.data)
summary(fire)
TukeyHSD(fire)
#Time since last burn also seems to have a really significant effect



#Check for differences in certain basins
chat <- aov(PASS_LENGTH_KM ~ factor(burn_status), 
            data = combined.hab.data, subset=(basin=="chat"))
summary(chat) #significant 
TukeyHSD(chat)

chat <- aov(PASS_LENGTH_KM ~ factor(Years), 
            data = combined.hab.data, subset=(basin=="chat"))
summary(chat) #significant 
TukeyHSD(chat)



chen <- aov(PASS_LENGTH_KM ~ factor(burn_status), 
            data = combined.hab.data, subset=(basin=="chen"))
summary(chen)  #significant
TukeyHSD(chen)

chen <- aov(PASS_LENGTH_KM ~ factor(Years), 
            data = combined.hab.data, subset=(basin=="chen"))
summary(chen)  #significant
TukeyHSD(chen)



salc <- aov(PASS_LENGTH_KM ~ factor(burn_status), 
            data = combined.hab.data, subset=(basin=="salc"))
summary(salc) #NOT significant
TukeyHSD(salc)

salc <- aov(PASS_LENGTH_KM ~ factor(Years), 
            data = combined.hab.data, subset=(basin=="salc"))
summary(salc) #significant
TukeyHSD(salc)



shaw <- aov(PASS_LENGTH_KM ~ factor(burn_status), 
            data = combined.hab.data, subset=(basin=="shaw"))
summary(shaw) #significant
TukeyHSD(shaw)

shaw <- aov(PASS_LENGTH_KM ~ factor(Years), 
            data = combined.hab.data, subset=(basin=="shaw"))
summary(shaw) #significant
TukeyHSD(shaw)



good <- aov(PASS_LENGTH_KM ~ factor(burn_status), 
            data = combined.hab.data, subset=(basin=="good"))
summary(good) #NOT significant
TukeyHSD(good)

good <- aov(PASS_LENGTH_KM ~ factor(Years), 
            data = combined.hab.data, subset=(basin=="good"))
summary(good) #significant
TukeyHSD(good)




legend <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


plot <- p1+p2
plot

ggsave(plot= plot,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/POP_Beaverdensity_firepercent.jpeg",
       dpi = 2000, 
       height = 4,
       width = 8,
       units = "in")





#Trying to move the legend to the middle, cant get it to work because of weird package update things. 

#install.packages("ggpubr")
#install.packages("tidyverse")


#library(tidyverse)
#library(ggpubr)

#ggarrange(p1, p2,
          align='h',
          common.legend = T)

#labels=c('A', 'B','C','D')





# Plot --------------------------------------------------------------------




theoretical.data <- data.frame(
  POP_100 = c(rep(100, times = 11)),
  POP_80 = c(100, 80, 64, 51.2, 40.96, 32.76, 26.21, 20.97, 16.77, 13.42, 10.736),
  POP_50  = c(100, 50, 25, 12.5, 6.25, 3.125, 1.56, 0.78, 0.39, 0.19, 0.095),
  POP_20 = c(100, 20, 5, 0, 0.16, 0.032, 0.0064, 0.00128, 0.000256, 0.0000512, 0),
  Beaver_density = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)
theoretical.data


# Calculate the curve values
curve_data <- data.frame(Beaver_density = 0:10, POP_20 = sapply(0:10, manual_curve))

# Generate additional points along the curve for smoother appearance
smoothed_curve_data <- data.frame(Beaver_density = seq(0, 10, length.out = 100))
smoothed_curve_data$POP_20 <- sapply(smoothed_curve_data$Beaver_density, manual_curve)


theo_plot <- ggplot(theoretical.data, aes(x = Beaver_density))+
  geom_smooth(aes(y = POP_100), se = F, size = 1.25)+
  geom_smooth(aes(y = POP_80), se = F, color = "green4", size = 1.25)+
  geom_smooth(aes(y = POP_50), se = F, color = "orange", size = 1.25)+
  #geom_smooth(aes(y = POP_20), method = "loess", se = F, color = "red2", size = 1.25)+
  geom_line(data = smoothed_curve_data, aes(y = POP_20), color = "red2", size = 1.25) +
  labs(x = "Beaver Density", y = "Available Habitat (%)")+
  theme_cowplot()
theo_plot


ggsave(plot= theo_plot,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/theoretical plot.jpeg",
       dpi = 2000, 
       height = 3,
       width = 5,
       units = "in")








#Trying to add a legend
theoretical.data <- data.frame(
  type = c(rep("100", times = 11), rep("80", times = 11), rep("50", times = 11), rep("20", times = 11)),
  POP = c(rep(100, times = 11), 100, 80, 64, 51.2, 40.96, 32.76, 26.21, 20.97, 16.77, 13.42, 10.736, 100, 50, 25, 12.5, 6.25, 3.125, 1.56, 0.78, 0.39, 0.19, 0.095, 100, 20, 5, 0, 0.16, 0.032, 0.0064, 0.00128, 0.000256, 0.0000512, 0),
  Beaver_density = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)
theoretical.data


# Calculate the curve values
#curve_data <- data.frame(Beaver_density = 0:10, POP_20 = sapply(0:10, manual_curve))

# Generate additional points along the curve for smoother appearance
#smoothed_curve_data <- data.frame(Beaver_density = seq(0, 10, length.out = 100))
#smoothed_curve_data$POP_20 <- sapply(smoothed_curve_data$Beaver_density, manual_curve)


legend_plot <- ggplot(theoretical.data, aes(x = Beaver_density, y = POP, group = type, color = type))+
  geom_smooth(se = F, size = 1.25)+
 scale_color_manual(values = c("100" = "blue", "80" = "green4", "50" = "orange", "20" = "red2")) +
  theme_cowplot()+
  theme(legend.position = "bottom", legend.justification = "center")+
  labs(x = "Beaver Density", y = "Available Habitat (%)", color = "Probability of Passage")
  


legend_plot

















