#Fixed effects from top RVB SSN model - variable importance and relationship plots

rm(list=ls(all=TRUE))
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(cowplot)
library(zoo)


setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2")

covs <- read.csv("sitedata.csv",header=T)
burn_pct <- read.csv("vb_hs.clean_corrected.csv",header=T)

model_data <- data.frame(burnpct=burn_pct$burnpct, fire_area=covs$fire_area, fire_hs_sev=covs$hs_4,weath_bui7=covs$BUI_7d_window,
                         veg_flam=covs$flam,veg_olt=covs$OLT,wang_everg_vb=covs$wang_everg_vb,
                         nlcd_decid_vb=covs$nlcd_decid_vb,vb_area=covs$vb_area,
                         twi=covs$twi,stream_densHUC8=covs$stream_densHUC8)


model_data_scaled <- data.frame(scale(model_data))[-1]
model_data_scaled <- data.frame(burnpct=burn_pct$burnpct,model_data_scaled)

fixed_Cp2 <- glm(burnpct~ fire_area+fire_hs_sev+weath_bui7+veg_flam+veg_olt+wang_everg_vb+nlcd_decid_vb+vb_area+
                   twi+stream_densHUC8, data=model_data_scaled)
summary(fixed_Cp2)


#run model on unscaled data for ease of plotting
model_data <- na.aggregate(model_data)

us_fixed_Cp2 <- glm(burnpct~ fire_area+fire_hs_sev+weath_bui7+veg_flam+veg_olt+wang_everg_vb+nlcd_decid_vb+vb_area+
                      twi+stream_densHUC8, data=model_data)
summary(us_fixed_Cp2)

#create vector of variable names
( mod_vars = all.vars( formula(us_fixed_Cp2) )[-1] )

###function to create prediction datasets
preddat_fun = function(data, allvars, var) {
  sums = summarise_at(data, 
                      vars( one_of(allvars), -one_of(var) ), 
                      median) 
  cbind( select_at(data, var), sums)
}

pred_dats = mod_vars %>%
  set_names() %>%
  map( ~preddat_fun(model_data, mod_vars, .x) )


preds = pred_dats %>%
  map(~augment(us_fixed_Cp2, newdata = .x, se_fit = TRUE) ) %>%
  map(~mutate(.x, 
              lower = .fitted - 2*.se.fit,
              upper = .fitted + 2*.se.fit))



pred_plot = function(data, variable, xlab) {
  ggplot(data, aes(x = .data[[variable]], y = .fitted) ) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
    theme_bw(base_size = 14) +
    labs(x = xlab,
         y = "RVB % Burned")
  #ylim(-10,30)
}


xlabs = c("Fire area (km^2)","Hillslope burn - high severity (%)",
          "BUI-7 day","Flammability index","Organic layer thickness (m)","Evergreen (%)",
          "Deciduous (%)","RVB area (km^2)", "Topographic wetness index", "Stream density (m/sq-km)")

#this lets you check out individual plots
pred_plot(preds[[1]], mod_vars[1], xlabs[1])

##put all plots together
all_plots = pmap( list(preds, mod_vars, xlabs), pred_plot)
all_plots

#plot in a grid format
cowplot::plot_grid(plotlist = all_plots,
                   labels = "AUTO",
                   align = "hv")
