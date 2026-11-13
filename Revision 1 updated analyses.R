################################################################################
##################### Revisions (Ecosphere): updated analyses #################
################################# Will Samuel ##################################
################################################################################
#
#Everything I corrected or re-ran for the Ecosphere revision, in one script.
#
#The original scripts are left as they were so I can compare:
#  "Beaver-fire regression FINAL.R" - original models, Table 2, Appendix S4, Figure 4
#  "Beaver_Grayling_Theroetical_Modeling_Analysis.R" - original Figure 5
#Input data are only read, never overwritten. New outputs go to:
#  "Output Data Files/Revision 1/" (tables, summary stats)
#  "Figures/Revision 1/"           (Figures 4 and 5, Appendix S2 Figure S1)
#Replaces "Figure 5 corrected (Revision 1).R" (same Figure 5 code is in Part 5).
#
#Parts:
#  0  Original (first submission) numbers, to see where the errors came from
#  1  Data corrections and summary stats (Results para. 1)
#  2  Model selection (Table 2)
#  3  Model averaging (Appendix S4)
#  4  Figure 4
#  5  Figure 5, tributary pond densities, and Table 3
#  6  Table 2 and Appendix S4 redone with AICcmodavg
#  7  Sensitivity runs
#  8  Appendix S2 grayling thresholds (Table S1, Figure S1)
#
#What changed:
#1a. Nine valley bottoms (uRCA) were in VB_Y_sums_full_EDITED.csv twice: the VB plus a
#    ~17-25 m2 sliver RCA with the same uRCA. Attributes, pond counts, and covariates are
#    identical between the two rows, so each VB is kept once (186 VBs, 414 HC ponds;
#    was 195 and 435).
#1b. Burned/unburned uses the same pond-based definition as before: a VB is burned if it
#    has a time since last burn (its ponds fell inside a fire perimeter older than the 2017
#    imagery). Fires after the imagery (negative TSB) are unburned, same as before.
#1c. uRCA 2231 burned in 2015 (ignition 7 May 2015) but its TSB was coded -1, so it was
#    "unburned". Set to 2 (2017 - 2015) and kept its fire covariates.
#1d. Burn severity is now the % of the MTBS-classified area in each class (sums to 100%).
#    Before, I divided class area by VB area, which went over 100% in some VBs. Since the
#    four classes sum to 100%, percent no burn (MTBS "unburned to low") is the reference
#    class and isn't its own covariate.
#2.  All candidate models in a dataset are fit to the SAME VBs (complete cases for every
#    covariate in that dataset's candidate set) so AICc is comparable. Originally the fire
#    models were fit to 52 rows and the others to up to 194. Fire covariates only exist for
#    burned VBs, so the fire models are only in the burned dataset; the all-VB and unburned
#    datasets use the non-fire models.
#    Fit is R2 and adjusted R2. For these Gaussian GLMs, Cox and Snell, Nagelkerke, and
#    Efron pseudo R2 all equal R2, and McFadden depends on the units of the response (it
#    pulled down the averaged pseudo R2 in the first submission), so pseudo R2 is dropped.
#    The four pseudo R2s are still saved as extra columns in
#    "Table 2 - model selection (all models).csv".
#    Dropped the dredge model (dredge.vb): it wasn't in the manuscript and its covariates
#    were picked from the same data.
#3.  Model averaging: models with AICc weight >= 0.05 in each dataset, same math as
#    AICcmodavg::modavg (weights renormalized among the models with the parameter, revised
#    unconditional SE, 90% unconditional CI).
#4.  Figure 4: model-averaged predictions (burned VBs) for every covariate in the highest-R2
#    top model (Geomorphology, Hydrology, Fire) plus the important vegetation covariates.
#    Each panel averages the top models with that covariate, others held at their medians.
#    Important covariates (90% CI excludes zero) are blue with *.
################################################################################

library(dplyr)     #data organization
library(ggplot2)   #plotting
library(cowplot)   #Figure 4 panels
library(patchwork) #Figure 5 panels

#Output folders for the new tables and figures
tab_dir <- "Output Data Files/Revision 1"
fig_dir <- "Figures/Revision 1"
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

#Has to run from the project folder
if (!file.exists("Output Data Files/VB_Y_sums_full_EDITED.csv"))
  stop("Open Beaver_Fire_Fish_Public.Rproj (or setwd() to the project folder) before running this script.")

#AICc, same formula as AICcmodavg (K = logLik df)
aicc <- function(m) {
  ll <- logLik(m); K <- attr(ll, "df"); n <- nobs(m)
  -2 * as.numeric(ll) + 2 * K + 2 * K * (K + 1) / (n - K - 1)
}


# Part 0. Original (first submission) numbers ----------------------------------
#Same data and cleaning as "Beaver-fire regression FINAL.R" (lines 41-55)
orig <- read.csv("Output Data Files/VB_Y_sums_full_EDITED.csv")
orig_neg <- !is.na(orig$Time_Since_Burn_min) & orig$Time_Since_Burn_min < 0
for (v in c("Fire_Event_Area_Percent_of_uRCA", "BS1_AreaKm2", "BS2_AreaKm2", "BS3_AreaKm2", "BS4_AreaKm2"))
  orig[[v]][orig_neg] <- NA
orig$Time_Since_Burn_min[orig_neg] <- NA
orig$FP_WIDTH_mean[orig$FP_WIDTH_mean == -9999] <- NA

#Original covariate groups
o_geo  <- c("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "SINUOSITY_mean", "STRM_ORDER_max",
            "WIDTH_M_mean", "DEPTH_M_mean", "ELEV_M_mean")
o_hyd  <- c("StrmPow_mean", "MEANANNCMS_mean")
o_fire <- c("Time_Since_Burn_min", "Fire_Event_Area_Percent_of_uRCA", "percent_severe_burn",
            "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")
o_veg  <- c("percent_woody_cover", "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN", "DeciduousShrub_2015_MEAN",
            "EvergreenShrub_2015_MEAN", "Forb_2015_MEAN", "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN")
#GLM of pond density on a set of covariates
o_fit <- function(x, d) glm(as.formula(paste("dams_per_km2 ~", paste(x, collapse = " + "))), data = d)
#Pseudo R2 the way I originally calculated it: rcompanion::nagelkerke() refits the null
#model to ALL rows of the data frame, even when the model dropped rows with missing
#covariates, so the values were inflated
pseudo_r2_as_reported <- function(m, d) {
  n <- nobs(m); ll <- as.numeric(logLik(m)); ll0 <- as.numeric(logLik(glm(dams_per_km2 ~ 1, data = d)))
  mcf <- 1 - ll / ll0; cs <- 1 - exp(-2 / n * (ll - ll0)); nk <- cs / (1 - exp(2 / n * ll0))
  y <- model.response(model.frame(m)); ef <- 1 - sum((y - fitted(m))^2) / sum((y - mean(y))^2)
  mean(c(mcf, cs, nk, ef))
}
#Burned VBs in the original data
orig_burned <- orig[!is.na(orig$Time_Since_Burn_min), ]
#Three of the original models
o1 <- o_fit(c(o_geo, o_hyd, o_fire), orig)          # "All catchments" Geo/Hydro/Fire
o2 <- o_fit(c(o_geo, o_fire), orig)                 # "All catchments" Geo/Fire
o3 <- o_fit(c(o_geo, o_hyd), orig)                  # Geo/Hydro (no fire)
#glm drops rows with NAs, so each of these was fit to a different number of rows
cat("\nPart 0. Original analysis (first submission):\n")
cat("  Geo/Hydro/Fire n =", nobs(o1), "| Geo/Fire n =", nobs(o2), "| Geo/Hydro n =", nobs(o3),
    "| valley-bottom rows =", nrow(orig), "(includes 9 duplicates)\n")
cat("  AICc from models fit to different data cannot be compared; fewer rows -> lower AICc.\n")
cat("  AICc: Geo/Hydro/Fire", round(aicc(o1), 1), "| Geo/Fire", round(aicc(o2), 1), "\n")
cat("  Pseudo R2 as reported: Geo/Hydro/Fire, all", round(pseudo_r2_as_reported(o1, orig), 2),
    "| burned", round(pseudo_r2_as_reported(o1, orig_burned), 2),
    "| fire only", round(pseudo_r2_as_reported(o_fit(o_fire, orig), orig), 2),
    "| geo/hydro", round(pseudo_r2_as_reported(o3, orig), 2),
    "| vegetation", round(pseudo_r2_as_reported(o_fit(o_veg, orig), orig), 2), "\n")
cat("  Same Geo/Hydro/Fire model, null fit to the SAME 52 rows instead: McFadden =",
    round(1 - as.numeric(logLik(o1)) / as.numeric(logLik(glm(dams_per_km2 ~ 1, data = model.frame(o1)))), 2), "\n")

#Original Figure 5 tributary densities (Beaver_Grayling_Theroetical_Modeling_Analysis.R,
#lines 710-731). I divided the summed ponds in a tributary by the MEAN area of ONE valley
#bottom. That summarize() also returned one row per VB instead of one per tributary
#(because of the ifelse(is.na(...)) line). Newer dplyr won't allow that, so it's rebuilt
#with mutate() here, which still gives one row per VB and the same numbers.
orig5 <- read.csv("Theoretical Passage Data Files/VB_with_beaver_fire_data.csv") %>%
  group_by(HUC12) %>%
  mutate(VB_Area_SqKm = mean(RCA_Area_SqKm),         # mean area of ONE valley bottom (the error)
         Beaver_Count = sum(Point_Count),            # all ponds in the tributary
         BS_sum_AreaKm2 = ifelse(is.na(BS_sum_AreaKm2), 0, sum(BS_sum_AreaKm2))) %>%
  ungroup() %>%
  select(HUC12, VB_Area_SqKm, Beaver_Count, BS_sum_AreaKm2) %>%
  mutate(Beaver_Density = Beaver_Count / VB_Area_SqKm)
#Join to the habitat simulation output (one row per tributary) like the original did.
#merge() is base R, so this works the same in any dplyr version.
orig5 <- merge(read.csv("Theoretical Passage Data Files/POP.100HUC.HAB.dat.csv"), orig5, by = "HUC12", all.x = TRUE)
orig5 <- orig5[!is.na(orig5$Beaver_Density) & orig5$Beaver_Density > 0, ]
cat("  Original Figure 5:", nrow(orig5), "rows for ~133 tributaries | density min",
    round(min(orig5$Beaver_Density), 1), "max", round(max(orig5$Beaver_Density), 2),
    "mean", round(mean(orig5$Beaver_Density), 1),
    "| Chatanika mean", round(mean(orig5$Beaver_Density[orig5$basin == "chat"]), 1), "\n")


# Part 1. Data corrections and summary statistics ------------------------------

#Start over from the raw model data
vb_raw <- read.csv("Output Data Files/VB_Y_sums_full_EDITED.csv")

# 1a. One row per valley bottom (drop the sliver duplicate = smaller RCA area)
# Sort so the real valley bottom comes before its sliver duplicate...
vb <- vb_raw[order(vb_raw$uRCA, -vb_raw$Area_km2_RCA), ]
# ...then keep one row per valley bottom (195 -> 186)
vb <- vb[!duplicated(vb$uRCA), ]
# Restore the original row order
vb <- vb[order(vb$X.2), ]

# 1c. uRCA 2231 burned in 2015, two years before the 2017 imagery
vb$Time_Since_Burn_min[vb$uRCA == 2231] <- 2

# 1b. Fires after the 2017 imagery -> unburned (as in the original script)
post2017 <- !is.na(vb$Time_Since_Burn_min) & vb$Time_Since_Burn_min < 0
fire_cols <- c("Fire_Event_Area_Percent_of_uRCA", "BS1_AreaKm2", "BS2_AreaKm2",
               "BS3_AreaKm2", "BS4_AreaKm2", "DNBR_MEAN", "RDNBR_MEAN")
for (v in fire_cols) vb[[v]][post2017] <- NA
vb$Time_Since_Burn_min[post2017] <- NA
# Missing-value code -9999 -> NA
vb$FP_WIDTH_mean[vb$FP_WIDTH_mean == -9999] <- NA
# Burned = has a time since last burn
vb$burned <- !is.na(vb$Time_Since_Burn_min)

# 1d. Burn severity as % of the MTBS-classified area (classes sum to 100%).
#     Valley bottoms with no classified pixels get NA (no severity data).
# Total MTBS-classified area per valley bottom
class_total <- with(vb, BS1_AreaKm2 + BS2_AreaKm2 + BS3_AreaKm2 + BS4_AreaKm2)
class_total[!is.na(class_total) & class_total == 0] <- NA
# Each severity class as % of the classified area (the four add to 100%)
vb$percent_severe_burn   <- 100 * vb$BS4_AreaKm2 / class_total
vb$percent_moderate_burn <- 100 * vb$BS3_AreaKm2 / class_total
vb$percent_mild_burn     <- 100 * vb$BS2_AreaKm2 / class_total
vb$percent_no_burn       <- 100 * vb$BS1_AreaKm2 / class_total   # reference class
# Severity is only used for burned valley bottoms
for (v in c("percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn"))
  # No severity values for unburned valley bottoms
  vb[[v]][!vb$burned] <- NA

# Split into burned (87) and unburned (99)
burned_VB   <- vb[vb$burned, ]
unburned_VB <- vb[!vb$burned, ]

# Summary statistics -----------------------------------------------------------
# Collect summary statistics into one table
stat_rows <- list()
add_stat <- function(name, value) stat_rows[[length(stat_rows) + 1]] <<- data.frame(statistic = name, value = value)
#Means, SDs, and a Welch t-test, burned vs unburned
ttest_stats <- function(var, label) {
  t <- t.test(burned_VB[[var]], unburned_VB[[var]])
  add_stat(paste(label, "burned mean"), mean(burned_VB[[var]], na.rm = TRUE))
  add_stat(paste(label, "burned SD"), sd(burned_VB[[var]], na.rm = TRUE))
  add_stat(paste(label, "unburned mean"), mean(unburned_VB[[var]], na.rm = TRUE))
  add_stat(paste(label, "unburned SD"), sd(unburned_VB[[var]], na.rm = TRUE))
  add_stat(paste(label, "t-test T"), unname(t$statistic))
  add_stat(paste(label, "t-test DF"), unname(t$parameter))
  add_stat(paste(label, "t-test p"), t$p.value)
}
# Counts and overall means
add_stat("Rows in input file", nrow(vb_raw))
add_stat("Valley bottoms with HC ponds", nrow(vb))
add_stat("HC ponds", sum(vb$Dams_Per_Basin))
add_stat("Burned valley bottoms", nrow(burned_VB))
add_stat("Unburned valley bottoms", nrow(unburned_VB))
add_stat("Pond density mean (ponds/km2)", mean(vb$dams_per_km2))
add_stat("Pond density SD", sd(vb$dams_per_km2))
add_stat("Pond area mean (m2)", mean(vb$Pond_Area_m2_mean))
add_stat("Pond area SD", sd(vb$Pond_Area_m2_mean))
# Burned vs unburned comparisons (Results paragraph 1)
ttest_stats("Area_km2_VB", "Valley bottom area (km2)")
ttest_stats("dams_per_km2", "Pond density (ponds/km2)")
ttest_stats("Pond_Area_m2_mean", "Pond area (m2)")
# Fire timing (Results and Discussion)
add_stat("Time since burn mean (yr)", mean(burned_VB$Time_Since_Burn_min))
add_stat("Time since burn SD", sd(burned_VB$Time_Since_Burn_min))
add_stat("Percent of burned VBs burned in 2004 (TSB = 13)", 100 * mean(burned_VB$Time_Since_Burn_min == 13))
add_stat("Percent of burned VBs burned since 2003 (TSB <= 14)", 100 * mean(burned_VB$Time_Since_Burn_min <= 14))
# Burned valley bottoms with severity data (49)
sev <- burned_VB[!is.na(burned_VB$percent_severe_burn), ]
add_stat("Burned VBs with burn severity data", nrow(sev))
for (v in c("percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")) {
  add_stat(paste(v, "mean (% of classified area)"), mean(sev[[v]]))
  add_stat(paste(v, "SD"), sd(sev[[v]]))
  add_stat(paste(v, "max"), max(sev[[v]]))
}
# Vegetation, burned vs unburned
for (v in c("percent_woody_cover", "BroadleafTree_2015_MEAN", "ConiferTree_2015_MEAN",
            "DeciduousShrub_2015_MEAN", "EvergreenShrub_2015_MEAN"))
  ttest_stats(v, v)
# Stack, print and save the statistics and the corrected data
summary_stats <- do.call(rbind, stat_rows)
print(summary_stats, row.names = FALSE)
write.csv(summary_stats, file.path(tab_dir, "Summary statistics (Results).csv"), row.names = FALSE)
write.csv(vb, file.path(tab_dir, "Model data - corrected valley bottoms.csv"), row.names = FALSE)

#Discussion numbers: fire timing for the 49 burned VBs with severity data (the ones in
#the burned models), and severe burn vs deciduous shrub
cat("\nDiscussion: severity-data VBs last burned", 2017 - max(sev$Time_Since_Burn_min), "-",
    2017 - min(sev$Time_Since_Burn_min), "|", round(100 * mean(sev$Time_Since_Burn_min == 13)),
    "% in 2004 | r(% severe burn, deciduous shrub) =",
    round(cor(sev$percent_severe_burn, sev$DeciduousShrub_2015_MEAN), 2), "\n")
cat(sprintf("  (burned >= 10 years before imagery: %.1f%% of all burned VBs; %.1f%% of those with severity data)\n",
            100 * mean(burned_VB$Time_Since_Burn_min >= 10), 100 * mean(sev$Time_Since_Burn_min >= 10)))


# Part 2. Model selection on common rows (Table 2) -----------------------------

# Covariate groups (Table 1); percent no burn left out as the reference class
geo  <- c("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "SINUOSITY_mean",
          "STRM_ORDER_max", "WIDTH_M_mean", "DEPTH_M_mean", "ELEV_M_mean")
hyd  <- c("StrmPow_mean", "MEANANNCMS_mean")
fire <- c("Time_Since_Burn_min", "Fire_Event_Area_Percent_of_uRCA",
          "percent_severe_burn", "percent_moderate_burn", "percent_mild_burn")
veg  <- c("percent_woody_cover", "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN",
          "DeciduousShrub_2015_MEAN", "EvergreenShrub_2015_MEAN", "Forb_2015_MEAN",
          "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN")

# The 11 candidate models from the original script (dredge model dropped)
cand_all <- list(
  global.vb.reduced = c(geo, hyd, fire, veg),
  netmap.vb         = c(geo, hyd),
  fire.vb           = fire,
  veg.vb            = veg,
  netmap.fire.vb    = c(geo, hyd, fire),
  veg.fire.vb       = c(fire, veg),
  netmap.veg.vb     = c(geo, hyd, veg),
  geo.fire.vb       = c(geo, fire),
  hydro.fire.vb     = c(hyd, fire),
  geo.veg.vb        = c(geo, veg),
  hydro.veg.vb      = c(hyd, veg))
# Models without fire covariates (fire values do not exist for unburned valley bottoms)
cand_nofire <- cand_all[c("netmap.vb", "veg.vb", "netmap.veg.vb", "geo.veg.vb", "hydro.veg.vb")]

# Names used in Table 2
set_labels <- c(global.vb.reduced = "Global", netmap.vb = "Geomorphology, Hydrology",
                fire.vb = "Fire", veg.vb = "Vegetation",
                netmap.fire.vb = "Geomorphology, Hydrology, Fire", veg.fire.vb = "Fire, Vegetation",
                netmap.veg.vb = "Geomorphology, Hydrology, Vegetation", geo.fire.vb = "Geomorphology, Fire",
                hydro.fire.vb = "Hydrology, Fire", geo.veg.vb = "Geomorphology, Vegetation",
                hydro.veg.vb = "Hydrology, Vegetation")

#Four pseudo R2s and their mean, null model fit to the SAME rows
#(only saved as extra columns; the manuscript reports R2 and adjusted R2)
pseudo_r2 <- function(m) {                     # same four as rcompanion::nagelkerke + Efron
  mf <- model.frame(m); n <- nobs(m)
  ll  <- as.numeric(logLik(m))
  ll0 <- as.numeric(logLik(glm(dams_per_km2 ~ 1, data = mf)))   # null fit to the SAME rows
  mcf <- 1 - ll / ll0
  cs  <- 1 - exp(-2 / n * (ll - ll0))
  nk  <- cs / (1 - exp(2 / n * ll0))
  y   <- model.response(mf)
  ef  <- 1 - sum((y - fitted(m))^2) / sum((y - mean(y))^2)
  c(McFadden = mcf, CoxSnell = cs, Nagelkerke = nk, Efron = ef, Mean = mean(c(mcf, cs, nk, ef)))
}
#R2 and adjusted R2 (k = number of covariates, not counting the intercept)
r2_adj <- function(m) {
  y <- model.response(model.frame(m)); n <- length(y); k <- length(coef(m)) - 1
  r2 <- 1 - sum((y - fitted(m))^2) / sum((y - mean(y))^2)
  c(R2 = r2, adj_R2 = 1 - (1 - r2) * (n - 1) / (n - k - 1))
}
#Keep rows complete for every covariate in the set, then fit all the models to those rows
fit_set <- function(dat, cand) {
  vars <- unique(unlist(cand))
  common <- dat[complete.cases(dat[, c("dams_per_km2", vars)]), ]
  fits <- lapply(cand, function(x)
    glm(as.formula(paste("dams_per_km2 ~", paste(x, collapse = " + "))), data = common))
  list(data = common, fits = fits)
}
#AICc, delta AICc, and weights, best model first
selection_table <- function(fs, dataset) {
  out <- do.call(rbind, lapply(names(fs$fits), function(nm) {
    m <- fs$fits[[nm]]; p <- pseudo_r2(m); r <- r2_adj(m)
    data.frame(dataset = dataset, model = nm, covariate_set = unname(set_labels[nm]),
               n = nobs(m), K = attr(logLik(m), "df"), AICc = aicc(m),
               R2 = r["R2"], adj_R2 = r["adj_R2"], pseudo_R2 = p["Mean"], McFadden = p["McFadden"], CoxSnell = p["CoxSnell"],
               Nagelkerke = p["Nagelkerke"], Efron = p["Efron"], row.names = NULL)
  }))
  out <- out[order(out$AICc), ]
  out$delta_AICc <- out$AICc - min(out$AICc)
  out$weight <- exp(-0.5 * out$delta_AICc) / sum(exp(-0.5 * out$delta_AICc))
  out
}

# Fit the three datasets: all (179 rows), burned (49), unburned (98)
sets <- list(
  "All catchments"      = fit_set(vb,          cand_nofire),
  "Burned catchments"   = fit_set(burned_VB,   cand_all),
  "Unburned catchments" = fit_set(unburned_VB, cand_nofire))

# Full model-selection table, printed and saved
table2 <- do.call(rbind, lapply(names(sets), function(s) selection_table(sets[[s]], s)))
print(table2 %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
write.csv(table2, file.path(tab_dir, "Table 2 - model selection (all models).csv"), row.names = FALSE)

# Combined AICc weight of the burned-dataset models that include wildfire covariates
fire_models <- names(cand_all)[sapply(cand_all, function(x) any(x %in% fire))]
cat("Combined AICc weight of models with wildfire covariates (burned):",
    round(sum(table2$weight[table2$dataset == "Burned catchments" & table2$model %in% fire_models]), 3), "\n")

# Table 2 as presented: models with weight >= 0.01, plus the global model
table2_ms <- table2 %>%
  filter(weight >= 0.01 | model == "global.vb.reduced") %>%
  transmute(dataset, covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
            weight = round(weight, 2), K, n, model_averaging = weight >= 0.05)
print(table2_ms, row.names = FALSE)
write.csv(table2_ms, file.path(tab_dir, "Table 2 - manuscript version.csv"), row.names = FALSE)


# Part 3. Model averaging (Appendix S4) ----------------------------------------

#Weighted average of one covariate across the top models, with 90% CI (same as AICcmodavg::modavg)
modavg_param <- function(fits, parm, level = 0.90) {
  # Which models contain this covariate
  has <- sapply(fits, function(m) parm %in% names(coef(m)) && !is.na(coef(m)[parm]))
  if (!any(has)) return(NULL)
  f  <- fits[has]
  # AICc weights among those models
  a  <- sapply(f, aicc); w <- exp(-0.5 * (a - min(a))); w <- w / sum(w)
  b  <- sapply(f, function(m) unname(coef(m)[parm]))
  se <- sapply(f, function(m) unname(sqrt(diag(vcov(m)))[parm]))
  # Weighted average estimate
  est <- sum(w * b)
  use <- sqrt(sum(w * (se^2 + (b - est)^2)))       # "revised" unconditional SE
  z   <- qnorm(1 - (1 - level) / 2)
  data.frame(parameter = parm, estimate = est, uncond_SE = use,
             lower_90 = est - z * use, upper_90 = est + z * use, n_models = sum(has))
}
#Top models = AICc weight >= 0.05
top_models <- function(fs, dataset) {
  st <- selection_table(fs, dataset)
  fs$fits[st$model[st$weight >= 0.05]]
}
# Average every covariate in every dataset (Appendix S4)
appendix <- do.call(rbind, lapply(names(sets), function(s) {
  top <- top_models(sets[[s]], s)
  parms <- unique(unlist(lapply(top, function(m) names(coef(m))[-1])))
  res <- do.call(rbind, lapply(parms, function(p) modavg_param(top, p)))
  res$dataset <- s
  res$top_models <- paste(names(top), collapse = ", ")
  res
}))
# Important = 90% interval does not cross zero (bold in Appendix S4)
appendix$important <- appendix$lower_90 > 0 | appendix$upper_90 < 0
appendix <- appendix[, c("dataset", "parameter", "estimate", "uncond_SE", "lower_90",
                         "upper_90", "important", "n_models", "top_models")]
print(appendix %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
write.csv(appendix, file.path(tab_dir, "Appendix S4 - model-averaged estimates.csv"), row.names = FALSE)


#Variance inflation factors (Methods: all VIF < 10)
#VIF > 10 = strong overlap with the other covariates
vif_check <- function(dat, vars) {
  d <- dat[complete.cases(dat[, vars]), vars]
  sapply(vars, function(v) {
    r2 <- summary(lm(as.formula(paste(v, "~", paste(setdiff(vars, v), collapse = " + "))), data = d))$r.squared
    1 / (1 - r2)
  })
}
# VIF for the burned and all-valley-bottom covariates, saved to a CSV
vifs <- rbind(
  data.frame(dataset = "Burned catchments (global model covariates)", parameter = c(geo, hyd, fire, veg),
             VIF = vif_check(sets[["Burned catchments"]]$data, c(geo, hyd, fire, veg))),
  data.frame(dataset = "All catchments (non-wildfire covariates)", parameter = c(geo, hyd, veg),
             VIF = vif_check(sets[["All catchments"]]$data, c(geo, hyd, veg))))
print(vifs, row.names = FALSE)
write.csv(vifs, file.path(tab_dir, "VIF check.csv"), row.names = FALSE)


# Part 4. Figure 4: model-averaged predictions, burned valley bottoms ----------

# Top burned models and their weights
burned_set <- sets[["Burned catchments"]]
burned_top <- top_models(burned_set, "Burned catchments")
w_top <- sapply(burned_top, aicc); w_top <- exp(-0.5 * (w_top - min(w_top))); w_top <- w_top / sum(w_top)
# Median of every covariate (others are held at these in each panel)
all_vars <- unique(unlist(cand_all[names(burned_top)]))
medians  <- sapply(burned_set$data[, all_vars], median)

#Predicted pond density along one covariate, averaged over the top models that contain it
#(weights renormalized, same as the Appendix S4 averages), with a 90% band
modavg_pred <- function(var, x, level = 0.90) {   # like AICcmodavg::modavgPred, conditional on var
  has <- sapply(burned_top, function(m) var %in% names(coef(m)))
  mods <- burned_top[has]; w <- w_top[has] / sum(w_top[has])
  nd <- as.data.frame(t(medians))[rep(1, length(x)), , drop = FALSE]
  nd[[var]] <- x
  p  <- lapply(mods, function(m) predict(m, newdata = nd, se.fit = TRUE))
  fit <- sapply(p, `[[`, "fit"); se <- sapply(p, `[[`, "se.fit")
  if (is.null(dim(fit))) { fit <- matrix(fit, nrow = length(x)); se <- matrix(se, nrow = length(x)) }
  avg <- as.vector(fit %*% w)
  use <- sqrt(as.vector((se^2 + (fit - avg)^2) %*% w))
  z <- qnorm(1 - (1 - level) / 2)
  data.frame(x = x, fit = avg, lower = avg - z * use, upper = avg + z * use)
}

# Important burned covariates (90% CI excludes zero; drawn in blue with *)
fig4_imp <- appendix %>% filter(dataset == "Burned catchments", important) %>% pull(parameter)
# Panels: every covariate in the highest-R2 top model, then any important covariate not in it
best_r2  <- names(burned_top)[which.max(sapply(burned_top, function(m) r2_adj(m)["R2"]))]
fig4_vars <- c(cand_all[[best_r2]], setdiff(fig4_imp, cand_all[[best_r2]]))
# Axis labels
fig4_labels <- c(GRADIENT_mean = "Gradient", FP_WIDTH_mean = "Floodplain Width (m)",
                 Area_km2_VB = "Valley Bottom Area (km\u00b2)", SINUOSITY_mean = "Sinuosity",
                 STRM_ORDER_max = "Stream Order", WIDTH_M_mean = "Stream Width (m)",
                 DEPTH_M_mean = "Stream Depth (m)", ELEV_M_mean = "Elevation (m)",
                 StrmPow_mean = "Stream Power", MEANANNCMS_mean = "Mean Annual Discharge (m\u00b3/s)",
                 Time_Since_Burn_min = "Years Since Last Burn",
                 Fire_Event_Area_Percent_of_uRCA = "Percent Burned",
                 percent_severe_burn = "Percent Severe Burn", percent_moderate_burn = "Percent Moderate Burn",
                 percent_mild_burn = "Percent Mild Burn", percent_woody_cover = "Percent Woody Cover",
                 ConiferTree_2015_MEAN = "Percent Conifer Tree", BroadleafTree_2015_MEAN = "Percent Broadleaf Tree",
                 DeciduousShrub_2015_MEAN = "Percent Deciduous Shrub",
                 EvergreenShrub_2015_MEAN = "Percent Evergreen Shrub", Forb_2015_MEAN = "Percent Forb",
                 Graminoid_2015_MEAN = "Percent Graminoid", tmLichenLight_2015_MEAN = "Percent Lichen")

# Draw each panel (original style): 90% band (blue if important, grey if not) and
# points = model-averaged predictions at each valley bottom's observed value (tied values overlap),
# and rug ticks along the x-axis mark the observed values
fig4_panels <- lapply(fig4_vars, function(v) {
  obs  <- burned_set$data[[v]]
  band <- modavg_pred(v, seq(min(obs), max(obs), length.out = 100))
  pts  <- modavg_pred(v, obs)
  imp  <- v %in% fig4_imp
  ggplot(band, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = if (imp) "#3B7DD8" else "grey50", alpha = 0.35) +
    geom_point(data = pts, aes(x = x, y = fit), alpha = 0.6, size = 1.6) +
    geom_rug(data = data.frame(x = obs), aes(x = x), inherit.aes = FALSE,
             sides = "b", alpha = 0.5, length = unit(0.04, "npc")) +   # ticks = observed values
    theme_bw(base_size = 17) +
    theme(axis.text = element_text(colour = "black", size = 13),
          axis.title = element_text(colour = "black", size = 15.5),
          aspect.ratio = 1,                          # square panels
          plot.margin = margin(34, 14, 6, 6)) +      # top: room for panel letters
    labs(x = paste0(fig4_labels[[v]], if (imp) " *" else ""), y = NULL)   # y-axis title is shared (Reviewer 2)
})
# Arrange panels (4 per row, labels (a)-(q) as ESA asks) with one shared y-axis label, then save.
# Saved 14 in wide; at ESA's 18-cm maximum width all text is 6.6-9.6 pt (ESA: 6-10 pt)
fig4_grid <- cowplot::plot_grid(plotlist = fig4_panels, labels = paste0("(", letters[seq_along(fig4_panels)], ")"),
                                label_size = 18, align = "hv", ncol = 4)
fig4_ylab <- cowplot::ggdraw() +
  cowplot::draw_label("Pond Density (ponds/km\u00b2)", angle = 90, size = 19)
fig4 <- cowplot::plot_grid(fig4_ylab, fig4_grid, ncol = 2, rel_widths = c(0.03, 1))
fig4
ggsave(file.path(fig_dir, "Figure 4 (Revision 1).jpeg"), fig4,
       width = 14, height = 17.5, units = "in", dpi = 600, bg = "white")


# Part 5. Figure 5 and tributary pond densities --------------------------------
#Fixes to the original Figure 5 code:
#  1. Beaver density per tributary (HUC12) = total ponds / total valley-bottom area in the
#     tributary. Originally I divided the summed ponds by the MEAN area of one valley
#     bottom (summarize(VB_Area_SqKm = mean(...))).
#  2. Percent burned per tributary = 100 x burned VB area / total VB area. Burned area =
#     MTBS low + moderate + high severity (BS2 + BS3 + BS4), capped at each VB's area,
#     fires that ignited in or before 2017 only.
#  3. Panel B trend lines are fit to percent burned (the original p2 geom_smooth() used
#     x = Beaver_Density).
#  4. Panel B shows burned tributaries only.
#  5. VBs stored as several polygon pieces (same uRCA) are combined: piece areas and pond
#     counts are summed; fire attributes (joined by uRCA and repeated on each piece) are
#     counted once. The original summary also had several rows per tributary, so its
#     summary stats were weighted by the number of VBs.
#  6. Fixed the axis label typo ("Habitat"); both panels use 10-unit y-axis steps.
#Limitation (same as before): VBs that straddle two tributaries are counted in full in each.

# Read the fish-passage simulation output for each passage rate
read_pop <- function(f) {
  x <- read.csv(file.path("Theoretical Passage Data Files", f))
  x[, c("HUC12", "POP_rate", "TOTAL_LENGTH_KM", "PASS_LENGTH_KM", "basin")]
}
pop <- bind_rows(read_pop("POP.100HUC.HAB.dat.csv"),
                 read_pop("POP.80HUC.HAB.dat.csv"),
                 read_pop("POP.50HUC.HAB.dat.csv"),
                 read_pop("POP.20HUC.HAB.dat.csv"))
# Percent of habitat still reachable vs no dams (100% passage)
total_len <- pop %>% filter(POP_rate == 100) %>% select(HUC12, Total_Length = TOTAL_LENGTH_KM)
pop <- pop %>%
  left_join(total_len, by = "HUC12") %>%
  mutate(percent_available_habitat = PASS_LENGTH_KM / Total_Length * 100)

# Valley bottoms with ponds and fire data
trib <- read.csv("Theoretical Passage Data Files/VB_with_beaver_fire_data.csv")
# Combine split polygons per valley bottom; burned area from fires up to 2017 only
trib <- trib %>%
  filter(!is.na(HUC12)) %>%
  mutate(RCA_Area_SqKm = as.numeric(RCA_Area_SqKm),
         Point_Count   = as.numeric(Point_Count),
         fire_year     = as.integer(substr(Fire_Event_Ignition_Date, 1, 4)),
         burn_km2_raw  = coalesce(BS2_AreaKm2, 0) + coalesce(BS3_AreaKm2, 0) + coalesce(BS4_AreaKm2, 0)) %>%
  group_by(HUC12, uRCA) %>%
  summarize(VB_Area_SqKm = sum(RCA_Area_SqKm, na.rm = TRUE),
            Point_Count  = sum(Point_Count, na.rm = TRUE),
            fire_year    = first(fire_year),
            burn_km2_raw = first(burn_km2_raw),
            .groups = "drop") %>%
  mutate(burned_km2 = ifelse(!is.na(fire_year) & fire_year <= 2017,
                             pmin(burn_km2_raw, VB_Area_SqKm), 0))

# Per tributary: density = ponds / TOTAL valley-bottom area (the main fix); percent burned
huc <- trib %>%
  group_by(HUC12) %>%
  summarize(VB_Area_SqKm = sum(VB_Area_SqKm, na.rm = TRUE),
            Beaver_Count = sum(Point_Count, na.rm = TRUE),
            Burned_km2   = sum(burned_km2, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Beaver_Density = Beaver_Count / VB_Area_SqKm,       # ponds per km2 of valley bottom
         Percent_Burned = 100 * Burned_km2 / VB_Area_SqKm)

# Join habitat and density for tributaries with ponds (133)
dat5 <- pop %>%
  left_join(huc, by = "HUC12") %>%
  filter(Beaver_Density > 0)                                 # same filter as the original figure
dat5$POP_rate <- factor(dat5$POP_rate, levels = c(100, 80, 50, 20))
pop_cols <- c("100" = "darkblue", "80" = "darkgreen", "50" = "#E69F00", "20" = "darkred")

# Density summary overall and by basin (Results)
s100 <- dat5 %>% filter(POP_rate == "100")
dens_summary <- s100 %>%
  summarize(n_tributaries = n(), min = min(Beaver_Density), max = max(Beaver_Density),
            mean = mean(Beaver_Density), sd = sd(Beaver_Density))
basin_summary <- s100 %>% group_by(basin) %>%
  summarize(n_tributaries = n(), mean_density = mean(Beaver_Density),
            n_burned = sum(Percent_Burned > 0), mean_pct_burned = mean(Percent_Burned), .groups = "drop")
print(dens_summary); print(basin_summary)

# Straight-line fit of available habitat vs density or % burned, per passage rate
trend_stats <- function(d, xvar) d %>% group_by(POP_rate) %>%
  group_modify(~ {
    m <- lm(as.formula(paste("percent_available_habitat ~", xvar)), data = .x); s <- summary(m)
    data.frame(n = nrow(.x), slope = coef(m)[2], p_value = coef(s)[2, 4], r2 = s$r.squared)
  }) %>% ungroup()
# Panel B uses burned tributaries only
burned5 <- dat5 %>% filter(Percent_Burned > 0)
dens_fits <- trend_stats(dat5, "Beaver_Density")
fire_fits <- trend_stats(burned5, "Percent_Burned")
print(dens_fits); print(fire_fits)

# Save the Figure 5 data and fits
write.csv(huc, file.path(fig_dir, "tributary_density_and_burn_corrected.csv"), row.names = FALSE)
write.csv(dens_summary, file.path(fig_dir, "tributary_density_summary.csv"), row.names = FALSE)
write.csv(basin_summary, file.path(fig_dir, "basin_density_summary.csv"), row.names = FALSE)
write.csv(dens_fits, file.path(fig_dir, "panelA_density_trend_stats.csv"), row.names = FALSE)
write.csv(fire_fits, file.path(fig_dir, "panelB_fire_trend_stats.csv"), row.names = FALSE)

# coord_cartesian() zooms to 0-100 without dropping data, so fitted lines are drawn in full
# Figure 5 panels (a) and (b), combined and saved
y_scale <- list(scale_y_continuous(breaks = seq(0, 100, 10)), coord_cartesian(ylim = c(0, 100)))
p1_corr <- ggplot(dat5, aes(x = Beaver_Density, y = percent_available_habitat, col = POP_rate)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.5) +
  scale_color_manual(values = pop_cols) + y_scale + theme_cowplot() +
  labs(title = "(a)", x = expression("Beaver-pond density (ponds/km"^2*")"),
       y = "Percent Available Habitat", col = "Probability of Passage (%)")
p2_corr <- ggplot(burned5, aes(x = Percent_Burned, y = percent_available_habitat, col = POP_rate)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.5) +
  scale_color_manual(values = pop_cols) + y_scale + theme_cowplot() +
  labs(title = "(b)", x = "Percent Burned", y = "Percent Available Habitat",
       col = "Probability of Passage (%)")
# Panel titles (a), (b) as ESA asks, at the base font size so text stays <= 10 pt when the
# 10-in-wide figure is printed at ESA's 18-cm maximum width
fig5_corr <- (p1_corr + p2_corr) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom", plot.title = element_text(size = 14, face = "bold"))
fig5_corr
ggsave(file.path(fig_dir, "Figure 5 corrected.jpeg"), fig5_corr, width = 10, height = 5, units = "in", dpi = 600, bg = "white")


# Habitat lost to beaver dams in each tributary at 20% passage, relative to the same
# tributary with no dams (100% passage). PASS_LENGTH_KM is in meters in these files.
p100 <- pop[pop$POP_rate == 100, c("HUC12", "basin", "PASS_LENGTH_KM")]
p20  <- pop[pop$POP_rate == 20,  c("HUC12", "PASS_LENGTH_KM")]
trib_loss <- merge(p100, p20, by = "HUC12", suffixes = c("_100", "_20"))
trib_loss$lost_km  <- (trib_loss$PASS_LENGTH_KM_100 - trib_loss$PASS_LENGTH_KM_20) / 1000
trib_loss$pct_lost <- 100 * (1 - trib_loss$PASS_LENGTH_KM_20 / trib_loss$PASS_LENGTH_KM_100)
trib_loss <- trib_loss[order(-trib_loss$pct_lost), ]
print(head(trib_loss, 10), row.names = FALSE)          # HUC12 190803050204 = Paldo Creek (Salcha)
write.csv(trib_loss, file.path(fig_dir, "tributary_habitat_loss_POP20.csv"), row.names = FALSE)

# Table 3: available habitat by basin and passage rate. Percentages are the change
# from potential habitat (100% passage); the Total row is calculated from the summed
# lengths, not averaged across basins (caption corrected in Revision 1).
table3 <- pop %>% group_by(basin, POP_rate) %>% summarise(km = sum(PASS_LENGTH_KM) / 1000, .groups = "drop")
table3 <- bind_rows(table3, table3 %>% group_by(POP_rate) %>% summarise(km = sum(km)) %>% mutate(basin = "Total"))
table3 <- table3 %>% group_by(basin) %>% mutate(pct_change = 100 * (km / km[POP_rate == 100] - 1)) %>% ungroup()
print(as.data.frame(table3 %>% mutate(km = round(km), pct_change = round(pct_change, 1))))
write.csv(table3, file.path(tab_dir, "Table 3 basin habitat (Revision 1).csv"), row.names = FALSE)


# Part 6. Table 2 and Appendix S4 with AICcmodavg ------------------------------
#Same tables redone with AICcmodavg (the package from the original analysis) and base R,
#without my aicc(), r2_adj(), or modavg_param() functions. Differences should all be ~0.
if (requireNamespace("AICcmodavg", quietly = TRUE)) {
  cat("\nPart 6. Largest differences vs AICcmodavg:\n")
  for (s6 in names(sets)) {
    at <- as.data.frame(AICcmodavg::aictab(cand.set = sets[[s6]]$fits, modnames = names(sets[[s6]]$fits)))
    mine <- table2[table2$dataset == s6, ]
    mine <- mine[match(at$Modnames, mine$model), ]
    top <- top_models(sets[[s6]], s6)
    d6 <- c()
    for (p in appendix$parameter[appendix$dataset == s6]) {
      has <- sapply(top, function(m) p %in% names(coef(m)))
      ma <- AICcmodavg::modavg(cand.set = top[has], parm = p, modnames = names(top)[has], conf.level = 0.90)
      a <- appendix[appendix$dataset == s6 & appendix$parameter == p, ]
      d6 <- c(d6, abs(ma$Mod.avg.beta - a$estimate), abs(ma$Uncond.SE - a$uncond_SE),
              abs(ma$Lower.CL - a$lower_90), abs(ma$Upper.CL - a$upper_90))
    }
    cat(sprintf("  %-19s AICc %.1e | weight %.1e | K %g | Appendix S4 %.1e\n", s6,
                max(abs(at$AICc - mine$AICc)), max(abs(at$AICcWt - mine$weight)),
                max(abs(at$K - mine$K)), max(d6)))
  }
} else cat("\nPart 6 skipped (needs AICcmodavg)\n")

#R2 and adjusted R2 vs summary(lm()) (same models refit with lm)
d6 <- c()
for (s6 in names(sets)) for (nm in names(sets[[s6]]$fits)) {
  m  <- sets[[s6]]$fits[[nm]]
  sl <- summary(lm(formula(m), data = model.frame(m)))
  mine <- table2[table2$dataset == s6 & table2$model == nm, ]
  d6 <- c(d6, abs(sl$r.squared - mine$R2), abs(sl$adj.r.squared - mine$adj_R2))
}
cat("  Largest R2 / adjusted R2 difference vs summary(lm()):", signif(max(d6), 2), "\n")


# Part 7. Sensitivity runs (not used in the manuscript) ------------------------
#7a. Keep the data-dredged model (dredge.vb from the original script) in the burned
#    candidate set. Its covariates were picked by MuMIn::dredge on the same data, which is
#    why I dropped it.
dredge_vars <- c("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "MEANANNCMS_mean", "SINUOSITY_mean",
                 "STRM_ORDER_max", "WIDTH_M_mean", "StrmPow_mean")
set_labels["dredge.vb"] <- "Dredge (reduced geomorphology, hydrology)"
s7a <- selection_table(fit_set(burned_VB, c(cand_all, list(dredge.vb = dredge_vars))), "Burned + dredge model")
cat("\nPart 7a. Burned catchments with the dredge model kept (n =", s7a$n[1], "):\n")
print(s7a %>% transmute(covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
                        weight = round(weight, 2)), row.names = FALSE)

#7b. Original burn severity definition (class area / VB area, with percent no burn as a
#    covariate) on the corrected valley bottoms.
vb_7b <- burned_VB
orig_sev <- vb_raw[!duplicated(vb_raw$uRCA), c("uRCA", "percent_severe_burn", "percent_moderate_burn",
                                                "percent_mild_burn", "percent_no_burn")]
vb_7b[, c("percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")] <-
  orig_sev[match(vb_7b$uRCA, orig_sev$uRCA), -1]
vb_7b[is.na(vb_7b$Fire_Event_Area_Percent_of_uRCA),
      c("percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")] <- NA
cand_7b <- lapply(cand_all, function(x) if (any(x %in% fire)) c(x, "percent_no_burn") else x)
s7b <- selection_table(fit_set(vb_7b, cand_7b), "Burned, original severity")
cat("\nPart 7b. Burned catchments with the original severity definition (n =", s7b$n[1], "):\n")
print(s7b %>% transmute(covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
                        weight = round(weight, 2)), row.names = FALSE)


#7c. Collinearity: refit the burned candidate set without stream width and depth (both
#    strongly correlated with mean annual discharge; see "VIF check.csv"). This is the
#    sensitivity check in the Methods and Results.
drop_wd <- function(x) setdiff(x, c("WIDTH_M_mean", "DEPTH_M_mean"))
fs_7c  <- fit_set(burned_VB, lapply(cand_all, drop_wd))
s7c    <- selection_table(fs_7c, "Burned, no width/depth")
top_7c <- fs_7c$fits[s7c$model[s7c$weight >= 0.05]]
ma_7c  <- do.call(rbind, lapply(unique(unlist(lapply(top_7c, function(m) names(coef(m))[-1]))),
                                function(p) modavg_param(top_7c, p)))
ma_7c$important <- ma_7c$lower_90 > 0 | ma_7c$upper_90 < 0
cat("\nPart 7c. Burned catchments without stream width and depth (n =", s7c$n[1], "):\n")
print(s7c %>% transmute(covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
                        weight = round(weight, 2)), row.names = FALSE)
print(ma_7c %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
# All other covariates have VIF < 10 in every non-global model
max_other_vif <- max(unlist(lapply(names(sets), function(s7) sapply(setdiff(names(sets[[s7]]$fits), "global.vb.reduced"),
  function(nm) {
    v <- cand_all[[nm]]
    if (length(v) < 2) return(1)
    x <- vif_check(sets[[s7]]$data, v)
    max(x[!names(x) %in% c("WIDTH_M_mean", "DEPTH_M_mean", "MEANANNCMS_mean")])
  }))))
cat("  Max VIF of the other covariates, all non-global models:", round(max_other_vif, 2), "\n")


# Part 8. Appendix S2: grayling distribution thresholds (Table S1, Figure S1) --
#Habitat thresholds (mean annual flow > 0.06 m3/s, gradient < 4%) come from the stream
#reaches with grayling in the Alaska Freshwater Fish Inventory (Yukon and Kuskokwim basins;
#n = 899 observations). The file has two NetMap flow estimates. The 0.06 m3/s cutoff is
#the 10th percentile of MEANANNCMS_Brabets (the column used in grayling_dist_4Will.R);
#for MEANANNCMS_MEAN, 0.06 m3/s would be below the 1st percentile. Gradient is stored as
#a proportion (0.04 = 4%).
#Fixed from the first-submission Appendix S2: the table mixed the two flow estimates (min
#and 95th percentile from Brabets, mean and median from MEANANNCMS_MEAN), listed the 10th
#percentile as the 5th, and had a typo in the max (105.84 for 1,056.84); the gradient row
#didn't match the data; the text called the gradient cutoff the 5th percentile (it's the
#95th, 3.6%, rounded to 4%); and the flow histogram showed MEANANNCMS_MEAN with the axis
#label "m/s^2".
cat("\nPart 8. Appendix S2 grayling distribution thresholds\n")
# The 899 grayling observations used to set the habitat cutoffs
gr <- read.csv("Input Data Files/grayling_pres_new.csv")
# Flow (Brabets estimate) and gradient in percent
flow <- gr$MEANANNCMS_Brabets
grad <- gr$GRADIENT_MEAN * 100                        # percent
# Summary statistics for Table S1, printed and saved
s2_stats <- function(x) c(Min = min(x), P05 = unname(quantile(x, 0.05)), P10 = unname(quantile(x, 0.10)),
                          Mean = mean(x), Median = median(x), P95 = unname(quantile(x, 0.95)), Max = max(x))
tableS1 <- rbind(`Mean annual flow (m3/s)` = s2_stats(flow), `Gradient (%)` = s2_stats(grad))
print(round(tableS1, 2))
write.csv(round(tableS1, 4), file.path(tab_dir, "Appendix S2 Table S1 (Revision 1).csv"))

# Figure S1: histograms with dashed lines at the cutoffs, saved as two files
s2_theme <- theme_classic() + theme(text = element_text(size = 15), axis.text = element_text(color = "black"),
                                    plot.title = element_text(face = "bold"))
pS2a <- ggplot(data.frame(flow), aes(x = flow)) +
  geom_histogram(bins = 25, color = "black", fill = "cornflowerblue") +
  geom_vline(xintercept = 0.06, linetype = "dashed") +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), labels = c("0.01", "0.1", "1", "10", "100", "1000")) +
  labs(title = "(a)", x = expression("Mean annual flow (m"^3*"/s, log scale)"), y = "Frequency") + s2_theme
pS2b <- ggplot(data.frame(grad), aes(x = grad)) +
  geom_histogram(binwidth = 0.5, boundary = 0, color = "black", fill = "cornflowerblue") +
  geom_vline(xintercept = 4, linetype = "dashed") +
  labs(title = "(b)", x = "Gradient (%)", y = "Frequency") + s2_theme
ggsave(file.path(fig_dir, "Appendix S2 Figure S1A (Revision 1).jpeg"), pS2a, width = 6, height = 4, units = "in", dpi = 500, bg = "white")
ggsave(file.path(fig_dir, "Appendix S2 Figure S1B (Revision 1).jpeg"), pS2b, width = 6, height = 4, units = "in", dpi = 500, bg = "white")
