################################################################################
##################### Revision 1 (Ecosphere): updated analyses #################
################################################################################
#
# One script for every data correction and re-analysis made for Revision 1.
#
# This script is NEW. It does not change or replace any existing script, data
# file, or figure:
#   - "Beaver-fire regression FINAL.R" (original beaver-wildfire models, Table 2,
#     Appendix S4, Figure 4) is untouched.
#   - "Beaver_Grayling_Theroetical_Modeling_Analysis.R" (original Figure 5) is
#     untouched.
#   - Input data files are only read, never overwritten.
# Outputs are written to two NEW folders, so the original results stay available
# for comparison (and the changes can be reverted):
#   - "Output Data Files/Revision 1/"  (tables, summary statistics)
#   - "Figures/Revision 1/"            (Figure 4 and Figure 5)
#
# It supersedes "Figure 5 corrected (Revision 1).R" (Part 5 below contains the
# same Figure 5 code).
#
# Run from the RStudio project (Beaver_Fire_Fish_Public.Rproj) so the relative
# paths resolve. Packages: dplyr, ggplot2, cowplot, patchwork. Part 6 also uses
# AICcmodavg (the package used in the original analysis).
#
# HOW TO VERIFY (run it top to bottom, or one Part at a time with Ctrl+Enter)
#   PART 0  reproduces the ORIGINAL published numbers from the original data and
#           shows where each problem came from (mixed sample sizes, inflated
#           pseudo R2, Figure 5 densities).
#   PART 1-5 are the corrected analyses. Each Part ends with CHECKPOINTS that
#           print the value calculated here next to the value now in the
#           revised manuscript, marked PASS or "<<< CHECK".
#   PART 6  recalculates Table 2 and Appendix S4 with AICcmodavg::aictab(),
#           AICcmodavg::modavg(), and summary(lm()) (R2 and adjusted R2),
#           independently of this script's own functions, and checks that
#           they agree.
#   PART 7  sensitivity runs: (a) keeping the data-dredged model, (b) the
#           original burn severity definition.
#   PART 8  Appendix S2 (grayling distribution thresholds): summary table,
#           habitat thresholds, and Figure S1, from the grayling observations
#           ("Input Data Files/grayling_pres_new.csv", copied from Beaver_Project_2).
#   The last lines print how many checkpoints passed.
#
# PART 1. Model data corrections and summary statistics (Results, para. 1)
#   1a. Nine valley bottoms (uRCA) appear twice in VB_Y_sums_full_EDITED.csv:
#       the valley bottom plus a ~17-25 m2 sliver reach contributing area with
#       the same uRCA. All valley-bottom attributes, pond counts, and covariates
#       are identical between the two rows, so each valley bottom is kept once
#       (186 valley bottoms, 414 high-confidence ponds; was 195 and 435).
#   1b. Burned / unburned follows the pond-based definition used in the
#       original script: a valley bottom is burned when it has a time since
#       last burn (its ponds fell within a fire perimeter that predates the
#       2017 imagery). Fires after the imagery (negative time since burn) are
#       treated as unburned, as in the original script.
#   1c. uRCA 2231 burned in 2015 (ignition 7 May 2015) but its time since burn
#       was coded -1, which made it "unburned". It is set to 2 (2017 - 2015)
#       and its fire covariates are kept.
#   1d. Burn severity covariates are re-expressed as the percentage of the
#       MTBS-classified area in each severity class (sums to 100%). The original
#       percentages divided class area by valley-bottom area, which exceeded
#       100% in some valley bottoms. Because the four classes now sum to 100%,
#       percent no burn (MTBS "unburned to low") is the reference class and is
#       not entered as a separate covariate.
#
# PART 2. Model selection (Table 2): every candidate model in a dataset is fit
#   to the SAME valley bottoms (rows with complete data for every covariate in
#   that dataset's candidate set), so AICc values are comparable. In the
#   original, models with wildfire covariates were fit to 52 rows and the
#   others to up to 194, and AICc is not comparable across different data.
#   Models with wildfire covariates can only be fit to burned valley bottoms
#   (wildfire covariates are undefined for unburned ones), so they are
#   evaluated in the burned dataset; the all-valley-bottom and unburned
#   datasets use the non-wildfire candidate models.
#   Model fit is reported as R2 and adjusted R2. For these Gaussian GLMs, the
#   Cox and Snell, Nagelkerke, and Efron pseudo R2 all equal the ordinary R2;
#   McFadden's version depends on the units of the response and pulled the
#   averaged pseudo R2 used in the first submission down, so it is no longer
#   reported (the four pseudo R2 measures are still saved as extra columns in
#   "Table 2 - model selection (all models).csv").
#   The data-dredged model ("dredge.vb" in the original script) is not part of
#   the candidate set (it was not described in the manuscript, and its
#   covariates were chosen from the same data).
#
# PART 3. Model averaging (Appendix S4): models with AICc weight >= 0.05 in each
#   dataset; same calculations as AICcmodavg::modavg (weights renormalized
#   among the models that contain the parameter; revised unconditional SE;
#   90% unconditional CI).
#
# PART 4. Figure 4: model-averaged predictions (burned valley bottoms) for every
#   covariate in the highest-R2 top model (Geomorphology, Hydrology, Fire) plus
#   the important vegetation covariates; each panel averages the top models that
#   contain that covariate, with other covariates held at their medians.
#   Important covariates (90% unconditional CI excludes zero) are blue with *.
#
# PART 5. Figure 5 and tributary pond densities (Results, last paragraph).
################################################################################

# Packages: install any that are missing, then load them
needed <- c("dplyr", "ggplot2", "cowplot", "patchwork")
missing_pkgs <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
# Load packages (dplyr = data wrangling; ggplot2/cowplot/patchwork = figures)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
# Print your R and package versions (useful if something errors)
cat("R", R.version$major, ".", R.version$minor, " | dplyr ", as.character(packageVersion("dplyr")),
    " | ggplot2 ", as.character(packageVersion("ggplot2")), "\n", sep = "")

# Output folders for new tables and figures (created if missing; originals untouched)
tab_dir <- "Output Data Files/Revision 1"
fig_dir <- "Figures/Revision 1"
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Stop early if R is not running from the project folder
if (!file.exists("Output Data Files/VB_Y_sums_full_EDITED.csv"))
  stop("Open Beaver_Fire_Fish_Public.Rproj (or setwd() to the project folder) before running this script.")

# Checkpoint helpers: compare a value calculated here with the value in the manuscript
# Counters for the checkpoint summary printed at the very end
n_pass <- 0; n_check <- 0
# check(): rounds a value calculated here, compares it to the manuscript value, prints PASS or <<< CHECK
check <- function(label, got, expected, digits = 2) {
  ok <- isTRUE(all.equal(round(got, digits), expected, tolerance = 1e-9))
  if (ok) n_pass <<- n_pass + 1 else n_check <<- n_check + 1
  cat(sprintf("  %-62s here: %-10s manuscript: %-10s %s\n", label,
              formatC(round(got, digits), format = "f", digits = digits),
              formatC(expected, format = "f", digits = digits), if (ok) "PASS" else "<<< CHECK"))
  invisible(ok)
}
# check_set(): same idea, but compares two lists of names (order ignored)
check_set <- function(label, got, expected) {
  ok <- setequal(got, expected)
  if (ok) n_pass <<- n_pass + 1 else n_check <<- n_check + 1
  cat(sprintf("  %-62s %s\n", label, if (ok) "PASS" else paste("<<< CHECK  here:", paste(sort(got), collapse = ", "))))
  invisible(ok)
}
# aicc(): AICc for a fitted model (same formula AICcmodavg uses)
aicc <- function(m) {                          # AICc exactly as AICcmodavg (K = logLik df)
  ll <- logLik(m); K <- attr(ll, "df"); n <- nobs(m)
  -2 * as.numeric(ll) + 2 * K + 2 * K * (K + 1) / (n - K - 1)
}


# ==============================================================================
# PART 0. Reproduce the ORIGINAL published numbers (to see what went wrong)
# ==============================================================================
# Same data and cleaning as "Beaver-fire regression FINAL.R" (lines 41-55).
# Original model data, cleaned the same way as the original script
orig <- read.csv("Output Data Files/VB_Y_sums_full_EDITED.csv")
orig_neg <- !is.na(orig$Time_Since_Burn_min) & orig$Time_Since_Burn_min < 0
for (v in c("Fire_Event_Area_Percent_of_uRCA", "BS1_AreaKm2", "BS2_AreaKm2", "BS3_AreaKm2", "BS4_AreaKm2"))
  orig[[v]][orig_neg] <- NA
orig$Time_Since_Burn_min[orig_neg] <- NA
orig$FP_WIDTH_mean[orig$FP_WIDTH_mean == -9999] <- NA

# The original covariate groups
o_geo  <- c("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "SINUOSITY_mean", "STRM_ORDER_max",
            "WIDTH_M_mean", "DEPTH_M_mean", "ELEV_M_mean")
o_hyd  <- c("StrmPow_mean", "MEANANNCMS_mean")
o_fire <- c("Time_Since_Burn_min", "Fire_Event_Area_Percent_of_uRCA", "percent_severe_burn",
            "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")
o_veg  <- c("percent_woody_cover", "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN", "DeciduousShrub_2015_MEAN",
            "EvergreenShrub_2015_MEAN", "Forb_2015_MEAN", "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN")
# o_fit(): fit a GLM of pond density on a list of covariates
o_fit <- function(x, d) glm(as.formula(paste("dams_per_km2 ~", paste(x, collapse = " + "))), data = d)
# Pseudo R2 as originally reported: rcompanion::nagelkerke() refits the null model to ALL rows
# of the data frame, even when the model itself dropped rows with missing covariates.
# Pseudo R2 the way it was originally calculated (null model fit to ALL rows -> inflated values)
pseudo_r2_as_reported <- function(m, d) {
  n <- nobs(m); ll <- as.numeric(logLik(m)); ll0 <- as.numeric(logLik(glm(dams_per_km2 ~ 1, data = d)))
  mcf <- 1 - ll / ll0; cs <- 1 - exp(-2 / n * (ll - ll0)); nk <- cs / (1 - exp(2 / n * ll0))
  y <- model.response(model.frame(m)); ef <- 1 - sum((y - fitted(m))^2) / sum((y - mean(y))^2)
  mean(c(mcf, cs, nk, ef))
}
# Burned valley bottoms in the original data
orig_burned <- orig[!is.na(orig$Time_Since_Burn_min), ]
# Refit three original models
o1 <- o_fit(c(o_geo, o_hyd, o_fire), orig)          # "All catchments" Geo/Hydro/Fire
o2 <- o_fit(c(o_geo, o_fire), orig)                 # "All catchments" Geo/Fire
o3 <- o_fit(c(o_geo, o_hyd), orig)                  # Geo/Hydro (no fire)
# Show how many rows each model actually used (glm drops rows with missing values)
cat("\nPART 0: original analysis. Note the different sample sizes (glm drops rows with NA):\n")
cat("  Geo/Hydro/Fire n =", nobs(o1), "| Geo/Fire n =", nobs(o2), "| Geo/Hydro n =", nobs(o3),
    "| valley-bottom rows =", nrow(orig), "(includes 9 duplicates)\n")
cat("  AICc from models fit to different data cannot be compared; fewer rows -> lower AICc.\n")
cat("CHECKPOINTS (original Table 2 and Abstract are reproduced exactly):\n")
# These should PASS: proves the original numbers are reproduced
check("Original Table 2 Geo/Hydro/Fire AICc", aicc(o1), 312.6, 1)
check("Original Table 2 Geo/Fire AICc", aicc(o2), 315.5, 1)
check("Original Table 2 all catchments Geo/Hydro/Fire pseudo R2", pseudo_r2_as_reported(o1, orig), 0.88)
check("Original Table 2 burned Geo/Hydro/Fire pseudo R2", pseudo_r2_as_reported(o1, orig_burned), 0.83)
check("Original Abstract fire-only pseudo R2", pseudo_r2_as_reported(o_fit(o_fire, orig), orig), 0.75)
check("Original Abstract geomorphology/hydrology pseudo R2", pseudo_r2_as_reported(o3, orig), 0.31)
check("Original Abstract vegetation pseudo R2", pseudo_r2_as_reported(o_fit(o_veg, orig), orig), 0.19)
cat("  Same Geo/Hydro/Fire model, null fit to the SAME 52 rows instead: McFadden =",
    round(1 - as.numeric(logLik(o1)) / as.numeric(logLik(glm(dams_per_km2 ~ 1, data = model.frame(o1)))), 2), "\n")

# Original Figure 5 tributary densities (Beaver_Grayling_Theroetical_Modeling_Analysis.R,
# lines 710-731). The original code divided the summed ponds in a tributary by the MEAN
# area of ONE valley bottom. Its summarize() also returned one row per valley bottom
# instead of one per tributary (because of the ifelse(is.na(...)) line); newer versions
# of dplyr no longer allow that, so the same result is rebuilt here with mutate(), which
# keeps one row per valley bottom and gives identical numbers.
# Original tributary densities: ponds / area of ONE average valley bottom (the original error)
orig5 <- read.csv("Theoretical Passage Data Files/VB_with_beaver_fire_data.csv") %>%
  group_by(HUC12) %>%
  mutate(VB_Area_SqKm = mean(RCA_Area_SqKm),         # mean area of ONE valley bottom (the error)
         Beaver_Count = sum(Point_Count),            # all ponds in the tributary
         BS_sum_AreaKm2 = ifelse(is.na(BS_sum_AreaKm2), 0, sum(BS_sum_AreaKm2))) %>%
  ungroup() %>%
  select(HUC12, VB_Area_SqKm, Beaver_Count, BS_sum_AreaKm2) %>%
  mutate(Beaver_Density = Beaver_Count / VB_Area_SqKm)
# Attach these to the habitat-simulation output (one row per tributary), as the original did.
# merge() is base R, so this line behaves the same in every dplyr version.
orig5 <- merge(read.csv("Theoretical Passage Data Files/POP.100HUC.HAB.dat.csv"), orig5, by = "HUC12", all.x = TRUE)
orig5 <- orig5[!is.na(orig5$Beaver_Density) & orig5$Beaver_Density > 0, ]
cat("  Original Figure 5 summary used", nrow(orig5), "rows for about 133 tributaries.\n")
# Should PASS: matches the original published Figure 5 values
check("Original Results tributary density minimum", min(orig5$Beaver_Density), 0.2, 1)
check("Original tributary density maximum (code comment 137.25; text 137.3)", max(orig5$Beaver_Density), 137.25, 2)
check("Original Results tributary density mean", mean(orig5$Beaver_Density), 23.6, 1)
check("Original Results Chatanika mean density", mean(orig5$Beaver_Density[orig5$basin == "chat"]), 33.1, 1)


# ==============================================================================
# PART 1. Model data corrections and summary statistics
# ==============================================================================

# Start again from the raw model data for the corrected analysis
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
# ttest_stats(): means, SDs and a Welch t-test, burned vs unburned
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

# sv(): look up one statistic by name for the checks below
sv <- function(name) summary_stats$value[summary_stats$statistic == name]
cat("\nPART 1 CHECKPOINTS (Abstract, Methods, Results paragraph 1):\n")
# Checkpoints: every Results paragraph 1 number
check("Valley bottoms with HC ponds", sv("Valley bottoms with HC ponds"), 186, 0)
check("HC ponds", sv("HC ponds"), 414, 0)
check("Burned valley bottoms", sv("Burned valley bottoms"), 87, 0)
check("Unburned valley bottoms", sv("Unburned valley bottoms"), 99, 0)
check("Pond density mean (ponds/km2)", sv("Pond density mean (ponds/km2)"), 7.4, 1)
check("Pond density SD", sv("Pond density SD"), 10.44, 2)
check("Burned VB area mean (km2)", sv("Valley bottom area (km2) burned mean"), 0.52, 2)
check("Burned VB area SD", sv("Valley bottom area (km2) burned SD"), 0.39, 2)
check("Unburned VB area mean", sv("Valley bottom area (km2) unburned mean"), 0.74, 2)
check("Unburned VB area SD", sv("Valley bottom area (km2) unburned SD"), 1.00, 2)
check("VB area t-test T", sv("Valley bottom area (km2) t-test T"), -2.04, 2)
check("VB area t-test DF", sv("Valley bottom area (km2) t-test DF"), 130.22, 2)
check("VB area t-test p", sv("Valley bottom area (km2) t-test p"), 0.04, 2)
check("Time since burn mean (yr)", sv("Time since burn mean (yr)"), 26, 0)
check("Time since burn SD", sv("Time since burn SD"), 20, 0)
check("Burned density mean", sv("Pond density (ponds/km2) burned mean"), 7.7, 1)
check("Burned density SD", sv("Pond density (ponds/km2) burned SD"), 9.6, 1)
check("Unburned density mean", sv("Pond density (ponds/km2) unburned mean"), 7.2, 1)
check("Unburned density SD", sv("Pond density (ponds/km2) unburned SD"), 11.2, 1)
check("Density t-test T", sv("Pond density (ponds/km2) t-test T"), 0.32, 2)
check("Density t-test DF", sv("Pond density (ponds/km2) t-test DF"), 183.94, 2)
check("Density t-test p", sv("Pond density (ponds/km2) t-test p"), 0.75, 2)
check("Burned VBs with burn severity data", sv("Burned VBs with burn severity data"), 49, 0)
check("Severe burn mean (% of classified area)", sv("percent_severe_burn mean (% of classified area)"), 6.64, 2)
check("Severe burn SD", sv("percent_severe_burn SD"), 9.04, 2)
check("Severe burn max", sv("percent_severe_burn max"), 36.53, 2)
check("Moderate burn mean (% of classified area)", sv("percent_moderate_burn mean (% of classified area)"), 24.38, 2)
check("Moderate burn SD", sv("percent_moderate_burn SD"), 15.13, 2)
check("Moderate burn max", sv("percent_moderate_burn max"), 73.35, 2)
check("Pond area mean (m2)", sv("Pond area mean (m2)"), 2880, 0)
check("Pond area SD", sv("Pond area SD"), 4903, 0)
check("Burned pond area mean", sv("Pond area (m2) burned mean"), 2062, 0)
check("Burned pond area SD", sv("Pond area (m2) burned SD"), 2444, 0)
check("Unburned pond area mean", sv("Pond area (m2) unburned mean"), 3600, 0)
check("Unburned pond area SD", sv("Pond area (m2) unburned SD"), 6246, 0)
check("Pond area t-test T", sv("Pond area (m2) t-test T"), -2.26, 2)
check("Pond area t-test DF", sv("Pond area (m2) t-test DF"), 130.61, 2)
check("Pond area t-test p", sv("Pond area (m2) t-test p"), 0.03, 2)
check("Woody cover burned / unburned", sv("percent_woody_cover burned mean"), 65.7, 1)
check("  (unburned)", sv("percent_woody_cover unburned mean"), 67.7, 1)
check("Conifer burned", sv("ConiferTree_2015_MEAN burned mean"), 15.8, 1)
check("Conifer unburned", sv("ConiferTree_2015_MEAN unburned mean"), 22.0, 1)
check("Conifer t-test T", sv("ConiferTree_2015_MEAN t-test T"), -5.55, 2)
check("Deciduous shrub burned", sv("DeciduousShrub_2015_MEAN burned mean"), 28.4, 1)
check("Deciduous shrub unburned", sv("DeciduousShrub_2015_MEAN unburned mean"), 23.7, 1)
check("Deciduous shrub t-test T", sv("DeciduousShrub_2015_MEAN t-test T"), 3.82, 2)
# Discussion (time since burn): fire timing among the burned valley bottoms with burn severity
# data (the 49 used in the burned models): all last burned 2003-2015, 78% in 2004
check("Discussion: severity-data VBs, earliest last-burn year", 2017 - max(sev$Time_Since_Burn_min), 2003, 0)
check("Discussion: severity-data VBs, latest last-burn year", 2017 - min(sev$Time_Since_Burn_min), 2015, 0)
check("Discussion: % of severity-data VBs that burned in 2004", 100 * mean(sev$Time_Since_Burn_min == 13), 78, 0)
# Discussion (wildfire paragraph): deciduous shrub cover rises with % severe burn, and most
# burned valley bottoms burned more than a decade before the 2017 imagery
# Severe burn vs deciduous shrub correlation (Discussion)
check("Discussion: r(% severe burn, deciduous shrub), burned VBs with severity", cor(sev$percent_severe_burn, sev$DeciduousShrub_2015_MEAN), 0.32, 2)
check("Discussion: share of burned VBs that burned >= 10 years before imagery", mean(burned_VB$Time_Since_Burn_min >= 10) > 0.5, 1, 0)
cat(sprintf("  (burned >= 10 years before imagery: %.1f%% of all burned VBs; %.1f%% of those with severity data)\n",
            100 * mean(burned_VB$Time_Since_Burn_min >= 10), 100 * mean(sev$Time_Since_Burn_min >= 10)))


# ==============================================================================
# PART 2. Model selection on common rows (Table 2)
# ==============================================================================

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

# pseudo_r2(): four pseudo R2 measures and their mean, null model fit to the SAME rows
# (saved as extra columns only; the manuscript reports R2 and adjusted R2)
pseudo_r2 <- function(m) {                     # same four measures as rcompanion::nagelkerke + Efron
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
# r2_adj(): ordinary R2 and adjusted R2 (k = number of covariates, not counting the intercept)
r2_adj <- function(m) {
  y <- model.response(model.frame(m)); n <- length(y); k <- length(coef(m)) - 1
  r2 <- 1 - sum((y - fitted(m))^2) / sum((y - mean(y))^2)
  c(R2 = r2, adj_R2 = 1 - (1 - r2) * (n - 1) / (n - k - 1))
}
# fit_set(): keep rows complete for every covariate in the set, then fit all models to those same rows
fit_set <- function(dat, cand) {
  vars <- unique(unlist(cand))
  common <- dat[complete.cases(dat[, c("dams_per_km2", vars)]), ]
  fits <- lapply(cand, function(x)
    glm(as.formula(paste("dams_per_km2 ~", paste(x, collapse = " + "))), data = common))
  list(data = common, fits = fits)
}
# selection_table(): AICc, delta AICc and weights, best model first
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

# t2(): look up one Table 2 value for the checks
t2 <- function(ds, cs, col) table2[table2$dataset == ds & table2$covariate_set == cs, col]
cat("\nPART 2 CHECKPOINTS (Methods sample sizes, Table 2, Results paragraph 2, Abstract):\n")
# Checkpoints: sample sizes and every Table 2 value
check("Common rows: all catchments", nrow(sets[["All catchments"]]$data), 179, 0)
check("Common rows: burned catchments", nrow(sets[["Burned catchments"]]$data), 49, 0)
check("Common rows: unburned catchments", nrow(sets[["Unburned catchments"]]$data), 98, 0)
ms_t2 <- list(   # dataset, covariate set, R2, adjusted R2, AICc, weight, K
  list("All catchments", "Geomorphology, Vegetation", 0.29, 0.22, 1303.4, 0.70, 18),
  list("All catchments", "Vegetation", 0.20, 0.16, 1306.1, 0.18, 10),
  list("All catchments", "Geomorphology, Hydrology, Vegetation", 0.29, 0.21, 1307.8, 0.08, 20),
  list("All catchments", "Hydrology, Vegetation", 0.20, 0.15, 1309.3, 0.04, 12),
  list("Burned catchments", "Fire", 0.23, 0.14, 298.6, 0.29, 7),
  list("Burned catchments", "Geomorphology, Fire", 0.56, 0.39, 299.0, 0.24, 15),
  list("Burned catchments", "Vegetation", 0.34, 0.21, 299.8, 0.17, 10),
  list("Burned catchments", "Geomorphology, Hydrology, Fire", 0.63, 0.46, 300.1, 0.14, 17),
  list("Burned catchments", "Geomorphology, Hydrology", 0.40, 0.25, 301.9, 0.06, 12),
  list("Burned catchments", "Hydrology, Vegetation", 0.39, 0.23, 302.7, 0.04, 12),
  list("Burned catchments", "Hydrology, Fire", 0.25, 0.12, 302.9, 0.03, 9),
  list("Burned catchments", "Geomorphology, Vegetation", 0.64, 0.46, 303.2, 0.03, 18),
  list("Burned catchments", "Global", 0.78, 0.57, 327.3, 0.00, 25),
  list("Unburned catchments", "Vegetation", 0.25, 0.18, 744.7, 0.63, 10),
  list("Unburned catchments", "Geomorphology, Vegetation", 0.39, 0.27, 746.5, 0.26, 18),
  list("Unburned catchments", "Hydrology, Vegetation", 0.26, 0.18, 748.4, 0.10, 12),
  list("Unburned catchments", "Geomorphology, Hydrology, Vegetation", 0.40, 0.26, 751.9, 0.02, 20))
for (r in ms_t2) {
  lab <- paste0(sub(" catchments", "", r[[1]]), ": ", r[[2]])
  check(paste(lab, "R2"), t2(r[[1]], r[[2]], "R2"), r[[3]], 2)
  check(paste(lab, "adjusted R2"), t2(r[[1]], r[[2]], "adj_R2"), r[[4]], 2)
  check(paste(lab, "AICc"), t2(r[[1]], r[[2]], "AICc"), r[[5]], 1)
  check(paste(lab, "weight"), t2(r[[1]], r[[2]], "weight"), r[[6]], 2)
  check(paste(lab, "K"), t2(r[[1]], r[[2]], "K"), r[[7]], 0)
}
check("Combined weight of burned models with wildfire covariates",
      sum(table2$weight[table2$dataset == "Burned catchments" & table2$model %in% fire_models]), 0.71, 2)


# ==============================================================================
# PART 3. Model averaging (Appendix S4)
# ==============================================================================

# modavg_param(): weighted average of one covariate across the top models, with 90% interval (as AICcmodavg::modavg)
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
# top_models(): models with AICc weight >= 0.05
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

# Helpers to look up Appendix S4 values for the checks
ap <- function(ds, p, col) appendix[appendix$dataset == ds & appendix$parameter == p, col]
imp <- function(ds) appendix$parameter[appendix$dataset == ds & appendix$important]
cat("\nPART 3 CHECKPOINTS (Appendix S4, Results paragraph 3, Discussion):\n")
# Checkpoints: which covariates are important, and key estimates
check_set("Burned: important parameters (90% CI excludes 0)", imp("Burned catchments"),
          c("percent_severe_burn", "Area_km2_VB", "SINUOSITY_mean", "DeciduousShrub_2015_MEAN", "Forb_2015_MEAN"))
check_set("All: important parameters", imp("All catchments"),
          c("Area_km2_VB", "SINUOSITY_mean", "ELEV_M_mean", "percent_woody_cover", "BroadleafTree_2015_MEAN", "Graminoid_2015_MEAN"))
check_set("Unburned: important parameters", imp("Unburned catchments"),
          c("Area_km2_VB", "ELEV_M_mean", "ConiferTree_2015_MEAN", "BroadleafTree_2015_MEAN",
            "DeciduousShrub_2015_MEAN", "Graminoid_2015_MEAN", "tmLichenLight_2015_MEAN"))
check("Burned: severe burn estimate", ap("Burned catchments", "percent_severe_burn", "estimate"), 0.26, 2)
check("Burned: severe burn 90% CI lower", ap("Burned catchments", "percent_severe_burn", "lower_90"), 0.12, 2)
check("Burned: severe burn 90% CI upper", ap("Burned catchments", "percent_severe_burn", "upper_90"), 0.40, 2)
check("Burned: moderate burn estimate", ap("Burned catchments", "percent_moderate_burn", "estimate"), -0.09, 2)
check("Burned: moderate burn 90% CI lower", ap("Burned catchments", "percent_moderate_burn", "lower_90"), -0.17, 2)
check("Burned: moderate burn 90% CI upper", ap("Burned catchments", "percent_moderate_burn", "upper_90"), 0.00, 2)
check("Burned: valley bottom area estimate", ap("Burned catchments", "Area_km2_VB", "estimate"), -8.55, 2)
check("Burned: sinuosity estimate", ap("Burned catchments", "SINUOSITY_mean", "estimate"), -48.41, 2)
check("Burned: deciduous shrub estimate", ap("Burned catchments", "DeciduousShrub_2015_MEAN", "estimate"), 0.46, 2)
check("Burned: forb estimate", ap("Burned catchments", "Forb_2015_MEAN", "estimate"), 2.33, 2)
check("All: sinuosity estimate (positive)", ap("All catchments", "SINUOSITY_mean", "estimate"), 28.98, 2)

# Variance inflation factors (checks the Methods statement that all VIF < 10)
# vif_check(): VIF = how much each covariate overlaps the others (>10 = strong overlap)
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


# ==============================================================================
# PART 4. Figure 4: model-averaged predictions, burned valley bottoms
# ==============================================================================

# Top burned models and their weights
burned_set <- sets[["Burned catchments"]]
burned_top <- top_models(burned_set, "Burned catchments")
w_top <- sapply(burned_top, aicc); w_top <- exp(-0.5 * (w_top - min(w_top))); w_top <- w_top / sum(w_top)
# Median of every covariate (others are held at these in each panel)
all_vars <- unique(unlist(cand_all[names(burned_top)]))
medians  <- sapply(burned_set$data[, all_vars], median)

# modavg_pred(): predicted pond density at values x of one covariate, averaged over the top
# models that contain that covariate (weights renormalized, same as the Appendix S4 averages),
# with a 90% band
modavg_pred <- function(var, x, level = 0.90) {   # as AICcmodavg::modavgPred, conditional on var
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
cat("\nPART 4 CHECKPOINTS (Figure 4):\n")
check_set("Figure 4: highest-R2 top model (Geo + Hydro + Fire)", best_r2, "netmap.fire.vb")
check("Figure 4: panels (15 + deciduous shrub + forb)", length(fig4_vars), 17, digits = 0)
# Each panel's line has the Appendix S4 slope, and a flat line fits inside the band only for unimportant covariates
fig4_ok <- sapply(fig4_vars, function(v) {
  obs <- burned_set$data[[v]]; pd <- modavg_pred(v, seq(min(obs), max(obs), length.out = 100))
  slope <- (pd$fit[100] - pd$fit[1]) / (pd$x[100] - pd$x[1])
  est <- appendix$estimate[appendix$dataset == "Burned catchments" & appendix$parameter == v]
  flat_fits <- max(pd$lower) <= min(pd$upper)
  isTRUE(all.equal(slope, est, tolerance = 1e-6)) && (flat_fits == !(v %in% fig4_imp))
})
check("Figure 4: slopes = Appendix S4; blue bands exclude a flat line", sum(fig4_ok), 17, digits = 0)
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


# ==============================================================================
# PART 5. Figure 5 and tributary pond densities
# ==============================================================================
# Corrections relative to the original Figure 5 code:
#   1. Beaver density per tributary (HUC12) = total ponds / total valley-bottom
#      area in the tributary. The original divided the summed ponds by the MEAN
#      area of one valley bottom (summarize(VB_Area_SqKm = mean(...))).
#   2. Percent burned per tributary = 100 x burned valley-bottom area / total
#      valley-bottom area. Burned area = MTBS low + moderate + high severity
#      (BS2 + BS3 + BS4), capped at each valley bottom's area, fires that
#      ignited in or before 2017 only.
#   3. Panel B trend lines are fit to percent burned (the original p2
#      geom_smooth() used x = Beaver_Density).
#   4. Panel B shows burned tributaries only.
#   5. Valley bottoms stored as several polygon pieces (same uRCA ID) are
#      combined: piece areas and pond counts are summed; fire attributes
#      (joined by uRCA and repeated on each piece) are counted once. The
#      original summary also produced several rows per tributary, so its
#      summary statistics were weighted by the number of valley bottoms.
#   6. Axis label typo fixed ("Habitat"); both panels use 10-unit y-axis steps.
# Limitation (same as the original): valley bottoms that straddle two
# tributaries are counted in full in each.

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
  labs(title = "(a)", x = expression("Beaver pond density (ponds/km"^2*")"),
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


cat("\nPART 5 CHECKPOINTS (Results last paragraph, Figure 5, response to R1):\n")
# Checkpoints: Results last paragraph and Figure 5
check("Tributaries in panel A", dens_summary$n_tributaries, 133, 0)
check("Tributary density minimum (ponds/km2)", dens_summary$min, 0.01, 2)
check("Tributary density maximum", dens_summary$max, 5.19, 2)
check("Tributary density mean", dens_summary$mean, 0.79, 2)
check("Tributary density SD", dens_summary$sd, 0.91, 2)
for (b in list(c("chat", "Chatanika", 1.16), c("chen", "Chena", 0.89), c("salc", "Salcha", 0.57),
               c("good", "Goodpaster", 0.55), c("shaw", "Shaw Creek", 0.43)))
  check(paste(b[2], "mean density"), basin_summary$mean_density[basin_summary$basin == b[1]], as.numeric(b[3]), 2)
check("Burned tributaries in panel B", nrow(burned5) / 4, 74, 0)
# Results: basins with the highest pond density also had the most valley-bottom area burned
check("Chatanika mean % valley-bottom area burned", basin_summary$mean_pct_burned[basin_summary$basin == "chat"], 13, 0)
check("Chena mean % valley-bottom area burned", basin_summary$mean_pct_burned[basin_summary$basin == "chen"], 13, 0)
check("Other basins: lowest mean % burned", min(basin_summary$mean_pct_burned[!basin_summary$basin %in% c("chat", "chen")]), 2, 0)
check("Other basins: highest mean % burned", max(basin_summary$mean_pct_burned[!basin_summary$basin %in% c("chat", "chen")]), 10, 0)

# Habitat lost to beaver dams in each tributary at 20% passage, relative to the same
# tributary with no dams (100% passage). PASS_LENGTH_KM is in meters in these files.
# Habitat lost per tributary at 20% passage; worst first (Paldo Creek)
p100 <- pop[pop$POP_rate == 100, c("HUC12", "basin", "PASS_LENGTH_KM")]
p20  <- pop[pop$POP_rate == 20,  c("HUC12", "PASS_LENGTH_KM")]
trib_loss <- merge(p100, p20, by = "HUC12", suffixes = c("_100", "_20"))
trib_loss$lost_km  <- (trib_loss$PASS_LENGTH_KM_100 - trib_loss$PASS_LENGTH_KM_20) / 1000
trib_loss$pct_lost <- 100 * (1 - trib_loss$PASS_LENGTH_KM_20 / trib_loss$PASS_LENGTH_KM_100)
trib_loss <- trib_loss[order(-trib_loss$pct_lost), ]
print(head(trib_loss, 10), row.names = FALSE)          # HUC12 190803050204 = Paldo Creek (Salcha)
write.csv(trib_loss, file.path(fig_dir, "tributary_habitat_loss_POP20.csv"), row.names = FALSE)
check("Tributaries in the fish passage simulation", nrow(trib_loss), 201, 0)
check("Largest tributary loss at 20% POP (%)", trib_loss$pct_lost[1], 98.7, 1)
check("Largest tributary loss at 20% POP (km)", trib_loss$lost_km[1], 52, 0)
check("Tributaries losing more than half at 20% POP", sum(trib_loss$pct_lost > 50), 10, 0)
check("Basin totals: potential habitat, all basins (km; Table 3)", sum(p100$PASS_LENGTH_KM) / 1000, 4526, 0)
check("Panel B: smallest P value (all > 0.05)", min(fire_fits$p_value), 0.07, 2)

# Table 3: available habitat by basin and passage rate. Percentages are the change
# from potential habitat (100% passage); the Total row is calculated from the summed
# lengths, not averaged across basins (caption corrected in Revision 1).
# Table 3: km per basin and passage rate, plus a Total row and % change
table3 <- pop %>% group_by(basin, POP_rate) %>% summarise(km = sum(PASS_LENGTH_KM) / 1000, .groups = "drop")
table3 <- bind_rows(table3, table3 %>% group_by(POP_rate) %>% summarise(km = sum(km)) %>% mutate(basin = "Total"))
table3 <- table3 %>% group_by(basin) %>% mutate(pct_change = 100 * (km / km[POP_rate == 100] - 1)) %>% ungroup()
print(as.data.frame(table3 %>% mutate(km = round(km), pct_change = round(pct_change, 1))))
write.csv(table3, file.path(tab_dir, "Table 3 basin habitat (Revision 1).csv"), row.names = FALSE)
# Checkpoints: Table 3 values and the km lost in Chatanika and Chena
t3 <- function(b, r) table3$pct_change[table3$basin == b & table3$POP_rate == r]
check("Table 3: Chena, 50% POP (%; corrected from -12.9)", t3("chen", 50), -9.4, 1)
check("Table 3: Chatanika, 50% POP (%)", t3("chat", 50), -12.9, 1)
check("Table 3: Total, 80% POP (%)", t3("Total", 80), -1.1, 1)
check("Table 3: Total, 50% POP (%)", t3("Total", 50), -6.2, 1)
check("Table 3: Total, 20% POP (%)", t3("Total", 20), -11.5, 1)
check("Table 3: Total available habitat, 20% POP (km)", table3$km[table3$basin == "Total" & table3$POP_rate == 20], 4006, 0)
check("Results: Chatanika habitat lost at 20% POP (km; corrected from 178)", table3$km[table3$basin == "chat" & table3$POP_rate == 100] - table3$km[table3$basin == "chat" & table3$POP_rate == 20], 183, 0)
check("Results: Chena habitat lost at 20% POP (km; corrected from 188)", table3$km[table3$basin == "chen" & table3$POP_rate == 100] - table3$km[table3$basin == "chen" & table3$POP_rate == 20], 187, 0)
check("Table 3: Chena potential habitat (km; corrected from 1,369)", table3$km[table3$basin == "chen" & table3$POP_rate == 100], 1368, 0)


# ==============================================================================
# PART 6. Independent cross-check with AICcmodavg and rcompanion
# ==============================================================================
# Recalculates Table 2 and Appendix S4 with the same package as the original
# analysis (AICcmodavg) and base R, without using this script's own aicc(),
# r2_adj() or modavg_param() functions.
# Only runs if AICcmodavg is installed: redo Table 2 and Appendix S4 with the package; differences should be 0
if (requireNamespace("AICcmodavg", quietly = TRUE)) {
  cat("\nPART 6 CHECKPOINTS: AICcmodavg\n")
  for (s6 in names(sets)) {
    at <- as.data.frame(AICcmodavg::aictab(cand.set = sets[[s6]]$fits, modnames = names(sets[[s6]]$fits)))
    mine <- table2[table2$dataset == s6, ]
    mine <- mine[match(at$Modnames, mine$model), ]
    check(paste(s6, "- largest AICc difference vs aictab()"), max(abs(at$AICc - mine$AICc)), 0, 6)
    check(paste(s6, "- largest weight difference vs aictab()"), max(abs(at$AICcWt - mine$weight)), 0, 6)
    check(paste(s6, "- largest K difference vs aictab()"), max(abs(at$K - mine$K)), 0, 0)
    top <- top_models(sets[[s6]], s6)
    d6 <- c()
    for (p in appendix$parameter[appendix$dataset == s6]) {
      has <- sapply(top, function(m) p %in% names(coef(m)))
      ma <- AICcmodavg::modavg(cand.set = top[has], parm = p, modnames = names(top)[has], conf.level = 0.90)
      a <- appendix[appendix$dataset == s6 & appendix$parameter == p, ]
      d6 <- c(d6, abs(ma$Mod.avg.beta - a$estimate), abs(ma$Uncond.SE - a$uncond_SE),
              abs(ma$Lower.CL - a$lower_90), abs(ma$Upper.CL - a$upper_90))
    }
    check(paste(s6, "- largest Appendix S4 difference vs modavg()"), max(d6), 0, 6)
  }
} else cat("\nPART 6: install.packages('AICcmodavg') to run the AICcmodavg cross-check.\n")

# Redo R2 and adjusted R2 with summary(lm()) (same model refit as an ordinary linear model)
cat("\nPART 6 CHECKPOINTS: R2 and adjusted R2 vs summary(lm())\n")
d6 <- c()
for (s6 in names(sets)) for (nm in names(sets[[s6]]$fits)) {
  m  <- sets[[s6]]$fits[[nm]]
  sl <- summary(lm(formula(m), data = model.frame(m)))
  mine <- table2[table2$dataset == s6 & table2$model == nm, ]
  d6 <- c(d6, abs(sl$r.squared - mine$R2), abs(sl$adj.r.squared - mine$adj_R2))
}
check("Largest R2 / adjusted R2 difference vs summary(lm())", max(d6), 0, 6)


# ==============================================================================
# PART 7. Sensitivity runs (not used in the manuscript)
# ==============================================================================
# 7a. Keep the data-dredged model (dredge.vb from the original script) in the
#     burned candidate set. Its covariates were chosen by MuMIn::dredge on the
#     same data, which is why it was dropped.
dredge_vars <- c("GRADIENT_mean", "FP_WIDTH_mean", "Area_km2_VB", "MEANANNCMS_mean", "SINUOSITY_mean",
                 "STRM_ORDER_max", "WIDTH_M_mean", "StrmPow_mean")
set_labels["dredge.vb"] <- "Dredge (reduced geomorphology, hydrology)"
s7a <- selection_table(fit_set(burned_VB, c(cand_all, list(dredge.vb = dredge_vars))), "Burned + dredge model")
cat("\nPART 7a. Burned catchments with the dredge model kept (n =", s7a$n[1], "):\n")
print(s7a %>% transmute(covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
                        weight = round(weight, 2)), row.names = FALSE)

# 7b. Original burn severity definition (class area / valley-bottom area, with
#     percent no burn as a covariate) on the corrected valley bottoms.
vb_7b <- burned_VB
orig_sev <- vb_raw[!duplicated(vb_raw$uRCA), c("uRCA", "percent_severe_burn", "percent_moderate_burn",
                                                "percent_mild_burn", "percent_no_burn")]
vb_7b[, c("percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")] <-
  orig_sev[match(vb_7b$uRCA, orig_sev$uRCA), -1]
vb_7b[is.na(vb_7b$Fire_Event_Area_Percent_of_uRCA),
      c("percent_severe_burn", "percent_moderate_burn", "percent_mild_burn", "percent_no_burn")] <- NA
cand_7b <- lapply(cand_all, function(x) if (any(x %in% fire)) c(x, "percent_no_burn") else x)
s7b <- selection_table(fit_set(vb_7b, cand_7b), "Burned, original severity")
cat("\nPART 7b. Burned catchments with the original severity definition (n =", s7b$n[1], "):\n")
print(s7b %>% transmute(covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
                        weight = round(weight, 2)), row.names = FALSE)


# 7c. Collinearity: refit the burned candidate set without stream width and depth
#     (both strongly correlated with mean annual discharge; see "VIF check.csv").
#     Reported in the Methods and Results as a sensitivity check.
drop_wd <- function(x) setdiff(x, c("WIDTH_M_mean", "DEPTH_M_mean"))
fs_7c  <- fit_set(burned_VB, lapply(cand_all, drop_wd))
s7c    <- selection_table(fs_7c, "Burned, no width/depth")
top_7c <- fs_7c$fits[s7c$model[s7c$weight >= 0.05]]
ma_7c  <- do.call(rbind, lapply(unique(unlist(lapply(top_7c, function(m) names(coef(m))[-1]))),
                                function(p) modavg_param(top_7c, p)))
ma_7c$important <- ma_7c$lower_90 > 0 | ma_7c$upper_90 < 0
cat("\nPART 7c. Burned catchments without stream width and depth (n =", s7c$n[1], "):\n")
print(s7c %>% transmute(covariate_set, R2 = round(R2, 2), adj_R2 = round(adj_R2, 2), AICc = round(AICc, 1),
                        weight = round(weight, 2)), row.names = FALSE)
print(ma_7c %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
cat("PART 7c CHECKPOINTS (Methods/Results sensitivity sentence):\n")
# Checkpoints for the Methods/Results sensitivity sentence
check_set("Severe burn and valley bottom area still important; sinuosity not",
          intersect(ma_7c$parameter[ma_7c$important], c("percent_severe_burn", "Area_km2_VB", "SINUOSITY_mean")),
          c("percent_severe_burn", "Area_km2_VB"))
check("Severe burn estimate still positive (sign)", sign(ma_7c$estimate[ma_7c$parameter == "percent_severe_burn"]), 1, 0)
check("Valley bottom area estimate still negative (sign)", sign(ma_7c$estimate[ma_7c$parameter == "Area_km2_VB"]), -1, 0)
# All other covariates have VIF < 10 in every non-global model
max_other_vif <- max(unlist(lapply(names(sets), function(s7) sapply(setdiff(names(sets[[s7]]$fits), "global.vb.reduced"),
  function(nm) {
    v <- cand_all[[nm]]
    if (length(v) < 2) return(1)
    x <- vif_check(sets[[s7]]$data, v)
    max(x[!names(x) %in% c("WIDTH_M_mean", "DEPTH_M_mean", "MEANANNCMS_mean")])
  }))))
check("Max VIF of other covariates, all non-global models, < 10 (Methods)", max_other_vif < 10, 1, 0)


# ==============================================================================
# PART 8. Appendix S2: grayling distribution thresholds (Table S1, Figure S1)
# ==============================================================================
# The potential fish habitat thresholds (mean annual flow > 0.06 m3/s, gradient
# < 4%) were set from the stream reaches occupied by grayling in the Alaska
# Freshwater Fish Inventory (Yukon and Kuskokwim basins; n = 899 observations).
# The data file has two NetMap flow estimates. The 0.06 m3/s cutoff is the 10th
# percentile of MEANANNCMS_Brabets (the column used in grayling_dist_4Will.R);
# for MEANANNCMS_MEAN, 0.06 m3/s would fall below the 1st percentile. Gradient
# is stored as a proportion (0.04 = 4%).
# Corrections to the published Appendix S2: the table mixed the two flow
# estimates (min and 95th percentile from Brabets, mean and median from
# MEANANNCMS_MEAN), listed the 10th percentile as the 5th, and had a typo in the
# maximum (105.84 for 1,056.84); the gradient row did not match the data; the
# text described the gradient cutoff as the 5th percentile (it is the 95th,
# 3.6%, rounded to 4%); the flow histogram showed MEANANNCMS_MEAN with the axis
# label "m/s^2".
cat("\nPART 8: Appendix S2 grayling distribution thresholds\n")
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
# Checkpoints: the cutoffs and every Table S1 value
check("Appendix S2: grayling observations", nrow(gr), 899, 0)
check("Appendix S2: flow 10th percentile = cutoff (m3/s)", unname(quantile(flow, 0.10)), 0.06, 2)
check("Appendix S2: gradient 95th percentile (%)", unname(quantile(grad, 0.95)), 3.63, 2)
check("Appendix S2: flow minimum", min(flow), 0.02, 2)
check("Appendix S2: flow 5th percentile", unname(quantile(flow, 0.05)), 0.04, 2)
check("Appendix S2: flow mean", mean(flow), 9.75, 2)
check("Appendix S2: flow median", median(flow), 1.14, 2)
check("Appendix S2: flow 95th percentile", unname(quantile(flow, 0.95)), 28.99, 2)
check("Appendix S2: flow maximum", max(flow), 1057.38, 2)
check("Appendix S2: gradient minimum (%)", min(grad), 0.01, 2)
check("Appendix S2: gradient 5th percentile (%)", unname(quantile(grad, 0.05)), 0.05, 2)
check("Appendix S2: gradient mean (%)", mean(grad), 0.90, 2)
check("Appendix S2: gradient median (%)", median(grad), 0.35, 2)
check("Appendix S2: gradient maximum (%)", max(grad), 11.39, 2)

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


# ==============================================================================
cat("\n==============================================================\n")
cat("CHECKPOINTS:", n_pass, "PASS,", n_check, "to check\n")
cat("==============================================================\n")
