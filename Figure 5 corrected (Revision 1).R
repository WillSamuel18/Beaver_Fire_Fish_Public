################################################################################
############ Figure 5 (corrected) and tributary beaver densities ##############
############################ Revision 1, Ecosphere #############################
################################################################################
#
# This script is NEW. It does not change or replace any existing script, data
# file, or figure. The original Figure 5 code is still in
# "Beaver_Grayling_Theroetical_Modeling_Analysis.R" (objects p1 and p2), and the
# original figure files are untouched. Outputs from this script are written to
# "Figures/Revision 1/" so you can compare them with the original and revert.
#
# What it corrects relative to the original code:
#   1. Beaver density per tributary (HUC12) = total ponds / total valley-bottom
#      area in the tributary. The original divided the summed ponds by the MEAN
#      area of a single valley bottom (summarize(VB_Area_SqKm = mean(...))),
#      which inflates density by the number of valley bottoms in the tributary.
#   2. Percent burned per tributary = 100 x burned valley-bottom area / total
#      valley-bottom area. Burned area = MTBS low + moderate + high severity
#      (BS2 + BS3 + BS4), capped at each valley bottom's area, and only for fires
#      that ignited in or before 2017 (the year of the beaver imagery). The
#      original divided summed area by the mean area of one valley bottom, and
#      its ifelse(is.na(...)) handling set most burned tributaries to zero.
#   3. Panel B trend lines are fit to percent burned. In the original p2 the
#      geom_smooth() used x = Beaver_Density, so the lines were not fire trends.
#   4. Panel B shows burned tributaries only.
#   5. Valley bottoms stored as several polygon pieces (same uRCA ID) are combined:
#      piece areas and pond counts are summed; fire attributes (joined by uRCA and
#      repeated on each piece) are counted once. The original summary also
#      produced several rows per tributary, so its summary statistics were
#      weighted by the number of valley bottoms in each tributary.
#   6. Axis label typo fixed ("Habitat"); both panels use 10-unit y-axis steps.
#
# Pond counts use Point_Count, the same field used for the original figure.
#
# Run from the RStudio project (Beaver_Fire_Fish_Public.Rproj) so the relative
# paths below resolve.
################################################################################

library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)

out_dir <- "Figures/Revision 1"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Habitat simulation output (unchanged) ------------------------------------
read_pop <- function(f) {
  x <- read.csv(file.path("Theoretical Passage Data Files", f))
  x[, c("HUC12", "POP_rate", "TOTAL_LENGTH_KM", "PASS_LENGTH_KM", "basin")]
}
pop <- bind_rows(read_pop("POP.100HUC.HAB.dat.csv"),
                 read_pop("POP.80HUC.HAB.dat.csv"),
                 read_pop("POP.50HUC.HAB.dat.csv"),
                 read_pop("POP.20HUC.HAB.dat.csv"))

# Total potential habitat per tributary = habitat length with no dams (POP 100),
# exactly as in the original script
total_len <- pop %>% filter(POP_rate == 100) %>% select(HUC12, Total_Length = TOTAL_LENGTH_KM)
pop <- pop %>%
  left_join(total_len, by = "HUC12") %>%
  mutate(percent_available_habitat = PASS_LENGTH_KM / Total_Length * 100)

# Valley-bottom beaver and fire data, summarized by tributary ----------------
vb <- read.csv("Theoretical Passage Data Files/VB_with_beaver_fire_data.csv")
vb <- vb %>%
  filter(!is.na(HUC12)) %>%
  mutate(RCA_Area_SqKm = as.numeric(RCA_Area_SqKm),
         Point_Count   = as.numeric(Point_Count),
         fire_year     = as.integer(substr(Fire_Event_Ignition_Date, 1, 4)),
         burn_km2_raw  = coalesce(BS2_AreaKm2, 0) + coalesce(BS3_AreaKm2, 0) + coalesce(BS4_AreaKm2, 0)) %>%
  # Some valley bottoms are stored as 2+ polygon pieces with the same uRCA ID (usually
  # the valley bottom plus a tiny sliver). Each piece has its own area and its own
  # pond count, so those are summed. The fire/burn-severity attributes were joined
  # by uRCA, so they repeat on every piece and are taken once.
  group_by(HUC12, uRCA) %>%
  summarize(VB_Area_SqKm = sum(RCA_Area_SqKm, na.rm = TRUE),
            Point_Count  = sum(Point_Count, na.rm = TRUE),
            fire_year    = first(fire_year),
            burn_km2_raw = first(burn_km2_raw),
            .groups = "drop") %>%
  mutate(burned_km2 = ifelse(!is.na(fire_year) & fire_year <= 2017,
                             pmin(burn_km2_raw, VB_Area_SqKm), 0))

huc <- vb %>%
  group_by(HUC12) %>%
  summarize(VB_Area_SqKm   = sum(VB_Area_SqKm, na.rm = TRUE),
            Beaver_Count   = sum(Point_Count, na.rm = TRUE),
            Burned_km2     = sum(burned_km2, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Beaver_Density = Beaver_Count / VB_Area_SqKm,       # ponds per km2 of valley bottom
         Percent_Burned = 100 * Burned_km2 / VB_Area_SqKm)

dat <- pop %>%
  left_join(huc, by = "HUC12") %>%
  filter(Beaver_Density > 0)                                 # same filter as the original figure

dat$POP_rate <- factor(dat$POP_rate, levels = c(100, 80, 50, 20))
pop_cols <- c("100" = "darkblue", "80" = "darkgreen", "50" = "#E69F00", "20" = "darkred")

# Summary statistics for the Results (orig. L387-391) ------------------------
s100 <- dat %>% filter(POP_rate == "100")
dens_summary <- s100 %>%
  summarize(n_tributaries = n(), min = min(Beaver_Density), max = max(Beaver_Density),
            mean = mean(Beaver_Density), sd = sd(Beaver_Density))
basin_summary <- s100 %>% group_by(basin) %>%
  summarize(n_tributaries = n(), mean_density = mean(Beaver_Density),
            n_burned = sum(Percent_Burned > 0), mean_pct_burned = mean(Percent_Burned), .groups = "drop")
print(dens_summary); print(basin_summary)
cat("Burned tributaries in panel B:", sum(s100$Percent_Burned > 0), "of", nrow(s100), "\n")

# Trend statistics for panel B (habitat ~ percent burned, burned tributaries only)
burned <- dat %>% filter(Percent_Burned > 0)
fire_fits <- burned %>% group_by(POP_rate) %>%
  group_modify(~ {
    m <- lm(percent_available_habitat ~ Percent_Burned, data = .x)
    s <- summary(m)
    data.frame(n = nrow(.x), slope = coef(m)[2], p_value = coef(s)[2, 4], r2 = s$r.squared)
  }) %>% ungroup()
print(fire_fits)

# Trend statistics for panel A (habitat ~ beaver pond density, all tributaries)
dens_fits <- dat %>% group_by(POP_rate) %>%
  group_modify(~ {
    m <- lm(percent_available_habitat ~ Beaver_Density, data = .x)
    s <- summary(m)
    data.frame(n = nrow(.x), slope = coef(m)[2], p_value = coef(s)[2, 4], r2 = s$r.squared)
  }) %>% ungroup()
print(dens_fits)

write.csv(huc, file.path(out_dir, "tributary_density_and_burn_corrected.csv"), row.names = FALSE)
write.csv(dens_summary, file.path(out_dir, "tributary_density_summary.csv"), row.names = FALSE)
write.csv(basin_summary, file.path(out_dir, "basin_density_summary.csv"), row.names = FALSE)
write.csv(fire_fits, file.path(out_dir, "panelB_fire_trend_stats.csv"), row.names = FALSE)
write.csv(dens_fits, file.path(out_dir, "panelA_density_trend_stats.csv"), row.names = FALSE)

# Figure 5 (corrected) ------------------------------------------------------
# coord_cartesian() zooms to 0-100 without dropping data, so the fitted lines are
# drawn in full (scale limits would cut the POP 50 line where it reaches 100.2)
y_scale <- list(scale_y_continuous(breaks = seq(0, 100, 10)),
                coord_cartesian(ylim = c(0, 100)))

p1_corr <- ggplot(dat, aes(x = Beaver_Density, y = percent_available_habitat, col = POP_rate)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.5) +
  scale_color_manual(values = pop_cols) +
  y_scale +
  theme_cowplot() +
  labs(title = "A", x = expression("Beaver pond density (ponds/km"^2*")"),
       y = "Percent Available Habitat", col = "Probability of Passage (%)")

p2_corr <- ggplot(burned, aes(x = Percent_Burned, y = percent_available_habitat, col = POP_rate)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.5) +       # fit to percent burned
  scale_color_manual(values = pop_cols) +
  y_scale +
  theme_cowplot() +
  labs(title = "B", x = "Percent Burned", y = "Percent Available Habitat",
       col = "Probability of Passage (%)")

fig5_corr <- (p1_corr + p2_corr) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
fig5_corr

ggsave(file.path(out_dir, "Figure 5 corrected.jpeg"), fig5_corr,
       width = 10, height = 5, units = "in", dpi = 600)
