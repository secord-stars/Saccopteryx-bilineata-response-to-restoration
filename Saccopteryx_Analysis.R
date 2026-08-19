#------------- Setup ---------
library(DHARMa)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(lme4)
library(performance)
library(tidyverse)
library(viridis)
sessionInfo()


#------- Aim 1 - Does Forest Age Predict Activity ------
#1. Load your Level 1 individual observation dataset
df_level1 <- read.csv('/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_1_Filtered_No_Controls.csv')

#2. Format your variables
df_level1$Hardware <- as.factor(df_level1$Hardware)
df_level1$Scaled_Forest_Age <- scale(df_level1$Exact_Forest_Age_Days)

#3. Convert TRUE/FALSE to 1/0 numeric format
df_level1$Detection_Binary <- as.numeric(as.logical(df_level1$Saccopteryx_Present))

# 4. Fit the clean binary model 
aim1_model_clean <- glmer(
  Detection_Binary ~ Scaled_Forest_Age + Hardware + (1 | Point), 
  family = binomial, 
  data = df_level1
)
print("--- AIM 1 RESULTS: REFORESTATION ---")
summary(aim1_model_clean)
performance::r2_nakagawa(aim1_model_clean)
sim_res_aim1_clean <- simulateResiduals(fittedModel = aim1_model_clean, n = 250)
testDispersion(sim_res_aim1_clean)


#------- Aim 2 - Does Habitat Change Activity ---------
#1. Load your Level 1 individual observation dataset
df_controls <- read.csv('/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_1_Full_Merged_Master_With_Controls.csv')

#2. Format your variables
df_controls$Hardware <- as.factor(df_controls$Hardware)
df_controls$Scaled_Forest_Age <- scale(df_controls$Exact_Forest_Age_Days)
df_controls$Binary_Habitat <- ifelse(df_controls$Habitat == "Forest", "Forest", "Restoration")
df_controls$Binary_Habitat <- as.factor(df_controls$Binary_Habitat) # Create a binary habitat column: Forest vs. Non-Forest (Pasture + Restoration)
df_controls$Binary_Habitat <- relevel(df_controls$Binary_Habitat, ref = "Forest") # Set 'Forest' as the reference category so R compares Non-Forest against Forest

#3. Convert TRUE/FALSE to 1/0 numeric format
df_controls$Detection_Binary <- as.numeric(as.logical(df_controls$Saccopteryx_Present))

#4. Test the model for habitat
aim3_model <- glmer(
  Detection_Binary ~ Binary_Habitat + Hardware + (1 | Point), 
  family = binomial, 
  data = df_controls
)

print("--- AIM 2 RESULTS: HABITAT ---")
summary(aim3_model)
performance::r2_nakagawa(aim3_model)
sim_res_aim3 <- simulateResiduals(fittedModel = aim3_model, n = 250)
testDispersion(sim_res_aim3)


#------- visualisations and plots --------

# Global Theme Adjustments for Thesis Quality
thesis_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#install.packages("ggeffects") # Run this line once if you don't have it installed

#------- Figure 1: truncating the audio moth line -------
# Extract predictions using ggpredict - this creates the smooth relationship line
pred_aim1 <- ggpredict(aim1_model_clean, terms = c("Scaled_Forest_Age [all]", "Hardware"))

# We want to show a scatterplot of the site level detections, use level 3 with aggregates rather than level 1
df_level3_aim1 <- read.csv('/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_3_Site_Year_GLMM.csv')
df_level3_aim1$Proportion <- df_level3_aim1$Positive_Recordings / df_level3_aim1$Total_Recordings

# Calculate the scaling factors for exact fore age from level 1
mean_age <- mean(df_level1$Exact_Forest_Age_Days, na.rm = TRUE)
sd_age <- sd(df_level1$Exact_Forest_Age_Days, na.rm = TRUE)

# Un-scale the x-axis of the model predictions back into real days
pred_aim1$Real_Days <- (as.numeric(as.character(pred_aim1$x)) * sd_age) + mean_age

# TRUNCATION FIX: Stop prediction lines at the edge of actual observed data
# Calculate maximum observed days for each hardware type from the Level 3 dataset
max_days_am <- max(df_level3_aim1$Average_Forest_Age_Days[df_level3_aim1$Hardware == "AM"], na.rm = TRUE)
max_days_sm <- max(df_level3_aim1$Average_Forest_Age_Days[df_level3_aim1$Hardware == "SM"], na.rm = TRUE)

# Filter the prediction dataset using group (Hardware) and Real_Days (un-scaled x)
pred_aim1_trimmed <- pred_aim1 %>%
  filter(
    (group == "AM" & Real_Days <= max_days_am) |
      (group == "SM" & Real_Days <= max_days_sm)
  )

# Plot the Level 3 Scatter Points WITH the Level 1 Model Predictions!

plot_aim1_combined <- ggplot() +
  # Add the raw scatter points from the Level 3 dataset, site level
  geom_jitter(data = df_level3_aim1, aes(x = Average_Forest_Age_Days, y = Proportion, color = Hardware), 
              alpha = 0.5, size = 3, width = 15, height = 0) +
  
  # Add the CORRECT upward model lines from the TRIMMED Level 1 dataset
  geom_line(data = pred_aim1_trimmed, aes(x = Real_Days, y = predicted, color = group), 
            linewidth = 1.5) +
  
  # Add the confidence intervals for the model lines using the TRIMMED dataset
  geom_ribbon(data = pred_aim1_trimmed, aes(x = Real_Days, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2) +
  
  labs(#title = "Aim 1: Effect of Forest Age on S. bilineata Activity",
    x = "Time Since Reforestation (Days)",
    y = "Proportion of Positive Recordings",
    color = "Hardware", fill = "Hardware") +
  thesis_theme +
  scale_color_manual(values = c("AM" = "#F8766D", "SM" = "#0072B2"), labels = c("AudioMoth","Song meter"), name = "Device") +
  scale_fill_manual(values = c("AM" = "#F8766D", "SM" = "#0072B2"), labels = c("AudioMoth","Song meter"), name = "Device")

print(plot_aim1_combined)

#------- Figure 2: box plots colour coded ------------
# Setup Habitat categories and proportion
df_level3_controls$Binary_Habitat <- ifelse(df_level3_controls$Habitat == "Forest", "Forest", "Restoration")
df_level3_controls$Binary_Habitat <- factor(df_level3_controls$Binary_Habitat, levels = c("Forest", "Restoration"))
df_level3_controls$Proportion <- df_level3_controls$Positive_Recordings / df_level3_controls$Total_Recordings

plot_aim3_visual <- ggplot(df_level3_controls, aes(x = Binary_Habitat, y = Proportion, fill = Hardware)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(color = "black", size = 2, alpha = 0.5, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  labs(#title = "Aim 3: Habitat Preference & Hardware",
       x = "Habitat Type",
       y = "Proportion of Positive Recordings",
       fill = "Hardware") +
  thesis_theme + 
  scale_fill_manual(values = c("AM" = "#F8766D", "SM" = "#0072B2"), labels = c("AudioMoth","Song meter"), name = "Device") +
  guides(fill = guide_legend(override.aes = list(color = c("#F8766D", "#0072B2"))))

print(plot_aim3_visual)

#------- Saving the figures --------
# Define output dimensions for consistent thesis formatting
fig_width <- 8
fig_height <- 5
fig_dpi <- 300
output_folder <- ('/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Figures')

# Save Aim 1
#ggsave("Aim1_Reforestation_THESIS.png", plot = plot_aim1_combined, path = output_folder, width = fig_width, height = fig_height, units = "in", dpi = fig_dpi)

# Save Aim 2
#ggsave("Aim2_Habitat_Preference_THESIS.png", plot = plot_aim3_visual, path = output_folder, width = fig_width, height = fig_height, units = "in", dpi = fig_dpi)



#------- Heatmap figure ---------

# 1. Load CSV with check.names = FALSE to preserve numeric column names
df_wide <- read.csv(
  '/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Model_V6_Heat_Map_Summary_2025_merged.csv',
  check.names = FALSE
)

apply(df_wide[,-1], 2, function(x) c(Median = median(x, na.rm=T), IQR = IQR(x, na.rm=T)))

# 2. Pivot the 4 cohort columns into long format
df_long <- df_wide %>%
  pivot_longer(
    cols = c("2023_AM", "2024_AM", "2024_SM", "2025"),
    names_to = "Cohort",
    values_to = "Detection_Rate"
  )

# 3. Set factor levels for proper axis ordering
df_long$Cohort <- factor(df_long$Cohort, levels = c("2023_AM", "2024_AM", "2024_SM", "2025"))
df_long$Point  <- factor(df_long$Point, levels = rev(sort(unique(df_long$Point))))

# 4. Create formatted label column (hides text for NA cells)
df_long$Label <- ifelse(is.na(df_long$Detection_Rate), "", sprintf("%.1f", df_long$Detection_Rate))

# 5. Build heatmap
plot_heatmap <- ggplot(df_long, aes(x = Cohort, y = Point, fill = Detection_Rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = Label, color = Detection_Rate > 6),
    size = 3.2,
    na.rm = TRUE
  ) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none", labels = c("2023","2024-AM","2024-SM")) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Final\nDetection\nRate (%)",
    na.value = "white"
  ) +
  scale_x_discrete(labels = c(
    "2023_AM" = "2023",
    "2024_AM" = "2024-AM",
    "2024_SM" = "2024-SM",
    "2025"    = "2025"
  )) +
  labs(
    x = "Sampling Cohort",
    y = "Sample Point ID",
    #title = "Site Detection Rates Across Cohorts"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank())

print(plot_heatmap)

# 6. Save figure
#ggsave("Heatmap_Detection_Rates.png", plot = plot_heatmap, path = output_folder, width = 7.5, height = 10, dpi = 300)


#------- Trajectory plot over time ------
# 1. Load data into a separate dataframe variable
df_traj <- read.csv(
  '/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_3_Site_Year_GLMM_With_Controls.csv',
  check.names = FALSE
)

# 2. Aggregate across hardware in 2024 so each site has 1 total proportion per year
df_traj_annual <- df_traj %>%
  group_by(Point, Year) %>%
  summarise(
    Positive_Recordings = sum(Positive_Recordings, na.rm = TRUE),
    Total_Recordings    = sum(Total_Recordings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Proportion = Positive_Recordings / Total_Recordings)

# 3. Build trajectory plot
plot_trajectory <- ggplot(df_traj_annual, aes(x = factor(Year), y = Proportion, group = Point, color = Point)) +
  geom_line(alpha = 0.6, linewidth = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  scale_x_discrete(expand = expansion(mult = c(0.04, 0.04))) +
  labs(
    x = "Sampling Year",
    y = "Proportion of Positive Recordings",
    #title = "Site Detection Trajectories (2023–2025)",
    color = "Sample Point ID"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.35, "cm")
  ) +
  # Split the 29 site legend into 2 neat columns so it fits on the page
  guides(color = guide_legend(ncol = 1))

print(plot_trajectory)

# 4. Save high-res PNG
#ggsave("Figures/Site_Trajectories_1col_nopadding.png", plot = plot_trajectory, path = output_folder, width = 10, height = 5.5, dpi = 300)




#------- To backtransform --------

# If your raw forest age column is called Forest_Age_Days
sd(df_level1$Exact_Forest_Age_Days, na.rm = TRUE)

# Or if stored in years
sd(df_level1$Exact_Forest_Age_Years, na.rm = TRUE)

# 1. Get exact SD from your level 1 data
sd_age <- sd(df_level1$Exact_Forest_Age_Days, na.rm = TRUE)

# 2. Raw model parameters
beta_scaled <- fixef(aim1_model_clean)["Scaled_Forest_Age"]
se_scaled   <- summary(aim1_model_clean)$coefficients["Scaled_Forest_Age", "Std. Error"]

# --- FOR 100 DAYS ---
beta_100 <- beta_scaled * (100 / sd_age)
se_100   <- se_scaled * (100 / sd_age)

or_100      <- exp(beta_100)
ci_lower_100 <- exp(beta_100 - 1.96 * se_100)
ci_upper_100 <- exp(beta_100 + 1.96 * se_100)
pct_increase_100 <- (or_100 - 1) * 100

cat(sprintf("100 Days -> OR: %.2f (95%% CI: [%.2f, %.2f]) | %% Increase: %.1f%%\n", 
            or_100, ci_lower_100, ci_upper_100, pct_increase_100))

# --- FOR 1 YEAR (365 DAYS) ---
beta_365 <- beta_scaled * (365 / sd_age)
se_365   <- se_scaled * (365 / sd_age)

or_365      <- exp(beta_365)
ci_lower_365 <- exp(beta_365 - 1.96 * se_365)
ci_upper_365 <- exp(beta_365 + 1.96 * se_365)
pct_increase_365 <- (or_365 - 1) * 100

cat(sprintf("1 Year   -> OR: %.2f (95%% CI: [%.2f, %.2f]) | %% Increase: %.1f%%\n", 
            or_365, ci_lower_365, ci_upper_365, pct_increase_365))

# Get SD of raw forest age
sd_forest_age <- sd(df_level1$Exact_Forest_Age_Days, na.rm = TRUE)

# Scaled coefficient and SE
beta_scaled <- 0.22039
se_scaled   <- 0.04866

# Define your unit of interest (1 year in days)
delta_x <- 365

# Back-transform
scalar        <- delta_x / sd_forest_age
beta_per_year <- beta_scaled * scalar
se_per_year   <- se_scaled * scalar   # SE scales linearly too

OR_per_year   <- exp(beta_per_year)
CI_lower      <- exp(beta_per_year - 1.96 * se_per_year)
CI_upper      <- exp(beta_per_year + 1.96 * se_per_year)
pct_increase  <- (OR_per_year - 1) * 100

cat(sprintf("SD of forest age: %.1f days\n", sd_forest_age))
cat(sprintf("Beta per year: %.4f\n", beta_per_year))
cat(sprintf("OR per year: %.3f (95%% CI: %.3f–%.3f)\n", 
            OR_per_year, CI_lower, CI_upper))
cat(sprintf("Percentage increase per year: %.1f%%\n", pct_increase))


#------- citations---------
citation("DHARMa")
citation("dplyr")
citation("ggeffects")
citation("ggplot2")
citation("lme4")
citation("performance")
citation("tidyverse")
citation("viridis")



#------- New heatmp -------
library(tidyverse)
library(viridis)

# 1. Load Master Dataset
df_master <- read.csv(
  '/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_1_Full_Merged_Master_With_Controls.csv',
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 2. Filter out artifact points and merge 2025 sub-cohorts
df_clean <- df_master %>%
  filter(!Point %in% c("TU_S1_7")) %>%
  mutate(Cohort_Plot = case_when(
    Cohort %in% c("2025_S1", "2025_S2") ~ "2025",
    TRUE ~ Cohort
  ))

# 3. Robust logical check for positive detections
df_heatmap <- df_clean %>%
  group_by(Point, Cohort_Plot) %>%
  summarise(
    Total_Files = n(),
    Pos_Files   = sum(as.character(Saccopteryx_Present) %in% c("True", "TRUE", "1", TRUE), na.rm = TRUE),
    .groups     = 'drop'
  ) %>%
  mutate(
    Detection_Rate = (Pos_Files / Total_Files) * 100
  )

# 4. Set factor levels for proper axis ordering
df_heatmap$Cohort_Plot <- factor(df_heatmap$Cohort_Plot, levels = c("2023_AM", "2024_AM", "2024_SM", "2025"))
df_heatmap$Point       <- factor(df_heatmap$Point, levels = rev(sort(unique(df_heatmap$Point))))

# 5. Create formatted two-line label
df_heatmap <- df_heatmap %>%
  mutate(
    Label = ifelse(
      is.na(Detection_Rate),
      "",
      sprintf("%.1f%%\n(n=%d)", Detection_Rate, Pos_Files)
    )
  )

# 6. Build High-Resolution Heatmap
plot_heatmap <- ggplot(df_heatmap, aes(x = Cohort_Plot, y = Point, fill = Detection_Rate)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = Label, color = Detection_Rate > 5.0),
    size = 2.8,
    lineheight = 0.85,
    fontface = "bold",
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "white"),
    guide = "none"
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Naive\nDetection\nProbability (%)",
    na.value = "grey95"
  ) +
  scale_x_discrete(labels = c(
    "2023_AM" = "2023 (AM)",
    "2024_AM" = "2024 (AM)",
    "2024_SM" = "2024 (SM)",
    "2025"    = "2025 (SM)"
  )) +
  labs(
    x = "Sampling Cohort & Detector Regime",
    y = "Sample Point ID"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title       = element_text(face = "bold"),
    axis.text.y      = element_text(size = 9),
    axis.text.x      = element_text(face = "bold", size = 11),
    legend.title     = element_text(face = "bold", size = 11),
    legend.position  = "right",
    panel.grid       = element_blank()
  )

print(plot_heatmap)

# 7. Save publication-ready graphic
output_path <- "/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Figures"
ggsave("Heatmap_Detection_Rates_DualLabels.png", plot = plot_heatmap, path = output_path, width = 8, height = 11.5, dpi = 300)



#------- Cecking number of data points -------

library(tidyverse)

df_master <- read.csv('/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_1_Filtered_No_Controls.csv')

# Exclude controls and unplanted points
excluded_points <- c('TU004', 'TU013', 'TU027', 'TU028', 'TU029', 'TU_S1_7')
df_aim1 <- df_master %>% filter(!Point %in% excluded_points)

# 1. Count unique sites
n_distinct(df_aim1$Point)  # Output: 24

# 2. Count heatmap tiles (2025 merged)
df_aim1 %>%
  mutate(Cohort_Merged = ifelse(Cohort %in% c("2025_S1", "2025_S2"), "2025", Cohort)) %>%
  distinct(Point, Cohort_Merged) %>%
  nrow()                   # Output: 80

# 3. Count distinct deployment sessions
df_aim1 %>%
  distinct(Point, Cohort) %>%
  nrow()                   # Output: 84


library(tidyverse)

# Load master dataset with controls
df_controls <- read.csv(
  '/Users/isabellesecord/Library/CloudStorage/OneDrive-ImperialCollegeLondon/AA - MRES PROJECT/Model Training Version 6/Detections/Summary_Reports/Level_1_Full_Merged_Master_With_Controls.csv',
  stringsAsFactors = FALSE
)

# Filter out artifact test files
df_aim2 <- df_controls %>% filter(Point != "TU_S1_7")

# Define Binary Habitat
df_aim2 <- df_aim2 %>%
  mutate(Binary_Habitat = ifelse(Habitat == "Forest", "Forest", "Restoration"))

# 1. Total unique sites by habitat
df_aim2 %>%
  group_by(Binary_Habitat) %>%
  summarise(Unique_Sites = n_distinct(Point))

# 2. Total distinct deployment sessions (matching raw dots on boxplot: n = 101)
df_aim2 %>%
  group_by(Binary_Habitat, Hardware) %>%
  summarise(
    Deployments = n_distinct(Point, Cohort),
    .groups = 'drop'
  )

# Overall total deployments
df_aim2 %>%
  distinct(Point, Cohort) %>%
  nrow() # Output: 101
