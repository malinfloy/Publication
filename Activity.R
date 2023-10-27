# keep the first nine coloums and then keep/remove others
activity_data <- read_csv("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Publisering/Data/Behaviour_data/behaviour_activity.csv") |> 
  select(1:9, matches(paste(c("time", "distance"), collapse = "|"))) |> 
  select(-matches(paste(c("center", "total", "speed", "periphery", "corners", "stationary","raw"), collapse = "|"))) |> 
  mutate(Fish_ID = tolower(Fish_ID))


# Define a function to determine if a fish is acclimated or not:
is_acclimated <- function(fish_id) {
  if (as.integer(substr(fish_id, 1, 1)) %in% c(1, 3, 5, 8, 10, 12)) {
    return("Acclimated")
  } else {
    return("Not Acclimated")
  }
}

# Create a new column "Acclimation" and remove outliers:
activity_data <- activity_data|>
  mutate(Acclimation = sapply(Fish_ID, is_acclimated))|>
  group_by(Acclimation, Sex)|>
  mutate_at(vars(ends_with("moving")), list(~replace(., abs(scale(.)) > 2, NA)))

#Gather the data into a long format and calculate averages:
activity_long <- activity_data|>
  pivot_longer(cols = contains("moving"), names_to = "Variable", values_to = "Value") |>
  mutate(Trial = sub(".*([NHR]).*", "\\1", Variable),
         Metric = ifelse(grepl("distance", Variable), "Distance", 
                         ifelse(grepl("speed", Variable), "Speed", "Time")),
         Order = substr(Variable, nchar(Variable), nchar(Variable))) |>
  group_by(Fish_ID, Acclimation, Sex, Harvested, Weight_finished, Length_finished, Trial, Metric) |>
  summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop")

  
# Create a new dataset without outliers --- contains speed, distance and time metrics
activity_long_no_outliers <- activity_long|>
  group_by(Trial, Metric, Acclimation)|>
  mutate(Value_no_outliers = ifelse(Value > (quantile(Value, 0.75) + 1.5 * IQR(Value)) | 
                                      Value < (quantile(Value, 0.25) - 1.5 * IQR(Value)), NA, Value))|>
  ungroup()


#subset to contain only speed metric
speed_data <- activity_long |> filter(Metric == "Speed")


# Filter data to only include the speed metric
speed_no_outliers <- activity_long_no_outliers|>
  filter(Metric == "Speed")

# Update the Trial factor levels
speed_no_outliers$Trial <- factor(speed_no_outliers$Trial, levels = c("N", "H", "R"))



####################################################################################################################################
########################################################### PLOTS ##################################################################
library(scales)

# Hypoxia acclimated plot
H_speedplot<- ggplot(speed_no_outliers[speed_no_outliers$Acclimation == "Acclimated", ],
                       aes(x = Trial, y = Value_no_outliers, shape = Sex, fill = Trial)) +
  geom_boxplot(aes(group = Trial), outlier.shape = NA, position = position_dodge(0.8), color = "black") +
  geom_jitter(aes(color = Sex), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1.5) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = alpha(c("purple", "orange", "forestgreen"), 0.5)) +
  scale_shape_manual(values = c("M" = 16, "F" = 17)) +
  labs(title = "B: Fish Speed (Acclimated)", x = "Treatment", y = "", shape = "Sex", color = "Sex", fill = "Treatment") +
  theme_minimal() #+
  #scale_y_continuous(expand = expand_scale(add = c(0, 0.05)), limits = c(0, 160), breaks = seq(0, 160, 20))



# Normoxia acclinmated plot
N_speedplot <- ggplot(speed_no_outliers[speed_no_outliers$Acclimation == "Not Acclimated", ],
                        aes(x = Trial, y = Value_no_outliers, shape = Sex, fill = Trial)) +
  geom_boxplot(aes(group = Trial), outlier.shape = NA, position = position_dodge(0.8), color = "black") +
  geom_jitter(aes(color = Sex), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1.5) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("purple", "orange", "forestgreen")) +
  scale_shape_manual(values = c("M" = 16, "F" = 17)) +
  labs(title = "A: Fish Speed (Not Acclimated)", x = "Treatment", y = "Speed", color = "Sex", fill = "Treatment") +
  theme_minimal() +
  #scale_y_continuous(expand = expand_scale(add = c(0, 0.05)), limits = c(0, 160), breaks = seq(0, 160, 20)) +
  theme(legend.position = "NONE")

# Print plots
print(hypoxia_plot)
print(normoxia_plot)
library(patchwork)

speed_plot <- N_speedplot|H_speedplot 
plot(speed_plot)

ggsave("speed_plot.png", width = 8, height = 6, units = "in", dpi = 300)




####################################################################################################################################
######################################### CORRELATION TIME AND DISTANCE ############################################################
#Correlation test between time and distance:
#Calculate the correlation between time and distance for each treatment
correlation_data <-activity_long |>
  filter(Metric %in% c("Time", "Distance", "Speed")) |>
  pivot_wider(names_from = Metric, values_from = Value) |>
  rename(Time_Value = Time, Distance_Value = Distance, Speed_Value = Speed) |> 
  select(Fish_ID, Trial, Distance_Value, Time_Value, Speed_Value)
  

# Perform correlation tests
time_distance_correlation <- cor.test(correlation_data$Time_Value, correlation_data$Distance_Value)
print(paste("Correlation coefficient:", round(time_distance_correlation$estimate, 3), "p-value:", round(time_distance_correlation$p.value, 3)))


#Correlation test between length and weight:
length_weight_correlation <- cor.test(activity_data$Length_finished, activity_data$Weight_finished)
print(paste("Correlation coefficient:", round(length_weight_correlation$estimate, 3), "p-value:", round(length_weight_correlation$p.value, 3)))

# Correlation for speed and time/activity
speed_time_cor <- cor.test(correlation_data$Time_Value,correlation_data$Speed_Value)
print(paste("Correlation coefficient:", round(speed_time_cor$estimate, 3), "p-value:", round(speed_time_cor$p.value, 3)))

# Correlation for speed and distance
speed_distance_cor <- cor.test(correlation_data$Distance_Value,correlation_data$Speed_Value)
print(paste("Correlation coefficient:", round(speed_distance_cor$estimate, 3), "p-value:", round(speed_distance_cor$p.value, 3)))


# Scatter plot with regression line for Time_Value and Distance_Value
time_distance_plot <- ggplot(correlation_data, aes(x = Time_Value, y = Distance_Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Time and Distance",
       x = "Time", y = "Distance") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Correlation coefficient:", round(time_distance_correlation$estimate, 3)),
           size = 4)

# Scatter plot with regression line for Time and Speed
time_speed_plot <- ggplot(correlation_data, aes(x =Time_Value , y = Speed_Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Time and Speed",
       x = "Time", y = "Speed") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Correlation coefficient:", round(speed_time_cor$estimate, 3)),
           size = 4)

# Scatter plot with regression line for Distance and Speed
distance_speed_plot <- ggplot(correlation_data, aes(x =Distance_Value , y = Speed_Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Distance and Speed",
       x = "Distance", y = "Speed") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Correlation coefficient:", round(speed_distance_cor$estimate, 3)),
           size = 4)

# Scatter plot with regression line for Length_finished and Weight_finished
length_weight_plot <- ggplot(activity_data, aes(x = Length_finished, y = Weight_finished)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Length and Weight",
       x = "Length", y = "Weight") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Correlation coefficient:", round(length_weight_correlation$estimate, 3)),
           size = 4)

# Print the plots
print(time_distance_plot)
print(length_weight_plot)

time_distance_plot|length_weight_plot
time_distance_plot|time_speed_plot
time_speed_plot|distance_speed_plot

###################################################################################################################################
########################################################### DESCRIPTIVE ###########################################################
#Descriptive statistics:
library(readxl)
library(tidyr)
library(dplyr)
library(moments)  # for skewness() and kurtosis()

# Gather the data into a long format and calculate averages:
activity_long <- activity_data|>
  pivot_longer(cols = contains("moving"), names_to = "Variable", values_to = "Value") |>
  mutate(Trial = sub(".*([NHR]).*", "\\1", Variable),
         Metric = ifelse(grepl("distance", Variable), "Distance", 
                         ifelse(grepl("speed", Variable), "Speed", "Time")),
         Order = substr(Variable, nchar(Variable), nchar(Variable))) |>
  group_by(Fish_ID, Acclimation, Sex, Harvested, Weight_finished, Length_finished, Trial, Metric) |>
  summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop")

# Create the Treatment column
activity_long <- activity_long|>
  mutate(Treatment = case_when(
    Trial == "H" ~ "Hypoxia",
    Trial == "N" ~ "Normoxia",
    Trial == "R" ~ "Recovery"
  ))

# Calculate summary statistics on time spent moving
summary_data <- activity_long|>
  group_by(Fish_ID, Acclimation, Sex, Weight_finished, Treatment)|>
  summarize(MeanTimeSpentMoving = mean(Value, na.rm = TRUE),
            SDTimeSpentMoving = sd(Value, na.rm = TRUE),
            SkewnessTimeSpentMoving = skewness(Value, na.rm = TRUE),
            KurtosisTimeSpentMoving = kurtosis(Value, na.rm = TRUE),
            .groups = "drop")


View(summary_data)
print(summary_data, n=289, width = 420)

# Save summary_data as a tab-separated text file
write.table(summary_data, file = "summary_data.txt", sep = "\t", row.names = FALSE)

# Calculate summary statistics for each variable of interest
overall_sd <- sd(summary_data$MeanTimeSpentMoving)
overall_skewness <- mean(summary_data$SkewnessTimeSpentMoving)
overall_kurtosis <- mean(summary_data$KurtosisTimeSpentMoving)

# Calculate sd, skewness, and kurtosis for each Acclimation group
summary_data_acclimated <- summary_data|>
  group_by(Acclimation)|>
  summarize(SD = sd(MeanTimeSpentMoving, na.rm = TRUE),
            Skewness = mean(SkewnessTimeSpentMoving, na.rm = TRUE),
            Kurtosis = mean(KurtosisTimeSpentMoving, na.rm = TRUE),
            .groups = "drop")

#
print(overall_sd)
print(overall_kurtosis)
print(overall_skewness)


# Print the results
cat("Overall SD of Mean Time Spent Moving:", overall_sd, "\n")
cat("Overall Skewness of Time Spent Moving:", overall_skewness, "\n")
cat("Overall Kurtosis of Time Spent Moving:", overall_kurtosis, "\n")

# Calculate sd, skewness, and kurtosis for each Acclimation group and Treatment
summary_data_acclimated_treatment <- summary_data|>
  group_by(Acclimation, Treatment)|>
  summarize(SD = sd(MeanTimeSpentMoving, na.rm = TRUE),
            Skewness = mean(SkewnessTimeSpentMoving, na.rm = TRUE),
            Kurtosis = mean(KurtosisTimeSpentMoving, na.rm = TRUE),
            .groups = "drop")

# Print the summary_data_acclimated_treatment
print(summary_data_acclimated_treatment)

# Median time spent moving
median_time <- aggregate(Value_no_outliers ~ Acclimation + Trial, data = long_data_no_outliers_time, FUN = median)
print(median_time)

# Mean time spent moving
mean_time <- aggregate(Value_no_outliers ~ Acclimation + Trial, data = long_data_no_outliers_time, FUN = mean)
print(mean_time)

# Mean time spent moving
mean_time_acclimated <- aggregate(Value_no_outliers ~ Acclimation, data = long_data_no_outliers_time, FUN = mean)
print(mean_time_acclimated)

# Median time spent moving
median_time_acclimated <- aggregate(Value_no_outliers ~ Acclimation, data = long_data_no_outliers_time, FUN = median)
print(median_time_acclimated)

# Print the summary_data_acclimated
print(summary_data_acclimated)

# Overall mean time spent moving
overall_mean_time <- aggregate(Value_no_outliers ~ 1, data = long_data_no_outliers_time, FUN = mean)
print(overall_mean_time)

# Overall median time spent moving
overall_median_time <- aggregate(Value_no_outliers ~ 1, data = long_data_no_outliers_time, FUN = median)
print(overall_median_time)

####################################################################################################################################
####################################################### STATISTICAL TESTS ##########################################################
############################################ LINEAR MODEL ##########################################################################

#First we fit the full model
modelfull <- lmer(Value_no_outliers ~ Trial * Acclimation + Sex + Weight_finished + (1 | Fish_ID), data = long_data_no_outliers_time, REML = FALSE)

#Without interaction between trial and acclimation
modelwointeraction <- lmer(Value_no_outliers ~ Trial + Acclimation + Sex + Weight_finished + (1 | Fish_ID), data = long_data_no_outliers_time, REML = FALSE)

#Without weight
modelwoweigth <- lmer(Value_no_outliers ~ Trial + Acclimation + Sex + (1 | Fish_ID), data = long_data_no_outliers_time, REML = FALSE)

#Without sex
modelwosex <- lmer(Value_no_outliers ~ Trial + Acclimation + Weight_finished + (1 | Fish_ID), data = long_data_no_outliers_time, REML = FALSE)

#Then we compare
extractAIC(modelfull) - extractAIC(modelwointeraction)

extractAIC(modelwointeraction) - extractAIC(modelwoweigth)

extractAIC(modelwointeraction) - extractAIC(modelwosex)

#Full model is the best (borderline)

library(nlme)
modelfinal <- lme(Value_no_outliers ~ Trial * Acclimation + Sex + Weight_finished, random= ~1 | Fish_ID, na.action = na.omit, data = long_data_no_outliers_time)
summary(modelfinal)

modelfull <- lmer(Value_no_outliers ~ Trial * Acclimation + Sex + Weight_finished + (1 | Fish_ID), data = long_data_no_outliers_time, REML = TRUE)
summary(modelfull)

#Total variance
479+518.2 #997.2

#Portion of variance due to individual 
479/997.2*100 #48.0345

#Post-hoc test ANOVA
library(lmerTest)
anova(modelfinal)

# Fit a qq plot and test
library(lme4)  # If not already loaded

standardized_resid <- resid(modelfinal, type = "pearson")
qqnorm(standardized_resid)
qqline(standardized_resid, col = "red")



