# Script analyzing if there is a link between metabolic rates and activity from acclimation
# Activity of interest is time spent moving, distance moved, and acceleration
# Add fishing selection to test a linkage between harvesting and metabolic rates

library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)


### Import of data of interest
# Metabolic rates
MR_raw <- read_excel("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Raw_calculated_rates/All_phys_scaled_cleaned.xlsx") |> 
  mutate(across(c(4:7,14:18), ~ round(as.numeric(.), digits = 3))) |> 
  mutate(AS_H = MMR_Hypoxic_scaled - SMR_Hypoxic_scaled) |> 
  mutate(AS_N = MMR_Normoxic_scaled - SMR_Normoxic_scaled)
write.csv(MR_raw, file = "Metabolic_rates_final.csv",row.names = FALSE, quote=FALSE)

# Fishing selection
Harvest <- read_xlsx("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Tagsheet_raw_DATA.xlsx") |> 
  mutate(Fish_ID = tolower(Fish_ID)) |> 
  select(Harvested, Fish_ID)

# Activity 
# Data in sorted in another script "activity"

# startle response data
stres_df <- read_csv("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Publisering/Data/Behaviour_data/behaviour_all_data.csv") |> 
  select(1,325:345) |> 
  mutate(Fish_ID = tolower(Fish_ID))

################################################################################

# Select data to make it cleaner
activity_df <- activity_long_no_outliers |> 
  select(Fish_ID:Sex,Trial, Metric,Value_no_outliers)

#make wide
activity_df <- activity_df |> 
  pivot_wider(names_from = Trial, values_from = Value_no_outliers)


### Add all variables to new data frame 

Base_df <- left_join(MR_raw, Harvest, by = "Fish_ID") |> 
  left_join(select(activity_df, Fish_ID, Metric:R), by = "Fish_ID") |> 
  rename(Hypoxia = "H",
         Normoxia ="N",
         Recovery = "R")

base_df1 <- left_join(Base_df, stres_df, by = "Fish_ID") 

# Save in csv file
write.csv(Base_df, file = "Metabolic_rates_activity.csv",row.names = FALSE, quote=FALSE)



################################################################################
### CORRELATION MATRIX

# -> Check high MMR in normoxia == high in hypoxia 
# -> Activity with metabolic rates, especially SMR (since smr gets lower in hypoxia)
# -> Harvest and metabolic rates


# subset into acclimation and prep for correlation 
hypo_df <- Base_df |> 
  filter(Acclimation == "Hypoxia acclimated") |> 
  select(-c(2, 4:14)) |> 
  rename(
    SMR_N = "SMR_Normoxic_scaled",
    SMR_H = "SMR_Hypoxic_scaled",
    MMR_N = "MMR_Normoxic_scaled",
    MMR_H = "MMR_Hypoxic_scaled")

norm_df <- Base_df |> 
  filter(Acclimation == "Normoxia acclimated") |> 
  select(-c(2, 4:14)) |> 
  rename(
  SMR_N = "SMR_Normoxic_scaled",
  SMR_H = "SMR_Hypoxic_scaled",
  MMR_N = "MMR_Normoxic_scaled",
  MMR_H = "MMR_Hypoxic_scaled")

## hypoxia acclimated and speed
Hypo_speed <- hypo_df |> 
  filter(Metric == "Speed") |> 
  select_if(is.numeric)
cor_speed_h<-cor(Hypo_speed, use="complete.obs")
H_speed <- ggcorrplot(cor_speed_h, type="lower", lab=T, title = "Hypoxia acclimated and filtered by speed")

## Normoxia acc. and speed
norm_speed <- norm_df |> 
  filter(Metric == "Speed") |> 
  select_if(is.numeric)
cor_speed_n<- cor(norm_speed, use= "complete.obs")  
N_speed <- ggcorrplot(cor_speed_n, type = "lower", lab = T, title = "Normoxia acclimated and filtered by speed")

library(patchwork)
H_speed|N_speed

## hypoxia acc and time
Hypo_time <- hypo_df |> 
  filter(Metric == "Time") |> 
  select_if(is.numeric)
cor_time_h<-cor(Hypo_time, use="complete.obs")
H_time <- ggcorrplot(cor_time_h, type="lower", lab=T, title = "HHypoxia acclimated and filtered by time")

## Normoxia acc and time
norm_time <- norm_df |> 
  filter(Metric == "Time") |> 
  select_if(is.numeric)
cor_time_n<- cor(norm_time,use="complete.obs")  
N_time <- ggcorrplot(cor_time_n, type = "lower", lab = T, title = "Normoxia acclimated and filtered by time")

H_time|N_time

cor_plot <- (H_speed|N_speed)/(  H_time|N_time)


ggsave("cor_plot.png", width = 12, height = 10, units = "in", dpi = 300)



################################################################################
### STATISTICAL ANALYSES

# 1) Is there a relationship between treatment, acclimation with: metabolic rates, activity?
# 2) Is there a relationship between metabolic rates and activity with acclimation/treatment?


### Metabolic rate analysis
# split SMR and MMR for analysis

smr_df <- Base_df |> 
  pivot_longer(cols = SMR_Hypoxic_scaled:SMR_Normoxic_scaled, names_to = "Treatment", values_to = "smr") |> 
  mutate(Treatment = ifelse(Treatment == "SMR_Normoxic_scaled", "Normoxia", "Hypoxia"),
         Treatment = recode_factor(Treatment, Normoxia = "Normoxia", Hypoxia = "Hypoxia"), # set reference, compare to normoxia
         Acclimation = recode_factor(Acclimation, "Normoxia acclimated" = "Normoxia acclimated", "Hypoxia acclimated" = "Hypoxia acclimated"))

mmr_df <- Base_df |>
  pivot_longer(cols = MMR_Hypoxic_scaled:MMR_Normoxic_scaled, names_to = "Treatment", values_to = "mmr") |> 
  mutate(Treatment = ifelse(Treatment == "MMR_Normoxic_scaled", "Normoxia", "Hypoxia"),
         Treatment = recode_factor(Treatment, Normoxica = "Normoxia", Hypoxia = "Hypoxia"),
         Acclimation = recode_factor(Acclimation, "Normoxia acclimated" = "Normoxia acclimated","Hypoxia acclimated" = "Hypoxia acclimated"))



## Fit models
hist(smr_df$smr)
smr_df<- subset(smr_df,smr<0.3) # normally distributed

hist(mmr_df$mmr) #already normal distributed


# with interaction - lowest AIC
smr_lme <-lme(smr ~ Treatment*Acclimation+sex+SGR, random = ~1|Fish_ID, na.action=na.omit, data = smr_df) #-1542
qqnorm(resid(smr_lme))

mmr_lme <-lme(mmr ~ Treatment*Acclimation+sex+SGR, random = ~1|Fish_ID, na.action=na.omit, data = mmr_df) #-1085
qqnorm(resid(mmr_lme))


smr_lmr <- lmer(smr ~ Treatment*Acclimation+sex+SGR + (1|Fish_ID), na.action=na.omit, data = smr_df, REML = F) 
summary(smr_lmr) # 59 % Proportion of total variance due to individuals

mmr_lmr <- lmer(mmr ~ Treatment*Acclimation+sex+SGR + (1|Fish_ID), na.action=na.omit, data = mmr_df, REML = F)
summary(mmr_lmr) # 65 % 


# continue with full model + interaction for both SMR and MMR
summary(smr_lme)
summary(mmr_lme)


### Activity analysis
# previous code in another script, this is just the main analysis codes

# by time
act_time_lmr <- lmer(Value_no_outliers ~ Trial * Acclimation + Sex + Weight_finished + (1 | Fish_ID), data = long_data_no_outliers_time, REML = TRUE)
summary(act_time_lmr)
anova(act_time_lmr) # trial + acclimation signf.

# by distance 
act_dist_lmr <- lmer(Value_no_outliers ~ Trial * Acclimation + Sex + Weight_finished + (1 | Fish_ID), data = long_data_no_outliers_distance, REML = TRUE)
summary(act_dist_lmr)
anova(act_dist_lmr) # only trial signf.


### Activity and metabolic rates
# smr and activity (only time, bc linear relationship)
 smr_time_df <- smr_df |> 
   pivot_longer(cols = c(Hypoxia, Normoxia, Recovery), names_to = "trial", values_to = "activity") |> 
   filter(Metric == "Time")


smr_act_lme <-lme(smr ~ Treatment*trial + activity, random = ~1|Fish_ID, na.action=na.omit, data = smr_time_df) 

summary(smr_act_lme) 








