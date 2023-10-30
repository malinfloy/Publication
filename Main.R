# Import and exploration of data

###### IMPORT DATA - METABOLIC RATES, ACTIVITY, HARVESTING
# Metabolic rates
library(readxl)
library(tidyverse)

MR_raw <- read_excel("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Raw_calculated_rates/All_phys_scaled_cleaned.xlsx") |> 
  mutate(across(c(4:7,14:18), ~ round(as.numeric(.), digits = 3))) |> 
  mutate(AS_Hypoxic = MMR_Hypoxic_scaled - SMR_Hypoxic_scaled) |> 
  mutate(AS_Normoxic = MMR_Normoxic_scaled - SMR_Normoxic_scaled) |> 
  mutate(SGR_length = round((((log(Length_end)-log(Length_start))/58)*100), digits = 2))

write.csv(MR_raw, file = "Metabolic_rates_final.csv",row.names = FALSE, quote=FALSE)


### Import of final dataframe with activity, startle response and metabolism
mydata <- read_csv("Masters_data_ver2.csv", col_names = TRUE)|> 
  mutate(Normalized_Time.moving = ifelse(
    Fish_ID %in% c("6mg", "6mb", "10mg", "7fg"), NA, Normalized_Time.moving))

 
# find the 4 outliers, remove them (above)
subset_above40speed <- mydata[which(mydata$Head.speed.moving > 40),]

 
################################################################################
### CORRELATION MATRIX

# -> Check high MMR in normoxia == high in hypoxia 
# -> Activity with metabolic rates, especially SMR (since smr gets lower in hypoxia)

library(ggcorrplot)
library(corrplot)
library(patchwork)
library(psych)

## make wide to show all 3 trials in activity, split between metabolic rates(mr) and activity
# subset mr and make wide
mydata_mr <- mydata |> 
  select(-c(1,6:11,13:17,28,29,33:35)) |> 
  distinct() |> 
  filter(!grepl("Recovery", Treatment)) |> 
  pivot_wider(names_from = Treatment, values_from = c(SMR_scaled, MMR_scaled,AS))

# subset activity into wide format
mydata_elise <- mydata |> 
  select(c(Head.time.moving,Fish_ID,Trial,Treatment)) |> 
  pivot_wider(names_from = c(Trial, Treatment), values_from = Head.time.moving)


# left_join to combine them into one wide format, rename for better readbility in plot!
mydata_wide <- left_join( mydata_elise, mydata_mr,by = "Fish_ID") |> 
  select(-matches(paste(c("code", "sex","Harvested","Aquarium","speed", "FSR", "Status", 
                          "Weight","end","start","mghg","32","AS_Recovery","scaled_recovery")))) |>                                              
  rename(
    SMR_N = "SMR_scaled_Normoxia",
    SMR_H = "SMR_scaled_Hypoxia",
    MMR_N = "MMR_scaled_Normoxia",
    MMR_H = "MMR_scaled_Hypoxia",
    AS_H = "AS_Hypoxia",
    AS_N =  "AS_Normoxia",
    moving_H2 ="2_Hypoxia", 
    moving_H1=  "1_Hypoxia", 
    moving_H3 = "3_Hypoxia",
    moving_N1 = "1_Normoxia",
    moving_N2 = "2_Normoxia",
    moving_N3 = "3_Normoxia",
    moving_R3 = "3_Recovery",
    moving_R1 = "1_Recovery",
    moving_R2 = "2_Recovery")



# subset by acclimation - Normoxia or hypoxia acclimated
mydata_h <- filter(mydata_wide, Acclimation == "Hypoxia acclimated") |> 
  select(-c(Fish_ID,Acclimation))

mydata_n <- filter(mydata_wide, Acclimation == "Normoxia acclimated") |> 
  select(-c(Fish_ID,Acclimation))


# Correlation in normoxia acclimated fish
cor_norm <- cor(mydata_n, use = "pairwise.complete.obs")
cor_norm_plot1 <- ggcorrplot(cor_norm, type="lower",
                            title = "non acclimated", lab = T)
cor_norm_plot1

# Correlation in hypoxia acclimated fish
cor_hypo <- cor(mydata_h, use = "pairwise.complete.obs")
cor_hypo_plot2 <- ggcorrplot(cor_hypo, type="lower", lab = T,
                             title = "hypoxia acclimated")
cor_hypo_plot2

cor_norm_plot1|cor_hypo_plot2


# If you want to check specific variables, deeper dive from the correlation plots

mydata_n<-subset(mydata_n,moving_R3<0.15)

formula <- y ~ x
sp1 <- ggscatter(mydata_n, x = "moving_H2", y = "moving_H1",
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                 conf.int = F, xlab = "moving_H2", ylab = "moving_H1", 
                 ggtheme = theme_classic()) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),formula = formula)
sp1












