library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

# script for the data frames in "main.R"

# add SGR but using length instead
MR_raw <- MR_raw |> 
  mutate(SGR_length = round((((log(Length_end)-log(Length_start))/58)*100), digits = 2))



# make data long for metabolic plots
Plot_data <- MR_raw |> 
  pivot_longer(cols = 15:18, names_to ="metabolic_type", values_to = "metabolic_rate") |> 
  separate(metabolic_type, into = c("metabolic_type", "condition"), sep = "_") |> 
  pivot_longer(
    cols = starts_with("AS_"),
    names_to = "condition1",
    values_to = "AS",
    names_prefix = "AS_")


# Calculate mean for activity for plotting
mydata_plot <- mydata |> 
  group_by(Fish_ID,Treatment,Acclimation,Sex,AS) |> 
  summarise_at(vars(Normalized_Time.moving), list(mean = ~mean(., na.rm = TRUE)))




##### PLOTS ####

# Plot for metabolic rates by acclimation - All values
mr_p1 <- ggplot(Plot_data, aes( x = metabolic_type, 
                       y = metabolic_rate,
                       fill = condition)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="#E7298A", fill="#E7298A",aes(group=condition),position = position_dodge(width = 0.75)) +
  geom_point(position=position_dodge(width=0.75), aes(group=condition), alpha=0.2)+
  scale_y_continuous(limits = c(0,0.5))+
  labs(x = "",
       y ="Oxygen consumption mgO2/h/g") +
  facet_wrap(~Acclimation)+
  
  #theme(panel.spacing = unit(0, 'points'))+
  theme_minimal()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size=13),
        legend.text = element_text(size = 8))+
  scale_fill_brewer(palette = "Dark2")
mr_p1

ggsave(plot = mr_p1, width = 10, height = 10, dpi = 300, filename = "mr_figure.png")

# split by sex as well
# Create a new column for the x-axis combining SMR/MMR, sex, and treatment
# Might be hard to read
Plot_data$X_axis <- with(Plot_data, paste(metabolic_type, sex, condition, sep = "/"))
Plot_data$X_axis <- sub("/(Hypoxic|Normoxic)$", "", Plot_data$X_axis)

ggplot(Plot_data, aes(x = X_axis,  # Use the new column for x-axis
  y = metabolic_rate,
  fill = condition)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun.y = mean,geom = "point",shape = 20,size = 5, color = "red",fill = "red",
    aes(group = condition),
    position = position_dodge(width = 0.75)) +
  geom_point(position = position_dodge(width = 0.75), aes(group = condition), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "",y = "Oxygen consumption mgO2/h/g") +
  facet_wrap(~ Acclimation, scales = "free_x") +  # Facet by Acclimation
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 1),
    axis.title = element_text(size = 7),
    legend.text = element_text(size = 8))+
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2")



### Aerobic scope plot
AS_plot <-ggplot(Plot_data, aes( x = sex, 
                       y = AS,
                       fill = condition1)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="#E7298A", fill="#E7298A",aes(group=condition1),position = position_dodge(width = 0.75)) +
  labs(title= "The aerboic scope under both treatments",
       x = "",
       y ="Absolute Aerobic scope",
       fill = "Treatment") +
  theme_minimal()+
  theme(strip.text = element_text(size = 10, face = "bold"))+
  scale_x_discrete(labels = c("Females", "Males"))+
  scale_fill_brewer(palette = "Dark2")
AS_plot

ggsave(plot = AS_plot, width = 10, height = 10, dpi = 300, filename = "AS_figure.png")



### Activity
library(patchwork)

activity_plot <-ggplot(mydata,
       aes(x = Treatment, y = Normalized_Time.moving, fill = Treatment)) +
  geom_boxplot(alpha = 0.5,aes(group = Treatment), outlier.shape = NA, position = position_dodge(0.8), color = "black") +
  geom_point(position=position_dodge(width=0.75), aes(group=Treatment), alpha=0.2)+
  labs(title = "Time spent moving", x = "Treatment", y = "Normalized Time Spent Moving (cm/s???)", shape = "Sex", color = "Sex", fill = "Treatment") +
  theme_minimal()+
  facet_grid(~Acclimation)+
  scale_fill_brewer(palette = "Dark2")
activity_plot

### activity and metabolic rates
# calculate the mean for the individual for the plot. only for the plots! 

ggscatter(mydata, x = "AS", y = "Normalized_Time.moving", color = "Treatment",
          shape = "Sex", conf.int = FALSE, xlab = "Activity", ylab = "AS", 
          ggtheme = theme_classic()) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Treatment,color = Treatment)) 

#mean
all_plot <- ggscatter(mydata_plot, x = "AS", y = "mean", color = "Treatment", alpha= 0.7,
          shape = "Sex", conf.int = FALSE, xlab = "Aerobic scope", ylab = "Activity",
          ggtheme = theme_minimal()) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Treatment,color = Treatment))+
  scale_color_brewer(palette = "Dark2")
all_plot



p1 <- activity_plot+ theme(legend.position = "none")|all_plot

p1



