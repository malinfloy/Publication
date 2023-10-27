################################################################################
### STATISTICAL ANALYSES

# 1) Is there a relationship between treatment, acclimation with: metabolic rates, activity?
# 2) Can activity explain why my SMR is lower in hypoxia treatment regardless of acclimation?

#set normoxia as reference level
mydata$Treatment <- factor(mydata$Treatment, levels = c("Normoxia", "Hypoxia","Recovery"))
mydata$Treatment <- relevel(mydata$Treatment, ref = "Normoxia", ordered = TRUE)
mydata$Acclimation <- factor(mydata$Acclimation, levels = c("Normoxia acclimated", "Hypoxia acclimated"))
mydata$Acclimation <- relevel(mydata$Acclimation, ref = "Normoxia acclimated", ordered = TRUE)



### FIT DATA FOR ANALYSIS
library(DHARMa)
library(lme4)
library(emmeans)
library(lmerTest)


### BY GLM MODELS ####
### SMR
## best fitted with Gaussian distribution, and non transformed
hist(mydata$SMR_scaled)
hist(sqrt(mydata$SMR_scaled))
hist(log(mydata$SMR_scaled))


# Testing the AIC for different variables of interest
smr_glm0 <- glmer(SMR_scaled ~ Treatment+Acclimation + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm1 <- glmer(SMR_scaled ~ Treatment+Acclimation*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm2 <- glmer(SMR_scaled ~ Treatment+Acclimation*Sex+SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm3 <- glmer(SMR_scaled ~ Treatment+Acclimation*SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm4 <- glmer(SMR_scaled ~ Treatment*Acclimation*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm5 <- glmer(SMR_scaled ~ Treatment*Acclimation*Sex+SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,family = "gaussian",data = mydata)

smr_glm6 <- glmer(SMR_scaled ~ Treatment+Acclimation*Sex+Harvest2+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm7 <- glmer(SMR_scaled ~ Treatment+Acclimation*Sex+SGR+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

smr_glm8 <- glmer(SMR_scaled ~ Treatment+Acclimation*Sex+Length_end+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)


# If failed to converge
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


### To see all the AIC together, and find the lowest AIC
# List of models
model_smr <- list(smr_glm0, smr_glm1, smr_glm2, smr_glm3, smr_glm4, smr_glm5,smr_glm6,smr_glm7,smr_glm8)

# Calculate AIC for each model and store in a vector
aic_values <- sapply(model_smr, function(model) AIC(model))
aic_values


### BEST MODEL ###
smr_glm <- glmer(SMR_scaled ~ Treatment*Acclimation*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

# use lmer - avoid deprecated usage - this will print the p-values
smr_lmer <- lmer(SMR_scaled ~ Treatment * Acclimation * Sex + (1 | Fish_ID) + (1 | Aquarium), data = mydata, na.action = na.omit)


# test by making variables a factor
as.factor(mydata$Treatment)
as.factor(mydata$Acclimation)
as.factor(mydata$Sex) # changed nothing

# Check residuals
simulationOutput <- simulateResiduals(fittedModel = smr_lmer, plot = T)
testOverdispersion(simulationOutput)

qqnorm(resid(smr_glm))
summary(smr_glm) 

summary(smr_lmer) 

### EMMEANS ON SMR 
smr_grid <- update(ref_grid(smr_lmer, cov.reduce = range))

smr_emm <- emmeans(smr_grid, pairwise~Treatment|Acclimation|Sex, adjust = "none", type="response")
smr_emm

# look within treatment
pairs(smr_emm, simple = "Acclimation")

# table of models
smr_table <- emmip(smr_glm,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = F) 

# plot the model
emmip(smr_glm,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = T)



### MMR ###
hist(mydata$MMR_scaled,breaks = 50) # looks normally distributed

# Testing the AIC for different variables of interest
mmr_glm0 <- glmer(MMR_scaled ~ Treatment+Acclimation + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm)

mmr_glm1 <- glmer(MMR_scaled ~ Treatment+Acclimation+Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm1)

mmr_glm2 <- glmer(MMR_scaled ~ Treatment+Acclimation+Sex+SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm2)

mmr_glm3 <- glmer(MMR_scaled ~ Treatment+Acclimation+SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm3)

mmr_glm4 <- glmer(MMR_scaled ~ Treatment*Acclimation+Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm4)

mmr_glm5 <- glmer(MMR_scaled ~ Treatment*Acclimation+Sex+SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,family = "gaussian",data = mydata)
AIC(mmr_glm5)

mmr_glm6 <- glmer(MMR_scaled ~ Treatment+Acclimation+Sex+Harvest2+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm6)

mmr_glm7 <- glmer(MMR_scaled ~ Treatment+Acclimation+Sex+SGR+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm7)

mmr_glm8 <- glmer(MMR_scaled ~ Treatment+Acclimation+Sex+Length_end+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(mmr_glm9)

mmr_glm9 <- glmer(MMR_scaled ~ Treatment*Acclimation*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)


# List of models
model_mmr <- list(mmr_glm0, mmr_glm1, mmr_glm2, mmr_glm3, mmr_glm4, mmr_glm5,mmr_glm6,mmr_glm7,mmr_glm8,mmr_glm9)

# Calculate AIC for each model and store in a vector
aic_values1 <- sapply(model_mmr, function(model) AIC(model))
aic_values1



### BEST MODEL ###
mmr_glm <- glmer(MMR_scaled ~ Treatment+Acclimation+Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

mmr_lmer <- lmer(MMR_scaled ~ Treatment+Acclimation+Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,data = mydata)


# Check residuals
simulationOutput <- simulateResiduals(fittedModel = mmr_glm, plot = T)
testOverdispersion(simulationOutput)

qqnorm(resid(mmr_glm))
summary(mmr_lmer)


### EMMEANS ON MMR
mmr_grid <- update(ref_grid(mmr_lmer, cov.reduce = range))

mmr_emm <- emmeans(mmr_grid, pairwise~Treatment|Acclimation, adjust = "none", type="response")
mmr_emm

# look within treatment
pairs(mmr_emm, simple = "Acclimation")

# table of models
mmr_table <- emmip(mmr_glm,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = F) 

# plot the model
emmip(mmr_glm,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = T)


#################################################################################


### ACTIVITY ANALYSIS 
## Fit models
hist(mydata$Normalized_Time.moving,breaks = 50) # skewered
hist(sqrt(mydata$Normalized_Time.moving),breaks = 50) # normal


# Gaussian distribution - testing of AIC
moving_glm0 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment*Acclimation+ (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm0)

moving_glm1 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+ (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm1)

moving_glm2 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment*Acclimation+Sex +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm2)


moving_glm3 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+Sex +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,family = "gaussian",data = mydata)
AIC(moving_glm3)

moving_glm4 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+SGR +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm4)

moving_glm5 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+Sex+SGR +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm5)

moving_glm6 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+Sex+SGR_length +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm6)

moving_glm7<- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+Sex+Length_end +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(moving_glm7)

moving_glm8 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment*Acclimation+Sex+Length_end+SGR+ (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",dat  = mydata)
AIC(moving_glm8)

moving_glm9 <- glmer(sqrt(Normalized_Time.moving) ~ Treatment*Acclimation*Sex +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)



# List of models
model_list <- list(moving_glm0,moving_glm1,moving_glm2,moving_glm3,moving_glm4,moving_glm5,moving_glm6,moving_glm7,moving_glm8,moving_glm9)

# Calculate AIC for each model and store in a vector
aic_values2 <- sapply(model_list, function(model) AIC(model))
aic_values2


### BEST MODEL ###
moving_glm <- glmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+Sex +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,family = "gaussian",data = mydata)

moving_lmer <- lmer(sqrt(Normalized_Time.moving) ~ Treatment+Acclimation+Sex +(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,data = mydata)


#check residuals
qqnorm(resid(moving_glm))
simulationOutput1 <- simulateResiduals(fittedModel = moving_lmer, plot = T) 
testOverdispersion(simulationOutput1)

summary(moving_lmer)

### EMMEANS for activity
moving_grid <- update(ref_grid(moving_lmer, cov.reduce = range))

moving_emm <- emmeans(moving_grid, pairwise~Treatment|Acclimation, adjust = "none", type="response")
moving_emm

# look within treatment
pairs(moving_emm, simple = "Acclimation") # no sigificane...

# table of models
moving_table <- emmip(moving_glm,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = F) 

# plot the model
emmip(moving_glm,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = T)


### METABOLIC RATES AND ACTIVITY ####

all_glm1 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled+Treatment+Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm1)

all_glm2 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled+Treatment + (1|Fish_ID) + (1|Aquarium), na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm2)

all_glm3 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled*Treatment + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm3)

all_glm4 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled*Treatment + Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm4)

all_glm5 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled+Treatment + Sex+SGR_length + (1|Fish_ID) + (1|Aquarium),na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm5)

all_glm6 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled+Treatment + Sex+SGR + (1|Fish_ID) + (1|Aquarium),na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm6)

all_glm7 <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled+Treatment + Sex+Length_end + (1|Fish_ID) + (1|Aquarium) ,na.action=na.omit, family = "gaussian",data = mydata)
AIC(all_glm7)

# List of models
model_all <- list(all_glm1,all_glm2,all_glm3,all_glm4,all_glm5,all_glm6,all_glm7)

# Calculate AIC for each model and store in a vector
aic_values <- sapply(model_list, function(model) AIC(model))

# Print AIC values and the best model
aic_values


### BEST MODEL ###
all_glm <- glmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled*Treatment + Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_lmer <- lmer(sqrt(Normalized_Time.moving) ~ SMR_scaled*MMR_scaled*Treatment + Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit,data = mydata)

AIC(all_glm)
qqnorm(resid(all_glm))
summary(all_glm)

summary(all_lmer)

# check residuals in mode
simulationOutput <- simulateResiduals(fittedModel = all_lmer, plot = T)
testOverdispersion(simulationOutput)

### EMMEANS for both

all_grid <- update(ref_grid(all_lmer, cov.reduce = range)) # Range show min and max values

# How the SMR responds
all_emm <- emmeans(all_grid, pairwise~SMR_scaled|MMR_scaled|Treatment, adjust = "none", type="response")
all_emm


# look at how the MMR responds
all_emm1 <- emmeans(all_grid, pairwise~MMR_scaled |SMR_scaled|Treatment, adjust = "none", type="response")
all_emm1


# look within treatment
pairs(all_emm, simple = "Treatment") # no significance...

# table of models
all_table <- emmip(all_glm3,Acclimation~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = F) 

# plot the model
emmip(all_glm3,Treatment~SMR_scaled*MMR_scaled, CIs = TRUE, cov.reduce = range, type = "response", plotit = T)




### Explore with Aerobic scope ####
hist(mydata$AS, breaks = 50) # normal

as_glm <- glmer(AS ~ Treatment+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

as_glm1 <- glmer(AS ~ Treatment+Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

as_glm2<- glmer(AS ~ Treatment*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

as_glm3 <- glmer(AS ~ Treatment*Sex+SGR+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

as_glm4 <- glmer(AS ~ Treatment*Sex+SGR_length+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

as_glm5 <- glmer(AS ~ Treatment*Sex+Length_end+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

# List of models
model_as <- list(as_glm,as_glm1,as_glm2,as_glm3,as_glm4,as_glm5)

# Calculate AIC for each model and store in a vector
aic_values <- sapply(model_as, function(model) AIC(model))

# Print AIC values and the best model
aic_values 

### BEST MODEL ###
as_glm<- glmer(AS ~ Treatment*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

as_lmer <- lmer(AS ~ Treatment*Sex+(1|Fish_ID) + (1|Aquarium) , na.action=na.omit,data = mydata)


summary(as_glm)
summary(as_lmer)

# check residuals in mode
simulationOutput <- simulateResiduals(fittedModel = as_glm, plot = T)
testOverdispersion(simulationOutput)



### EMMEANS for AS
as_grid <- update(ref_grid(as_glm, cov.reduce = range))

as_emm <- emmeans(as_grid, pairwise~Sex|Treatment, adjust = "none", type="response")
as_emm

# look within treatment
pairs(as_emm, simple = "") 

# table of models
moving_table <- emmip(as_glm,Sex~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = F) 

# plot the model
emmip(as_lmer,Sex~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = T)




### Activity and Aerobic Scope ###

all_as_glm0 <- glmer(sqrt(Normalized_Time.moving) ~ AS + Treatment + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm1 <- glmer(sqrt(Normalized_Time.moving) ~ AS * Treatment + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm2 <- glmer(sqrt(Normalized_Time.moving) ~ AS + Treatment+ Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm3 <- glmer(sqrt(Normalized_Time.moving) ~ AS * Treatment * Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm4 <- glmer(sqrt(Normalized_Time.moving) ~ AS * Treatment + Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm5 <- glmer(sqrt(Normalized_Time.moving) ~ AS + Treatment * Sex + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm6 <- glmer(sqrt(Normalized_Time.moving) ~ AS + Treatment + Sex + SGR_length + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_glm7 <- glmer(sqrt(Normalized_Time.moving) ~ AS + Treatment + Sex + Length_end + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)


# List of models
model_all_as <- list(all_as_glm0,all_as_glm1,all_as_glm2,all_as_glm3,all_as_glm4,all_as_glm5,all_as_glm6,all_as_glm7)

# Calculate AIC for each model and store in a vector
aic_values <- sapply(model_all_as, function(model) AIC(model))

# Print AIC values and the best model
aic_values

# check residuals in mode
simulationOutput <- simulateResiduals(fittedModel = all_as_glm1, plot = T)
testOverdispersion(simulationOutput)


### BEST MODEL ###
all_as_glm <- glmer(sqrt(Normalized_Time.moving) ~ AS * Treatment + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit, family = "gaussian",data = mydata)

all_as_lmer <- lmer(sqrt(Normalized_Time.moving) ~ AS * Treatment + (1|Fish_ID) + (1|Aquarium) , na.action=na.omit,data = mydata)

summary(all_as_glm)
summary(all_as_lmer)

### EMMEANS for activity and as
all_as_grid <- update(ref_grid(all_as_lmer, cov.reduce = range))

all_as_emm <- emmeans(all_as_grid, pairwise~AS|Treatment, adjust = "none", type="response")
all_as_emm

# look within treatment
pairs(as_emm, simple = "") 

# table of models
moving_table <- emmip(all_as_glm4,Sex~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = F) 

# plot the model
emmip(all_as_glm4,Sex~Treatment, CIs = TRUE, cov.reduce = range, type = "response", plotit = T)



