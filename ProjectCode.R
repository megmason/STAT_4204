# =====================================================================
# STAT 4204 Project Part II - Questions 1-5 (Concrete Compressive Strength)
# =====================================================================

library(tidyverse)   
library(GGally)      
library(ppcor)       
library(patchwork)  
library(ggplot2)
library(psych)
library(dplyr)
library(plotly)
library(broom)
library(readxl)

# Data prep, other users may want to change the excel path. 
concrete <- read_excel("~/Downloads/Stat4204/Concrete_Data.xls", skip = 1, col_names = FALSE)
names(concrete) <- c("Cement", "Slag", "FlyAsh", "Water", "SupPlas", "CoarseAgg", "FineAgg", "Age", "CCS")
concrete <- concrete[, c("CCS", "Cement", "Slag", "Water", "SupPlas", "Age")]

# Centering the predictors
concrete$Cement_c <- scale(concrete$Cement, center=TRUE, scale=FALSE)
concrete$Slag_c <- scale(concrete$Slag, center=TRUE, scale=FALSE)
concrete$Water_c <- scale(concrete$Water, center=TRUE, scale=FALSE)
concrete$SupPlas_c <- scale(concrete$SupPlas, center=TRUE, scale=FALSE)
concrete$Age_c <- scale(concrete$Age, center=TRUE, scale=FALSE)

# Models
model_A <- lm(CCS ~ Cement_c + Water_c, data = concrete)
model_B <- lm(CCS ~ Cement_c + Slag_c + Age_c + Water_c + SupPlas_c, data = concrete)
model_C <- lm(CCS ~ Cement_c + Water_c + SupPlas_c + Slag_c + log(Age), data = concrete)
model_D <- lm(CCS ~ Cement_c + Slag_c + Age_c + I(Age_c^2), data = concrete)

summary(model_A)
summary(model_B)
summary(model_C)
summary(model_D)

AIC(model_A)
AIC(model_B)
AIC(model_C)
AIC(model_D)


vif(model_A)
vif(model_B)
vif(model_C)
vif(model_D)


