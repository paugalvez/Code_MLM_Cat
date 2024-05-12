#Availability_Health_Assets_SP and MW

# Libraries and datasets---------------------------------------------------------------

# Libraries and datasets---------------------------------------------------------------

library(lmerTest)
library(lme4)
library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)
library(sf)
library(sjPlot)
library(rstatix)
library(broom)
library(robustlmm)
library(influence.ME)
library(HLMdiag)

#These two datasets contain 6011 observations, including those with missing values and imputed values from the ESCA 2017-2022, 
#merged with datasets on asset-based intersectoral initiatives across geographical areas.
#These datasets have already merged the initiatives dataset with the Catalan Health Survey, calculated standardized numbers of initiatives per geographic area, and accounted for skewness by converting count data to categorical data.
ESCA_Finalv3 <- read_excel("C:\\Users\\galve\\OneDrive\\Desktop\\data storage projects\\MLM_PhD_Phase2\\Final_datasets_paper_JAandH\\Dataset_ESCA_HA2017_2022.xlsx")
ESCA_NAv2 <-read_excel("C:\\Users\\galve\\OneDrive\\Desktop\\data storage projects\\MLM_PhD_Phase2\\Final_datasets_paper_JAandH\\ESCA_NAv2.xlsx")

# Generation of independent variables  -----------------------------------------

#1.Number of initiatives
# Subset the data to include only observations from the year 2021
data_2021 <- ESCA_Finalv3[ESCA_Finalv3$Interview_wave == 2021, ]

# Calculate the median of the cumulative count of activities for 2021
median(data_2021$Activities10k, na.rm = TRUE)

# Creating 3 exposure categories 
ESCA_Finalv3 <- ESCA_Finalv3 %>%
  mutate(avail_activities = case_when(
    Activities10k == 0 ~ "0",
    Activities10k <= 15 ~ "1",
    Activities10k > 15 ~ "2"))

#2.Territorial Reach

ESCA_Finalv3 <- ESCA_Finalv3 %>%
  mutate(uptake = case_when(
    proportions == 0 ~ "0",
    proportions <= 0.5 ~ "1",
    proportions > 0.5 ~ "2",
  ))

#Grand mean centering covariates improving interpretability.

ESCA_Finalv3$CAge <- (ESCA_Finalv3$Age - mean(ESCA_Finalv3$Age))
ESCA_Finalv3$CChronic_conditions <- (ESCA_Finalv3$Chronic_conditions - mean(ESCA_Finalv3$Chronic_conditions))
ESCA_Finalv3$CHS_population <- (ESCA_Finalv3$HS_population - mean(ESCA_Finalv3$HS_population))
ESCA_Finalv3$CHS_prop_75_alone <- (ESCA_Finalv3$HS_prop_75_alone - mean(ESCA_Finalv3$HS_prop_75_alone))
ESCA_Finalv3$CHS_prop_65 <- (ESCA_Finalv3$HS_prop_65 - mean(ESCA_Finalv3$HS_prop_65))
ESCA_Finalv3$CHousehold_members <- (ESCA_Finalv3$Household_members - mean(ESCA_Finalv3$Household_members))
ESCA_Finalv3$CSocioEconomicIndex <- (ESCA_Finalv3$HS_Socioeconomicindex - mean(ESCA_Finalv3$HS_Socioeconomicindex))

#Diagnostics outcome variables #Social support

#Histogram
ggplot(ESCA_Finalv3, aes(x = Social_support)) +
  geom_histogram(aes(y = ..density..), binwidth = diff(range(ESCA_Finalv3$Social_support))/30, fill = "blue", alpha = 0.5) +
  geom_density(alpha = .2, fill = "#FF6666")

# QQ plot
qqnorm(ESCA_Finalv3$Social_support)
qqline(ESCA_Finalv3$Social_support, col = "steelblue", lwd = 2)

# Box plot 
ggplot(ESCA_Finalv3, aes(x = "", y = Social_support)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  theme_minimal()

#Diagnostics outcome variables #Mental Well being

#Histogram
ggplot(ESCA_Finalv3, aes(x = Mental_well_being)) +
  geom_histogram(aes(y = ..density..), binwidth = diff(range(ESCA_Finalv3$Mental_well_being))/30, fill = "blue", alpha = 0.5) +
  geom_density(alpha = .2, fill = "#FF6666")

# QQ plot
qqnorm(ESCA_Finalv3$Mental_well_being)
qqline(ESCA_Finalv3$Mental_well_being, col = "steelblue", lwd = 2)

# Box plot 
ggplot(ESCA_Finalv3, aes(x = "", y = Mental_well_being)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  theme_minimal()
# Descriptive analysis ----------------------------------------------------

#Numerical
summary_stats_numerical <- sapply(ESCA_Finalv3, function(x) if(is.numeric(x)) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)) else NA)

#Categorical
summary_stats_categorical <- lapply(ESCA_Finalv3, function(x) if(is.factor(x)) {
  counts <- table(x, useNA = "ifany")
  percentages <- prop.table(counts) * 100
  result <- cbind(counts, percentages)
  colnames(result) <- c("Count", "Percentage")
  result
} else NA)

print(summary_stats_numerical)
print(summary_stats_categorical)

# Bivariate analysis ----------------------------------------------------

#individual-level categorical covariates

categorical_vars <- c("Gender", "Limitation_daily_activities", "Education", "Employment", "Economic_strain", "Nationality")

#Calculate totals per avail_activities & uptake categories and covariates
totals <- ESCA_Finalv3 %>%
  group_by(avail_activities) %>%
  summarise(Total = n(), .groups = 'drop')

totals2 <- ESCA_Finalv3 %>%
  group_by(uptake) %>%
  summarise(Total = n(), .groups = 'drop')

#n and % categorical covariates per category of independent variable
for (var in categorical_vars) {         #independent_v1_avail_activities
  summary_data <- ESCA_Finalv3 %>%
    group_by(avail_activities, !!sym(var)) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    left_join(totals, by = "avail_activities") %>%
    mutate(Percentage = (Count / Total) * 100) %>%
    ungroup()
  
  print(summary_data)
}

for (var in categorical_vars) {         #independent_v2_uptake
  summary_data2 <- ESCA_Finalv3 %>%
    group_by(uptake, !!sym(var)) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    left_join(totals2, by = "uptake") %>%
    mutate(Percentage = (Count / Total) * 100) %>%
    ungroup()
  
  print(summary_data2)
}

#Chi-square bivariate analysis

for (var in categorical_vars) {    #independent_v1_avail_activities
  
  table_data <- table(ESCA_Finalv3$avail_activities, ESCA_Finalv3[[var]])
  chi_test <- chisq.test(table_data)
  
  tidy_chi <- tidy(chi_test)
  print(paste("Results for variable:", var))
  print(tidy_chi)
}

for (var in categorical_vars) {    #independent_v2_uptake
  
  table_data2 <- table(ESCA_Finalv3$uptake, ESCA_Finalv3[[var]])
  chi_test2 <- chisq.test(table_data2)
  
  tidy_chi2 <- tidy(chi_test2)
  print(paste("Results for variable:", var))
  print(tidy_chi2)
}

#Individual-level continuous covariates

numerical_vars <- c("Age", "Household_members", "Chronic_conditions")

# Calculating Means and SDs avail_activities

summary_stats2 <- ESCA_Finalv3 %>%  
  group_by(avail_activities) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    Mean_Household_members = mean(Household_members, na.rm = TRUE),
    SD_Household_members = sd(Household_members, na.rm = TRUE),
    Mean_Chronic_conditions = mean(Chronic_conditions, na.rm = TRUE),
    SD_Chronic_conditions = sd(Chronic_conditions, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats2)

# Calculating Means and SDs uptake #avail_activities

summary_stats3 <- ESCA_Finalv3 %>%  
  group_by(uptake) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    Mean_Household_members = mean(Household_members, na.rm = TRUE),
    SD_Household_members = sd(Household_members, na.rm = TRUE),
    Mean_Chronic_conditions = mean(Chronic_conditions, na.rm = TRUE),
    SD_Chronic_conditions = sd(Chronic_conditions, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats3)
  
  # ANOVA test for each numerical #avail_activities

results <- lapply(numerical_vars, function(var) {
  ESCA_Finalv3$avail_activities <- as.factor(ESCA_Finalv3$avail_activities)
    formula <- as.formula(paste(var, "~ avail_activities"))
    anova_result <- aov(formula, data = ESCA_Finalv3)
  summary(anova_result)
})

print(results)

# ANOVA test for each numerical #uptake

results <- lapply(numerical_vars, function(var) {
  ESCA_Finalv3$uptake <- as.factor(ESCA_Finalv3$uptake)
  formula <- as.formula(paste(var, "~ uptake"))
  anova_result <- aov(formula, data = ESCA_Finalv3)
  summary(anova_result)
})

print(results)

#Descriptive statistics and ANOVA for outcomes

ESCA_Finalv3 %>%  
  group_by(avail_activities) %>%
  summarise(
    Mean_SS = mean(Social_support, na.rm = TRUE),
    SD_SS= sd(Social_support, na.rm = TRUE),
    Mean_MW = mean(Mental_well_being, na.rm = TRUE),
    SD_MW = sd(Mental_well_being, na.rm = TRUE),
    .groups = 'drop'
  )

ESCA_Finalv3 %>%  
  group_by(uptake) %>%
  summarise(
    Mean_SS = mean(Social_support, na.rm = TRUE),
    SD_SS= sd(Social_support, na.rm = TRUE),
    Mean_MW = mean(Mental_well_being, na.rm = TRUE),
    SD_MW = sd(Mental_well_being, na.rm = TRUE),
    .groups = 'drop'
  )

anova_result <- aov(Social_support ~ avail_activities, data = ESCA_Finalv3)
summary(anova_result)
tukey_test <- TukeyHSD(anova_result)
print(tukey_test)

anova_result2 <- aov(Mental_well_being ~ avail_activities, data = ESCA_Finalv3)
summary(anova_result2)
tukey_test2 <- TukeyHSD(anova_result2)
print(tukey_test2)

anova_result3 <- aov(Social_support ~ uptake, data = ESCA_Finalv3)
summary(anova_result3)
tukey_test3 <- TukeyHSD(anova_result3)
print(tukey_test3)

anova_result4 <- aov(Mental_well_being ~ uptake, data = ESCA_Finalv3)
summary(anova_result4)
tukey_test4 <- TukeyHSD(anova_result4)
print(tukey_test4)


# Multilevel Models Social_support -------------------------------------------------------

  #Deciding between simple or multilevel

    # Null mixed model
    lmm_null <- lmer(Social_support ~ 1 + (1 | Health_Sector), data = ESCA_Finalv3)
    
    # Simple linear model
    lm_null <- lm(Social_support ~ 1, data = ESCA_Finalv3)
    
    # Likelihood Ratio Test
    anova(lmm_null, lm_null)
    
    # AIC and BIC (LMM shows better fit)
    AIC(lmm_null, lm_null)
    BIC(lmm_null, lm_null)


  #Null MLM
    
    #social_support ~ availability initiatives
      NullSS_model <- lmer(Social_support ~ 1 + (1 | Health_Sector), data = ESCA_Finalv3) #Random intercept only
      tab_model(NullSS_model)
      AIC(NullSS_model)
      summary(NullSS_model)
      
    #social_support ~ uptake
      NullUp_modelSS <- lmer(Social_support ~ 1 + (1 | Health_Sector), data = ESCA_Finalv3) #Random intercept and slope
      tab_model(NullUp_modelSS)
      AIC(NullUp_modelSS)
      
  #Model 1: only outcome, main exposure and interview wave
  
    #Social_support ~ availability initiatives 
      FinalM1a_SSmodel<- lmer(Social_support ~ avail_activities +   
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM1a_SSmodel)
      summary (FinalM1a_SSmodel)
      AIC(FinalM1a_SSmodel)
  
    #social_support ~ uptake

      FinalM1b_SSmodel<- lmer(Social_support ~ uptake +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM1b_SSmodel)
      AIC(FinalM1b_SSmodel)

  #Model 2: Avail_activities plus all individual covariates
    
    #Social_support ~ availability initiatives 

      FinalM2a_SSmodel<- lmer(Social_support ~ avail_activities + Gender + CAge + Limitation_daily_activities + 
                                CChronic_conditions + Education + Employment + CHousehold_members + 
                                Economic_strain + Nationality +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM2a_SSmodel)
      AIC(FinalM2a_SSmodel)

    #social_support ~ uptake

      FinalM2b_SSmodel<- lmer(Social_support ~ uptake + Gender + CAge + Limitation_daily_activities + 
                                CChronic_conditions + Education + Employment + CHousehold_members + 
                                Economic_strain + Nationality +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM2b_SSmodel)
      AIC(FinalM2b_SSmodel)


  #Model 3 (All individual and contextual covariates exposure avail activities)
     
    #Social_support ~ availability initiatives 
      
      FinalM3a_SSmodel<- lmer(Social_support ~ avail_activities + Gender + CAge + Limitation_daily_activities + 
                                CChronic_conditions + Education + Employment + CHousehold_members + 
                                Economic_strain + Nationality + CHS_prop_75_alone +
                                Interview_wave + CSocioEconomicIndex + S_HS_population +(1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM3a_SSmodel)
      AIC(FinalM3a_SSmodel)
      
      #Testing model fit with random slope after all covariates included
      
      FinalM3a_SSmodelRS <- lmer(Social_support ~ avail_activities + Gender + CAge + Limitation_daily_activities + 
                                CChronic_conditions + Education + Employment + CHousehold_members + 
                                Economic_strain + Nationality + Interview_wave + 
                                  (1 + avail_activities | Health_Sector), data = ESCA_Finalv3) #it converges removing the contexual covariates
      tab_model(FinalM3a_SSmodelRS)
      AIC(FinalM3a_SSmodelRS)
      
    #Social_support ~ Uptake
      
      FinalM3b_SSmodel<- lmer(Social_support ~ uptake + Gender + CAge + Limitation_daily_activities + 
                                CChronic_conditions + Education + Employment + CHousehold_members + 
                                Economic_strain + Nationality + CHS_prop_75_alone +
                                Interview_wave + CSocioEconomicIndex + S_HS_population +(1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM3b_SSmodel)
      AIC(FinalM3b_SSmodel)
      

      #Model 4: Parsimonious model_Social Support -----------------------------

  
      #Social_support ~ availability initiatives 

      FinalM4parsimonious_SSmodel <- step(FinalM3a_SSmodel, direction = "backward", alpha.fixed = 0.10, ddf = "Satterthwaite")
      
      FinalM4a_SSmodel <- lmer(Social_support ~ avail_activities+
                                 Limitation_daily_activities + CChronic_conditions + 
                                 CHousehold_members + Economic_strain + Nationality + 
                                 Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM4a_SSmodel)
      AIC(FinalM4a_SSmodel)
      

      #Social_support ~ uptake 

      FinalM4parsimoniousa_SSmodel <- step(FinalM3b_SSmodel, direction = "backward", alpha.fixed = 0.10, ddf = "Satterthwaite")
      
      FinalM4b_SSmodel <- lmer(Social_support ~ uptake + Limitation_daily_activities + 
                                 CChronic_conditions + CHousehold_members + Economic_strain + 
                                 Nationality + Interview_wave + (1 | Health_Sector), 
                               data = ESCA_Finalv3)

      tab_model(FinalM4b_SSmodel)
      AIC(FinalM4b_SSmodel)

      #Random Slope Social_support ~ availability initiatives 
      
      FinalM4a_SSmodelRS <- lmer(Social_support ~ avail_activities+
                                 Limitation_daily_activities + CChronic_conditions + 
                                 CHousehold_members + Economic_strain + Nationality + 
                                 Interview_wave + (1 + avail_activities | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4a_SSmodelRS)
      AIC(FinalM4a_SSmodelRS)
      
      #Random Slope Social_support ~ uptake 

      
      FinalM4b_SSmodelRS <- lmer(Social_support ~ uptake + Limitation_daily_activities + 
                                 CChronic_conditions + CHousehold_members + Economic_strain + 
                                 Nationality + Interview_wave + (1 + uptake | Health_Sector), 
                               data = ESCA_Finalv3)
      
      tab_model(FinalM4b_SSmodelRS)
      AIC(FinalM4b_SSmodelRS)
      
      
# Multilevel Models Mental Well-being -------------------------------------------------------
      
      
      #Deciding between simple or multilevel
      
      # Null mixed model
      lmm_nullMW <- lmer(Mental_well_being ~ 1 + (1 | Health_Sector), data = ESCA_Finalv3)
      
      # Simple linear model
      lm_nullMW <- lm(Mental_well_being ~ 1, data = ESCA_Finalv3)
      
      # Likelihood Ratio Test
      anova(lmm_null, lm_null)
      
      # AIC and BIC (LMM shows better fit)
      AIC(lmm_nullMW, lm_nullMW)
      BIC(lmm_nullMW, lm_nullMW)
      
      
      #Null MLM
      
      #Mental_well_being ~ availability initiatives
      NullMW_model <- lmer(Mental_well_being ~ 1 + (1 | Health_Sector), data = ESCA_Finalv3) #Random intercept only
      tab_model(NullMW_model)
      AIC(NullMW_model)
      summary(NullMW_model)
      
      #Model 1: only outcome, main exposure and interview wave
      
      #Mental_well_being ~ availability initiatives 
      FinalM1a_MWmodel<- lmer(Mental_well_being ~ avail_activities +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM1a_MWmodel)
      AIC(FinalM1a_MWmodel)
      
      #Mental_well_being ~ uptake
      
      FinalM1b_MWmodel<- lmer(Mental_well_being ~ uptake +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM1b_MWmodel)
      AIC(FinalM1b_MWmodel)
      
      #Model 2: Avail_activities plus all individual covariates
      
      #Mental_well_being ~ availability initiatives 
      
      FinalM2a_MWmodel<- lmer(Mental_well_being ~ avail_activities + Gender + CAge + Limitation_daily_activities + 
                                CHousehold_members+ CChronic_conditions + Education + Employment + 
                                Economic_strain + Nationality +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM2a_MWmodel)
      AIC(FinalM2a_MWmodel)
      
      #Mental_well_being ~ uptake
      
      FinalM2b_MWmodel<- lmer(Mental_well_being ~ uptake + Gender + CAge + Limitation_daily_activities + 
                                CHousehold_members+CChronic_conditions + Education + Employment  + 
                                Economic_strain + Nationality +
                                Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM2b_MWmodel)
      AIC(FinalM2b_MWmodel)
      
      
      #Model 3 (All individual and contextual covariates exposure avail activities)
      
      #Mental_well_being ~ availability initiatives 
      
      FinalM3a_MWmodel<- lmer(Mental_well_being ~ avail_activities + Gender + CAge + Limitation_daily_activities + 
                                CHousehold_members+CChronic_conditions + Education + Employment  + 
                                Economic_strain + Nationality + CHS_prop_75_alone +
                                Interview_wave + CSocioEconomicIndex + S_HS_population +(1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM3a_MWmodel)
      AIC(FinalM3a_MWmodel)
      
      #Testing model fit with random slope after all covariates included
      
      FinalM3a_MWmodelRS <- lmer(Mental_well_being ~ avail_activities + Gender + CAge + Limitation_daily_activities + 
                                   CChronic_conditions + Education + Employment + CHousehold_members + 
                                   Economic_strain + Nationality + Interview_wave + 
                                   (1 + avail_activities | Health_Sector), data = ESCA_Finalv3) #it converges removing the contexual covariates
      tab_model(FinalM3a_MWmodelRS)
      AIC(FinalM3a_MWmodelRS)
      
      #Mental_well_being ~ Uptake
      
      FinalM3b_MWmodel<- lmer(Mental_well_being ~ uptake + Gender + CAge + Limitation_daily_activities + 
                                CChronic_conditions + Education + Employment + CHousehold_members + 
                                Economic_strain + Nationality + CHS_prop_75_alone +
                                Interview_wave + CSocioEconomicIndex + S_HS_population +(1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM3b_MWmodel)
      AIC(FinalM3b_MWmodel)
      

      #Model 4: Parsimonious model_Mental Well_being --------------------------

      #Mental_well_being ~ availability initiatives 
      
      FinalM4parsimonious_MWmodel <- step(FinalM3a_MWmodel, direction = "backward", alpha.fixed = 0.10, ddf = "Satterthwaite")
      
      FinalM4a_MWmodel <- lmer(Mental_well_being ~ avail_activities + Gender +
                                 CAge + Limitation_daily_activities + CChronic_conditions + 
                                 Education + Economic_strain + 
                                 Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4a_MWmodel)
      AIC(FinalM4a_MWmodel)
      

      #Mental_well_being ~ uptake 
      
      FinalM4parsimoniousa_MWmodel <- step(FinalM3b_MWmodel, direction = "backward", alpha.fixed = 0.10, ddf = "Satterthwaite")
      
      FinalM4b_MWmodel <- lmer(Mental_well_being ~ uptake + Gender + CAge + 
                                 Limitation_daily_activities + CChronic_conditions + 
                                 Education + Economic_strain + Interview_wave + 
                                 (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4b_MWmodel)
      AIC(FinalM4b_MWmodel)
      
      #Random Slope Mental_well_being ~ availability initiatives 
      
      FinalM4a_MWmodelRS <- lmer(Mental_well_being ~ avail_activities + Gender +
                                 CAge + Limitation_daily_activities + CChronic_conditions + 
                                 Education + Economic_strain + 
                                 Interview_wave + (1 + avail_activities | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4a_MWmodelRS) #Does not converge
      AIC(FinalM4a_MWmodelRS)
      
      #Random Slope Mental_well_being ~ uptake 
      
      
      FinalM4b_MWmodel <- lmer(Mental_well_being ~ uptake + Gender + CAge + 
                                 Limitation_daily_activities + CChronic_conditions + 
                                 Education + Economic_strain + Interview_wave + 
                                 (1 + avail_activities | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4b_MWmodel)
      AIC(FinalM4b_MWmodel)

# Multilevel Models MODERATOR SOCIAL SUPPORT IN MENTAL WELLBEING -------------------------------------------------------
      
      FinalMediator_MWmodel <- lmer(Mental_well_being ~ avail_activities*Social_support +  
                                      Gender + CAge + 
                                      Limitation_daily_activities + CChronic_conditions + 
                                      Education + Economic_strain + Interview_wave + 
                                      (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalMediator_MWmodel)
      

# Model diagnostics -------------------------------------------------------

      #Residuals for homosedasticity
      plot(resid(FinalM4a_SSmodel) ~ fitted(FinalM4a_SSmodel), main="Residuals vs Fitted for NullSS_modelRS")
      abline(h=0, col="red")
      anova(NullSS_model, NullSS_modelRS) # P,0.001 indicates random slope improves model fit.
      ------
      
      # QQ Plot for Normality
      #M4a_SS
      qqnorm(resid(FinalM4a_SSmodel))
      qqline(resid(FinalM4a_SSmodel), col = "steelblue")
      
      #M4b_SS
      qqnorm(resid(FinalM4b_SSmodel))
      qqline(resid(FinalM4b_SSmodel), col = "steelblue")
      
      #M4a_MW
      qqnorm(resid(FinalM4a_MWmodel))
      qqline(resid(FinalM4a_MWmodel), col = "steelblue")
      
      #M4b_MW
      qqnorm(resid(FinalM4b_MWmodel))
      qqline(resid(FinalM4b_MWmodel), col = "steelblue") 
      
      #Diagnostics outliers level 2
      
      #M4a_SS
      
      cooksd_fm1 <- cooks.distance(FinalM4a_SSmodel, level = "Health_Sector")
      dotplot_diag(x = cooksd_fm1, cutoff = "internal")
      
      #M4b_SS
      
      cooksd_fm2 <- cooks.distance(FinalM4b_SSmodel, level = "Health_Sector")
      dotplot_diag(x = cooksd_fm2, cutoff = "internal")
      
      #M4a_MW
      
      cooksd_fm3 <- cooks.distance(FinalM4a_MWmodel, level = "Health_Sector")
      dotplot_diag(x = cooksd_fm3, cutoff = "internal")
      
      #M4b_MW
      
      cooksd_fm4 <- cooks.distance(FinalMbb_MWmodel, level = "Health_Sector")
      dotplot_diag(x = cooksd_fm4, cutoff = "internal")
      
# Sensitivity analyses --------------------------------------------------

    #specificity (selecting outcome chronic conditions) -----------------

      
      FinalM4a_CCmodel <- lmer(CChronic_conditions ~ avail_activities+
                                 Limitation_daily_activities +  
                                 CHousehold_members + Economic_strain + Nationality + 
                                 Interview_wave + (1 + avail_activities | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4a_CCmodel) #specificity assumption met
      
      

    #Robust models to adjust outliers observed in QQ graphs -------------

      #M4a_SS
      FinalM4a_SSmodel_Rob <- rlmer(Social_support ~ avail_activities+
                                  Limitation_daily_activities + CChronic_conditions + 
                                  CHousehold_members + Economic_strain + Nationality + 
                                  Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      tab_model(FinalM4a_SSmodel_Rob)
      AIC(FinalM4a_SSmodel_Rob)
  
      #M4b_SS
      
      FinalM4b_SSmodel_Rob <- rlmer(Social_support ~ uptake + Limitation_daily_activities + 
                                 CChronic_conditions + CHousehold_members + Economic_strain + 
                                 Nationality + Interview_wave + (1 | Health_Sector), 
                               data = ESCA_Finalv3)
      
      tab_model(FinalM4b_SSmodel_Rob)
      AIC(FinalM4b_SSmodel_Rob)
      
      #M4a_MW
     
       FinalM4a_MWmodel_Rob <- rlmer(Mental_well_being ~ avail_activities + Gender +
                                 CAge + Limitation_daily_activities + CChronic_conditions + 
                                 Education + Economic_strain + 
                                 Interview_wave + (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4a_MWmodel_Rob)
      AIC(FinalM4a_MWmodel_Rob)
      
      #M4b_MW
      
      FinalM4b_MWmodel_Rob <- rlmer(Mental_well_being ~ uptake + Gender + CAge + 
                                 Limitation_daily_activities + CChronic_conditions + 
                                 Education + Economic_strain + Interview_wave + 
                                 (1 | Health_Sector), data = ESCA_Finalv3)
      
      tab_model(FinalM4b_MWmodel_Rob)
      AIC(FinalM4b_MWmodel_Rob)
      

  # CLMM- Ordinal Logistic Mixed Model  -----------------------------------

      ESCA_Finalv4$SocialSupportCat <- cut(ESCA_Finalv4$Social_support, #Converting outcome measure to ordinal factor
                                           breaks = c(2, 8, 11, 14),
                                           labels = c("Poor", "Moderate", "Strong"),
                                           right = TRUE,
                                           include.lowest = TRUE)
      
      ESCA_Finalv4$SocialSupportCat <- ordered(ESCA_Finalv4$SocialSupportCat)
      
      FinalM4a_SSmodel_R <- clmm(SocialSupportCat ~ avail_activities+             #Reproducing parsimonious model with social support as a 3-level factor using CLMM
                                   Limitation_daily_activities + CChronic_conditions + 
                                   CHousehold_members + Economic_strain + Nationality + 
                                   Interview_wave + (1 | Health_Sector), data = ESCA_Finalv4)
      
      summary(FinalM4a_SSmodel_R)
      
          #Resuls are consistent with that of MLR model
          avail_activities1    OR  1.3267217 CI 1.0800349 1.6297533
          avail_activities2    OR  1.6559386 CI 1.2493873 2.1947820
        
#Removing COVID Years (2020-2021) ---------------------------------------

    ESCA_Finalv4_no2020 <- ESCA_Finalv3 %>% 
    filter(as.numeric(as.character(Interview_wave)) < 2020)
      
      
      #M4a_SS
      FinalM4a_SSmodel_noCOVID <- lmer(Social_support ~ avail_activities+
                                     Limitation_daily_activities + CChronic_conditions + 
                                     CHousehold_members + Economic_strain + Nationality + 
                                     Interview_wave + (1 | Health_Sector), data = ESCA_Finalv4_no2020)
      tab_model(FinalM4a_SSmodel_noCOVID)
      AIC(FinalM4a_SSmodel_noCOVID)
      
      #M4b_SS
      
      FinalM4b_SSmodel_noCOVID <- lmer(Social_support ~ uptake + Limitation_daily_activities + 
                                     CChronic_conditions + CHousehold_members + Economic_strain + 
                                     Nationality + Interview_wave + (1 | Health_Sector), 
                                   data = ESCA_Finalv4_no2020)
      
      tab_model(FinalM4b_SSmodel_noCOVID)
      AIC(FinalM4b_SSmodel_noCOVID)
      
      #M4a_MW
      
      FinalM4a_MWmodel_noCOVID <- lmer(Mental_well_being ~ avail_activities + Gender +
                                     CAge + Limitation_daily_activities + CChronic_conditions + 
                                     Education + Economic_strain + 
                                     Interview_wave + (1 | Health_Sector), data = ESCA_Finalv4_no2020)
      
      tab_model(FinalM4a_MWmodel_noCOVID)
      AIC(FinalM4a_MWmodel_noCOVID)
      
      #M4b_MW
      
      FinalM4b_MWmodel_noCOVID <- lmer(Mental_well_being ~ uptake + Gender + CAge + 
                                     Limitation_daily_activities + CChronic_conditions + 
                                     Education + Economic_strain + Interview_wave + 
                                     (1 | Health_Sector), data = ESCA_Finalv4_no2020)
      
      tab_model(FinalM4b_MWmodel_noCOVID)
      AIC(FinalM4b_MWmodel_noCOVID)
  

  #Removing population <65 years ----------------------------------------

    ESCA_Finalv4_65 <- ESCA_Finalv3 %>%
    filter(Age >= 65)
      
      #M4a_SS
      FinalM4a_SSmodel_65 <- lmer(Social_support ~ avail_activities+
                                      Limitation_daily_activities + CChronic_conditions + 
                                      CHousehold_members + Economic_strain + Nationality + 
                                      Interview_wave + (1 | Health_Sector), data = ESCA_Finalv4_65)
      tab_model(FinalM4a_SSmodel_65)
      AIC(FinalM4a_SSmodel_65)
      
      #M4b_SS
      
      FinalM4b_SSmodel_65 <- lmer(Social_support ~ uptake + Limitation_daily_activities + 
                                      CChronic_conditions + CHousehold_members + Economic_strain + 
                                      Nationality + Interview_wave + (1 | Health_Sector), 
                                    data = ESCA_Finalv4_65)
      
      tab_model(FinalM4b_SSmodel_65)
      AIC(FinalM4b_SSmodel_65)
      
      
      #M4a_MW
      
      FinalM4a_MWmodel_65 <- lmer(Mental_well_being ~ avail_activities + Gender +
                                      CAge + Limitation_daily_activities + CChronic_conditions + 
                                      Education + Economic_strain + 
                                      Interview_wave + (1 | Health_Sector), data = ESCA_Finalv4_65)
      
      tab_model(FinalM4a_MWmodel_65)
      AIC(FinalM4a_MWmodel_65)
      
      #M4b_MW
      
      FinalM4b_MWmodel_65 <- lmer(Mental_well_being ~ uptake + Gender + CAge + 
                                      Limitation_daily_activities + CChronic_conditions + 
                                      Education + Economic_strain + Interview_wave + 
                                      (1 | Health_Sector), data = ESCA_Finalv4_65)
      
      tab_model(FinalM4b_MWmodel_65)
      AIC(FinalM4b_MWmodel_65)
      
     #Single vs multilevel likelihood ratio test in final models -------------

          #M4a_SS
  
          FinalM4a_SSmodel_1level <- lm(Social_support ~ avail_activities +
                                     Limitation_daily_activities + CChronic_conditions + 
                                     CHousehold_members + Economic_strain + Nationality + 
                                     Interview_wave + Health_Sector, data = ESCA_Finalv3)
          
          tab_model(FinalM4a_SSmodel_1level)
          AIC(FinalM4a_SSmodel_1level)
          anova(FinalM4a_SSmodel,FinalM4a_SSmodel_1level)
          
          #M4b_SS

          FinalM4a_SSmodel_1alevel <- lm(Social_support ~ uptake +
                                        Limitation_daily_activities + CChronic_conditions + 
                                        CHousehold_members + Economic_strain + Nationality + 
                                        Interview_wave + Health_Sector, data = ESCA_Finalv3)

          tab_model(FinalM4a_SSmodel_1alevel)

          #M4a_MW
          
          FinalM4a_MWmodel_1level <- lm(Mental_well_being ~ avail_activities + Gender +
                                     CAge + Limitation_daily_activities + CChronic_conditions + 
                                     Education + Economic_strain + 
                                     Interview_wave + Health_Sector, data = ESCA_Finalv3)
          
          tab_model(FinalM4a_MWmodel_1level)
    
          #M4b_MW
    
          FinalM4b_MWmodel1_level <- lm(Mental_well_being ~ uptake + Gender + CAge + 
                               Limitation_daily_activities + CChronic_conditions + 
                               Education + Economic_strain + Interview_wave + 
                               Health_Sector, data = ESCA_Finalv3)
    
          tab_model(FinalM4b_MWmodel1_level)
          

  # Assuming a positive effect hypothesis
    #Need to divide by two (one-tailed hypothesis)
      One-tailed t test: 
        
        coef_summary <- summary(FinalM4a_SSmodel)$coefficients
      predictor_name <- "avail_activities"  
      
      print(one_tailed_p_value)

      
  #NA vs no NA ----------------------------------------
      
    Preparing NAs Dataset
      
      #Grand mean centering (Hox2002) predictors
      
      ESCA_NAv2$CAge <- (ESCA_NAv2$Age - mean(ESCA_NAv2$Age))
      ESCA_NAv2$CChronic_conditions <- (ESCA_NAv2$Chronic_conditions - mean(ESCA_NAv2$Chronic_conditions))
      ESCA_NAv2$CHS_population <- (ESCA_NAv2$HS_population - mean(ESCA_NAv2$HS_population))
      ESCA_NAv2$CHS_75_living_alone <- (ESCA_NAv2$HS_prop_75_alone - mean(ESCA_NAv2$HS_prop_75_alone))
      ESCA_NAv2$CHS_proportion_population__65 <- (ESCA_NAv2$HS_prop_65 - mean(ESCA_NAv2$HS_prop_65))
      ESCA_NAv2$CHousehold_members <- (ESCA_NAv2$Household_members - mean(ESCA_NAv2$Household_members))
      #Converting categorical variables
      
      ESCA_NAv2$Gender <- factor(ESCA_NAv2$Gender, levels = c(1, 2), labels = c("Man", "Woman"))
      ESCA_NAv2$Interview_wave <- factor(ESCA_NAv2$Interview_wave, levels = c(2017, 2018, 2019, 2020, 2021),
                                               labels = c("2017", "2018", "2019", "2020", "2021"))
      ESCA_NAv2$Education <-factor(ESCA_NAv2$Education, levels = c(1, 2, 3), labels = c("Primary or no studies", "Secondary", "University"))
      ESCA_NAv2$Employment <-factor(ESCA_NAv2$Employment, levels = c(1, 2, 3), labels = c("Active employment", "No active employment", "Domestic work"))
      ESCA_NAv2$Limitation_daily_activities <- factor(ESCA_NAv2$Limitation_daily_activities, 
                                                            levels = c(1, 2, 3), labels = c("Severe limitation", "Limitated, but not severe", "No limitation"))
      ESCA_NAv2$Living_alone <- factor(ESCA_NAv2$Living_alone, levels = c(1, 2), labels = c("Living alone", "Not living alone"))
      ESCA_NAv2$Nationality <- factor(ESCA_NAv2$Nationality, levels = c(1, 2), labels = c("National", "Non-national"))
      ESCA_NAv2$Economic_strain <- factor(ESCA_NAv2$Economic_strain, levels = c(0, 1), labels = c("No economic strain", "Economic strain"))
      ESCA_NAv2$act_quintiles <- factor(ESCA_NAv2$act_quintiles, 
                                              levels = c(1, 2, 3, 4, 5), labels = c("HSActquintile1", "HSActquintile2","HSActquintile3","HSActquintile4","HSActquintile5" ))
      ESCA_Activities$Prop_Act_quintiles <- factor(ESCA_Activities$Prop_Act_quintiles, 
                                                   levels = c(1, 2, 3, 4, 5), labels = c("HSAPropquintile1", "HSPropquintile2","HSPropquintile3","HSPropquintile4","HSPropquintile5" ))
      ESCA_Activities$Health_Sector <- factor(ESCA_Activities$Health_Sector)
      
      unique(ESCA_Activities$Health_Sector)
      sum(is.na(ESCA_Activities))
      sum(!complete.cases(ESCA_Activities))
      
      
      #M4a_SS
      FinalM4a_SSmodel_NA <- lmer(Social_support ~ avail_activities+
                                    Limitation_daily_activities + CChronic_conditions + 
                                    CHousehold_members + Economic_strain + Nationality + 
                                    Interview_wave + (1 | Health_Sector), data = ESCA_NAv2)
      tab_model(FinalM4a_SSmodel_NA)
      AIC(FinalM4a_SSmodel_NA)
      
      #M4b_SS
      
      FinalM4b_SSmodel_NA <- lmer(Social_support ~ uptake + Limitation_daily_activities + 
                                    CChronic_conditions + CHousehold_members + Economic_strain + 
                                    Nationality + Interview_wave + (1 | Health_Sector), 
                                  data = ESCA_NAv2)
      
      tab_model(FinalM4b_SSmodel_NA)
      AIC(FinalM4b_SSmodel_NA)
      
      
      #M4a_MW
      
      FinalM4a_MWmodel_NA <- lmer(Mental_well_being ~ avail_activities + Gender +
                                    CAge + Limitation_daily_activities + CChronic_conditions + 
                                    Education + Economic_strain + 
                                    Interview_wave + (1 | Health_Sector), data = ESCA_NAv2)
      
      tab_model(FinalM4a_MWmodel_NA)
      AIC(FinalM4a_MWmodel_NA)
      
      #M4b_MW
      
      FinalM4b_MWmodel_NA <- lmer(Mental_well_being ~ uptake + Gender + CAge + 
                                    Limitation_daily_activities + CChronic_conditions + 
                                    Education + Economic_strain + Interview_wave + 
                                    (1 | Health_Sector), data = ESCA_NAv2)
      
      tab_model(FinalM4b_MWmodel_NA)
      AIC(FinalM4b_MWmodel_NA)

      
  
      
      
      
      
      
      
      
      
      
      
    
