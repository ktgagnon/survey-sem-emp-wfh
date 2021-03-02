library(tidyverse)
library(magrittr)
library(psych)
library(GPArotation)
library(mice)
library(lavaan)
library(semPlot)

setwd("C:/Users/KG050542/OneDrive - Cerner Corporation/Desktop")

# read file into workspace
data <- read.csv("COVID_Survey_DataCon2020.csv", header=TRUE, stringsAsFactors=FALSE)

colnames(data)[grep("Duration*",colnames(data))] <- "Duration"

# Change prior travel binary question into 0 and 1 numeric.
data$Q5 <- recode(data$Q5, "No"=0,"Yes"=1)
data$Q21 <- recode(data$Q21, "No"=0, "Yes"=1)

# recode and alter data types of many variables
# turn these columns into numeric
num_data <- data %>% 
    select(c(1,4,6:13)) %>% 
    mutate_each(funs(as.numeric))

# recode these columnes into 5 point numeric scale, with neutral = 0
likert_data <- data %>% 
    select(c(16:20)) %>% 
    mutate_each(funs(recode(.,"Strongly agree"=2
                            ,"Agree"=1
                            ,"Neutral"=0
                            ,"Disagree"=-1
                            ,"Strongly disagree"=-2
                            , "Very positively"=2
                            ,"Somewhat positively"=1
                            ,"No change in my productivty"=0
                            ,"Somewhat negatively"=-1
                            ,"Very negatively"=-2)))

# change these categorical variables into factors
categ_data <- data %>%
    select(c(14:15,21:23)) %>%
    mutate_each(funs(as.factor))



# For the sake of time, taking the more risky approach and concatenating 
# data back together. Should probably have joined by unique ids, but 
# hadn't carried those through. Since it's a small and one time dataset, 
# this should be okay though.

# bring all the data back together again.
ds <- cbind(num_data, likert_data, categ_data, "Finished"=data$Finished)

# filter outRecaptcha scores below .7 as well as any progress less than 30%.
# This removes 810 rows, or about 5% of the total dataset.
ds <- ds %>% filter(Q_RecaptchaScore >= 0.5 & Progress >= 30) 

#colnames(ds) <- c("Index","Duration","Progress","Q_RecaptchaScore", "Sadness", "Stress","Pessimism"
#                  ,"Nervous","Loneliness","Unsupported"
#                 ,"Cerner_Guidance","Work_Pers_Manage","Perc_Prod","Resource_Work_Eff"
#                  ,"Overall_Cerner_Management","Work_Locale","Travel_for_Work","Job_Fam_Grp","Direct_Reports"
#                  ,"Travel_for_Work_Freq","Finished_Survey")

colnames(ds) <- c('Index','Duration','Progress','Captcha','Happy_Sad','Relax_Stressed','Optimistic_Pessimistic','Calm_Nervous','Connected_Lonely','Supported_Unsupported','Cerner_Guidance','Work_Management','Work_Productivity','Resources_Availability', 'Overall_Cerner_Management','Work_Location','Travel_for_Work','Job_Family','Direct_Reports','Travel_Frequency', 'Finished')

ds$Travel_for_Work_Freq_recode <- ifelse(ds$Travel_for_Work==0 & ds$Travel_for_Work_Freq=="", "Never", as.character(ds$Travel_for_Work_Freq))
ds$Travel_for_Work_Freq_num <- recode("Never"=0, "Annually"=1, "Quarterly"=4, "Monthly"=12, "Weekly"=52)
# Use multiple imputations to handle missingness. This works for missing at random (MAR) and missing completely at random (MCAR).
# Later, we can leverage maximum likelihood estimation of the model to handle MAR and MCAR data without creating multiple different imputed datasets.
#ds_imputed <- mice(ds, m=10, maxit = 100, method='pmm', seed=2020)

# Subset analytical dataset to only quantified variables and sample 50% of rows for exploratory factor analysis
ds_samp <-sample_n(ds,nrow(ds)/2) %>% select("Index")
ds_efa <- ds %>% filter(Index %in% ds_samp$Index) %>% select(c(5:15))


efa_fit_5 <- fa(ds_efa, nfactors=5, n.obs=nrow(ds_efa))
efa_fit_5

efa_fit_3 <- fa(ds_efa, nfactors=3, n.obs=nrow(ds_efa))
efa_fit_3 

ds_cfa <- ds %>% filter(!(Index %in% ds_samp$Index))


meas_model_3f <- 'neg_affect =~ Stress + Sadness + Pessimism + Nervous + Loneliness + Unsupported
          productivity =~ Perc_Prod + Work_Pers_Manage + Resource_Work_Eff

          productivity ~ Travel_for_Work_Freq_num + Direct_Reports + Cerner_Guidance
          Overall_Cerner_Management ~ Travel_for_Work_Freq_num + Direct_Reports
          neg_affect ~ productivity + Overall_Cerner_Management'

mm_3f_fit <- cfa(meas_model_3f, data = ds_cfa)

semPaths(mm_3f_fit)


##########################################################
################# S E M ##################################
##########################################################

### Measurement Models
NA_PROD <-
  ' Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely
    Productivity =~ Work_Productivity + Work_Management + Resources_Availability

    Negative_Affect ~~ Productivity
  '
NA_PROD_fit <- sem(NA_PROD, data = ds,  missing="ML", estimator="MLR")
summary(NA_PROD_fit, fit.measures=TRUE)
#Akaike (AIC)                              738416.462  738416.462
#Bayesian (BIC)                            738608.671  738608.671
#Sample-size adjusted Bayesian (BIC)       738529.223  738529.223
#Robust RMSEA                                               0.082
#90 Percent confidence interval - lower                     0.079
#90 Percent confidence interval - upper                     0.086
#Model Test User Model:
#  Standard      Robust
#Test Statistic                               2096.960    1510.780
#Degrees of freedom                                 19          19
#P-value (Chi-square)                            0.000       0.000
#Scaling correction factor                                   1.388
#Yuan-Bentler correction (Mplus variant) 

#
NA_PROD_CERN <- 
  ' Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely
    Productivity =~ Work_Productivity + Work_Management + Resources_Availability
    Cerner_Perception =~ Cerner_Guidance + Overall_Cerner_Management

    Negative_Affect ~~ Productivity
    Productivity ~~ Cerner_Perception
    Negative_Affect ~~ Cerner_Perception
    Supported_Unsupported ~~ Negative_Affect 
    Supported_Unsupported ~~ Productivity
    Supported_Unsupported ~~ Cerner_Perception
  '
NA_PROD_CERN_fit <- sem(NA_PROD_CERN, data = ds,  missing="ML", estimator="MLR")
summary(NA_PROD_CERN_fit, fit.measures=TRUE)
#Akaike (AIC)                              793602.235  793602.235
#Bayesian (BIC)                            793855.954  793855.954
#Sample-size adjusted Bayesian (BIC)       793751.082  793751.082
#Robust RMSEA                                               0.072
#90 Percent confidence interval - lower                     0.070
#90 Percent confidence interval - upper                     0.075
#Model Test User Model:
#  Standard      Robust
#Test Statistic                               2753.575    2012.729
#Degrees of freedom                                 32          32
#P-value (Chi-square)                            0.000       0.000
#Scaling correction factor                                   1.368
#Yuan-Bentler correction (Mplus variant) 

NA_CON_SUPP <- 
  ' Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely
    Control =~ Work_Management + Resources_Availability
    Support =~ Cerner_Guidance + Overall_Cerner_Management + Supported_Unsupported
  
    Negative_Affect ~~ Control
    Control ~~ Support
    Support ~~ Negative_Affect
  '
NA_CON_SUPP_fit <- sem(NA_CON_SUPP, data = ds,  missing="ML", estimator="MLR")
summary(NA_CON_SUPP_fit, fit.measures=TRUE)
#Akaike (AIC)                              877672.665  877672.665
#Bayesian (BIC)                            877926.387  877926.387
#Sample-size adjusted Bayesian (BIC)       877821.516  877821.516
#Robust RMSEA                                               0.128
#90 Percent confidence interval - lower                     0.125
#90 Percent confidence interval - upper                     0.131
#Model Test User Model:
#  Standard      Robust
#Test Statistic                               8506.624    5658.520
#Degrees of freedom                                 32          32
#P-value (Chi-square)                            0.000       0.000
#Scaling correction factor                                   1.503
#Yuan-Bentler correction (Mplus variant) 
######################


aff_prod_model <- 'Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely
          Productivity =~ Work_Productivity + Work_Management + Resources_Availability

          Negative_Affect ~ Travel_for_Work + Supported_Unsupported + Overall_Cerner_Management
          Supported_Unsupported ~ Travel_for_Work + Cerner_Guidance + Overall_Cerner_Management
          Cerner_Guidance ~ Travel_for_Work
          Overall_Cerner_Management ~ Travel_for_Work
          Productivity ~ Supported_Unsupported + Cerner_Guidance + Travel_for_Work + Negative_Affect'

aff_prod_model_fit <- sem(aff_prod_model, data = ds,  se="bootstrap", missing="ML", estimator="MLR")
summary(aff_prod_model_fit, fit.measures=TRUE)

aff_prod_cern_perc_model <- 
'Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely
Productivity =~ Work_Productivity + Work_Management + Resources_Availability
Cerner_Perception =~ Cerner_Guidance + Overall_Cerner_Management

Negative_Affect ~ Travel_for_Work + Supported_Unsupported + Cerner_Perception
Supported_Unsupported ~ Travel_for_Work + Cerner_Perception
Cerner_Perception ~ Travel_for_Work
Productivity ~ Supported_Unsupported + Cerner_Perception + Travel_for_Work + Negative_Affect'

aff_prod_cern_perc_model_fit <- sem(aff_prod_cern_perc_model, data = ds,  missing="ML", estimator="MLR")
summary(aff_prod_cern_perc_model_fit, fit.measures=TRUE)


jdcsa_model <- 'Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely
                Control =~ Work_Management + Resources_Availability
                Support =~ Cerner_Guidance + Overall_Cerner_Management + Supported_Unsupported

                Work_Productivity ~ Negative_Affect + Control + Support + Travel_for_Work
                Negative_Affect ~ Control + Support + Travel_for_Work
                Control ~ Travel_for_Work
                Support ~ Travel_for_Work'

jdcsa_model_fit <- sem(jdcsa_model, data = ds, missing="ML", estimator="MLR")
summary(jdcsa_model_fit, fit.measures=TRUE)

#Affect and prod, simplest model
affect_prod_model_simple <- 
'Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely + Supported_Unsupported
Productivity =~ Cerner_Guidance + Overall_Cerner_Management + Work_Management + Resources_Availability + Work_Productivity

Productivity ~ Travel_for_Work + Negative_Affect
Negative_Affect ~ Travel_for_Work'
affect_prod_model_simple_fit <- sem(affect_prod_model_simple, data = ds, missing="ML", estimator="MLR")
summary(affect_prod_model_simple_fit, fit.measures=TRUE)

#Affect and prod, simplest model, but now productivity predicts affect
affect_prod_model_simple2 <- 
  'Negative_Affect =~ Happy_Sad + Relax_Stressed + Optimistic_Pessimistic + Calm_Nervous + Connected_Lonely + Supported_Unsupported
Productivity =~ Cerner_Guidance + Overall_Cerner_Management + Work_Management + Resources_Availability + Work_Productivity

Negative_Affect ~ Travel_for_Work + Productivity
Productivity ~ Travel_for_Work'
affect_prod_model_simple2_fit <- sem(affect_prod_model_simple2, data = ds, missing="ML", estimator="MLR")
summary(affect_prod_model_simple2_fit, fit.measures=TRUE)


library(ggplot2)
library(tidyr)
library(purrr)


ds %>% 
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

emo_data_check <- ds %>% 
  select(c("Index","Loneliness","Nervous","Pessimism","Sadness","Stress","Unsupported"
           ,"Cerner_Guidance","Work_Pers_Manage","Perc_Prod","Resource_Work_Eff","Overall_Cerner_Management")) %>% 
  pivot_longer(.
               , cols=(c("Index","Loneliness","Nervous","Pessimism","Sadness","Stress","Unsupported"
                         ,"Cerner_Guidance","Work_Pers_Manage","Perc_Prod","Resource_Work_Eff","Overall_Cerner_Management"))
               , "vars") %>% 
  group_by(Index) %>% 
  summarize(stdev = sd(value, na.rm=TRUE)
            ,total = sum(value, na.rm=TRUE))


