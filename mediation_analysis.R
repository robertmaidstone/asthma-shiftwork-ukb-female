

library(tidyverse)
library(officer)
library(flextable)
library(lubridate)
library(mediation)

source("load_data.R")
source("data_wrangling.R")


# medication analysis -----------------------------------------------------
# potential mediators Smoking_n , Packyears_nn , BMI_o , SleepDur ####
model_data  %>%
  filter(!is.na(JiNS)) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp

mod1 <- lm(BMI_o ~  JiNS  + Year_of_birth ,model_data_temp)
mod2 <- glm(Asthma_def_ms ~  JiNS  + Year_of_birth + BMI_o,model_data_temp,family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS",mediator = "BMI_o",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_bmi_m1 <- results

mod1 <- lm(BMI_o ~  JiNS  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o,model_data_temp)
mod2 <- glm(Asthma_def_ms ~  JiNS  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + BMI_o,model_data_temp,family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS",mediator = "BMI_o",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_bmi <- results

save(file = "data/mediationresults.RData",results_bmi_m1,results_bmi)

#####

mod1 <- lm(Packyears_nn ~  JiNS + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous  + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired,model_data_temp)
mod2 <- glm(Asthma_def_ms ~  JiNS + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired + Packyears_nn,model_data_temp,family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS",mediator = "Packyears_nn",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_packyears <- results

mod1 <- lm(SleepDur ~  JiNS + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous  + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired,model_data_temp)
mod2 <- glm(Asthma_def_ms ~  JiNS + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired + SleepDur,model_data_temp,family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS",mediator = "SleepDur",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_sleepdur <- results

model_data_temp %>% mutate(Smoking2=ifelse((Smoking_n=="current heavy smoker")|(Smoking_n=="current occassional smoker, previously a heavy smoker")|(Smoking_n=="not a current smoker, smoked heavily previously"),1,0)) -> model_data_temp2
mod1 <- glm(Smoking2 ~  JiNS + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous  + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired
                 ,model_data_temp2,family = binomial(link="logit"))
mod2 <- glm(Asthma_def_ms ~  JiNS + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired
            + Smoking2,model_data_temp2,family = binomial(link="logit"))
results2 <- mediate(mod1,mod2, treat = "JiNS",mediator = "Smoking2",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results2 %>% summary

results_smoking <- results2

save(file = "data/mediationresults2.RData",results_bmi,results_packyears,results_sleepdur,results_smoking)
