
library(tidyverse)
library(officer)
library(flextable)
library(lubridate)
library(lmtest)
library(patchwork)

source("load_data.R")
source("modelfunctions.R")
source("data_wrangling.R")

rm(ukb_data_processed)

tf<-function(table_input){
  table_input %>% mutate(samplesize=`FALSE`+`TRUE`,trueprop=round(`TRUE`/samplesize*100,2)) %>% 
    dplyr::select(JiNS,`TRUE`,trueprop,samplesize,everything()) %>% dplyr::select(-`FALSE`) %>% t
}

cbat <- function(TT,SHBG,ALB = 43){
  Kalb <- 3.6*10^4
  Kshbg <- 10^9
  N <- 1 + Kalb*ALB/69000
  a <- N*Kshbg
  b <- N + Kshbg*(SHBG - TT)/10^9
  c <- -TT/10^9
  FT <- (-b + sqrt(b^2 - 4*a*c))/(2*a)*10^9
  cbat <- N*FT
  # return(list(free.T = FT, cbat = cbat))
  return(FT)
}
cbat_v <- Vectorize(cbat)

model_data %>% mutate(FT=cbat_v(Testosterone,SHBG)) %>% mutate(FAI=Testosterone/SHBG*100) -> model_data

sex_plots <- function(table_all,table_men,table_women,title,model){
  rbind(
    table_all %>% pivot_longer(cols=4:9,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="all"),
    table_women %>% pivot_longer(cols=4:9,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="women"),
    table_men %>% pivot_longer(cols=4:9,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="men")
  ) %>%
    as_tibble %>% 
    filter(group!="all") %>%
    separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
    mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
    mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
    mutate(OR=as.numeric(OR)) %>%
    mutate(OR=ifelse(is.na(OR),1,OR)) %>%
    mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
    mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
    separate(Model,into="Model",sep = ":") %>%
    mutate(Model=factor(Model,levels=c("Model 7","Model 6","Model 5","Model 4","Model 3a","Model 3","Model 2a","Model 2","Model 1a","Model 1"))) %>%
    mutate(group=factor(group,levels=c("women","men","all"))) %>%
    mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Irregular shift work","Always"),labels=c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work"))) -> plot_data
  
  pd_width <- 0.6
  
  plot_data %>%
    filter(Model==model) %>%
    ggplot(aes(y=OR,x=JiNS,colour=group)) + 
    geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                  position = position_dodge(width = pd_width)) +
    geom_point(position = position_dodge(width = pd_width)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.position=c(0.8, 0.8),
          legend.background = element_blank())+
    ylab("Odds Ratio") +
    #ylim(c(.6,2)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(plot_data$JiNS)))+
    scale_colour_manual(#values = cbPalette[2:4],
      #values = c("transparent","transparent","black"),
      values = c("black","red"),
      name = element_blank(),guide = guide_legend(reverse=TRUE)) +
    ggtitle(title) -> plot_ms2
  
  return(plot_ms2)
}

# JiNS -----------------------------------------------------------------

model_vec<-c("JiNS + Year_of_birth",
             "JiNS + testosterone_group + Year_of_birth",
             #"JiNS + Testosterone + JiNS*Testosterone + Year_of_birth",
             "JiNS + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS + testosterone_group + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             # "JiNS + Testosterone + JiNS*Testosterone + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med",
             # "JiNS + Testosterone + JiNS*Testosterone + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur",
             "JiNS + testosterone_group + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1",
                 "Model 1a: Model 1 + testosterone",
                 "Model 2",
                 "Model 2a: Model 2 + testosterone",
                 "Model 3",
                 "Model 3a: Model 3 + testosterone")

model_data_men  %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE))-> model_data_temp_men
model_data_women  %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE))-> model_data_temp_women
DependentVar <- "Asthma_def_ms"

#### filter out people with no hormone data
load(file="hormonequartiledata.RData")

model_data_temp_men %>%filter(!is.na(Testosterone)) -> md_temp2
ORmodelrun_4shift(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men

model_data_temp_women %>% filter(!is.na(Testosterone)) -> md_temp2
ORmodelrun_4shift(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"A.","Model 2") + guides(colour="none")  + ylab("Adjusted odds ratio of moderate-\nsevere asthma") +theme(plot.title.position = "plot")+
  sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"","Model 2a") + ggtitle("", subtitle="additionally controlled for\ntestosterone") + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+ ylab("Adjusted odds ratio of moderate-\nsevere asthma") -> p1

# ORmodelrun_4shift(md_temp2%>% filter(Sex==1),DependentVar,model_vec,model_names)-> gg
# lrtest(gg[[2]][[2]],gg[[2]][[1]]) #interaction model 1

model_vec<-c("JiNS + Year_of_birth",
             "JiNS + oestradiol_group + Year_of_birth",
             #"JiNS + Oestradiol + JiNS*Oestradiol + Year_of_birth",
             "JiNS + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS + oestradiol_group + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             #"JiNS + Oestradiol + JiNS*Oestradiol + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med",
             #"JiNS + Oestradiol + JiNS*Oestradiol + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur",
             "JiNS + oestradiol_group + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1",
                 "Model 1a: Model 1 + Oestradiol",
                 "Model 2",
                 "Model 2a: Model 2 + Oestradiol",
                 "Model 3",
                 "Model 3a: Model 3 + Oestradiol")

model_data_temp_men %>% filter(!is.na(Oestradiol)) -> md_temp2
ORmodelrun_4shift(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men

model_data_temp_women %>% filter(!is.na(Oestradiol)) -> md_temp2
ORmodelrun_4shift(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"C.","Model 2") + guides(colour="none")  + ylab("Odds ratio of moderate-\nsevere asthma") +theme(plot.title.position = "plot")+
  sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"","Model 2a") + ggtitle("", subtitle="additionally controlled for\noestradiol") + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+ ylab("Odds ratio of moderate-\nsevere asthma") -> p2

ORmodelrun_4shift(md_temp2%>% filter(Sex==0),DependentVar,model_vec,model_names)-> gg
lrtest(gg[[2]][[2]],gg[[2]][[1]]) #interaction model 1

model_vec<-c("JiNS + Year_of_birth",
             "JiNS + shbg_group + Year_of_birth",
             #"JiNS + SHBG + JiNS*SHBG + Year_of_birth",
             "JiNS + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS + shbg_group + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             #"JiNS + SHBG + JiNS*SHBG + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med",
             #"JiNS + SHBG + JiNS*SHBG + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur",
             "JiNS + shbg_group + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o+Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1",
                 "Model 1a: Model 1 + SHBG",
                 "Model 2",
                 "Model 2a: Model 2 + SHBG",
                 "Model 3",
                 "Model 3a: Model 3 + SHBG")

model_data_temp_men %>% filter(!is.na(SHBG)) -> md_temp2
ORmodelrun_4shift(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men

model_data_temp_women %>% filter(!is.na(SHBG)) -> md_temp2
ORmodelrun_4shift(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"B.","Model 2") + guides(colour="none")  + ylab("Odds ratio of moderate-\nsevere asthma") +theme(plot.title.position = "plot")+
  sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"","Model 2a") + ggtitle("", subtitle="additionally controlled for\nSHBG") + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+ ylab("Odds ratio of moderate-\nsevere asthma") -> p3

ORmodelrun_4shift(md_temp2%>% filter(Sex==0),DependentVar,model_vec,model_names)-> gg
lrtest(gg[[2]][[2]],gg[[2]][[1]]) #interaction model 1

ggsave(filename="plots/hormones_additionallycontrolledbyquarts_filt.png",plot = p1/p3/p2,width=6,height=10)
################################################

# chronotype --------------------------------------------------------------


model_vec<-c("Chronotype  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
             "Chronotype  + Testosterone + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
             "Chronotype  + Oestradiol + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
             "Chronotype  + SHBG + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
             "Chronotype  + FT + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
             "Chronotype  + Testosterone + Oestradiol + SHBG + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired")
model_names <- c("Model 2: Adjusted by multivariate covariates.",
                 "Model 3: Model 2 + testosterone",
                 "Model 4: Model 2 + oestradiol",
                 "Model 5: Model 2 + SHBG",
                 "Model 6: Model 2 + FT",
                 "Model 7: Model 2 + test oest SHBG")

model_data  %>%
  # filter(!is.na(JiNS)) %>%
  # filter(!is.na(Sex)) %>%
  # filter(!is.na(Year_of_birth)) %>%
  # filter(!is.na(Ethnicity)) %>%
  # filter(!is.na(TDI)) %>%
  # filter(!is.na(SleepDur)) %>%
  # filter(!is.na(Smoking)) %>%
  filter(!is.na(Packyears_nn)) %>%
  # filter(!is.na(Alc_daily)) %>%
  # filter(!is.na(LengthofWW)) %>%
  # filter(!is.na(BMI)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE))-> model_data_temp
DependentVar <- "Asthma_def_ms"


#### filter out people with no hormone data

model_data_temp %>% filter(!is.na(Testosterone)) -> md_temp2

ORmodelrun_chrono(md_temp2,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_chrono(md_temp2%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women
ORmodelrun_chrono(md_temp2%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"B.","Model 2") + guides(colour="none") + ylab("Odds ratio of moderate-\nsevere asthma") +theme(plot.title.position = "plot")+
  sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"","Model 3") + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + ylab("Odds ratio of moderate-\nsevere asthma")


