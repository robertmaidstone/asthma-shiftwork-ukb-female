library(tidyverse)
library(officer)
library(flextable)
library(lubridate)
library(lmtest)

source("load_data.R")
source("modelfunctions.R")
source("data_wrangling.R")

tf<-function(table_input){
  table_input %>% mutate(samplesize=`FALSE`+`TRUE`,trueprop=round(`TRUE`/samplesize*100,2)) %>% 
    dplyr::select(Chronotype,`TRUE`,trueprop,samplesize,everything()) %>% dplyr::select(-`FALSE`) %>% t
}

tf2<-function(table_input){
  table_input %>% mutate(samplesize=`0`+`1`,trueprop=round(`1`/samplesize*100,2)) %>% 
    dplyr::select(Chronotype,`1`,trueprop,samplesize,everything()) %>% dplyr::select(-`0`) %>% t
}

rm(ukb_data_processed)
tf<-function(table_input){
  table_input %>% mutate(samplesize=`FALSE`+`TRUE`,trueprop=round(`TRUE`/samplesize*100,2)) %>% 
    dplyr::select(JiNS,`TRUE`,trueprop,samplesize,everything()) %>% dplyr::select(-`FALSE`) %>% t
}
sex_plots <- function(table_all,table_men,table_women,title,model){
  rbind(
    table_all %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="all"),
    table_women %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="women"),
    table_men %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="men")
  ) %>%
    as_tibble %>% 
    mutate(Chronotype=factor(Chronotype,levels=levels(Chronotype)[1:3])) %>%
    separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
    mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
    mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
    mutate(OR=as.numeric(OR)) %>%
    mutate(OR=ifelse(is.na(OR),1,OR)) %>%
    mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
    mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
    separate(Model,into="Model",sep = ":") %>%
    mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
    mutate(group=factor(group,levels=c("women","men","all"))) %>%
    mutate(Chronotype=factor(Chronotype,levels=c("Intermediate chronotype","Definitely a morning person","Definitely an evening person"),labels=c("Intermediate\nchronotype","Definitely a\nmorning person","Definitely an\nevening person"))) -> plot_data
  
  cbPalette <- c("red","black","red")
  pd_width <- 0.6
  
  plot_data %>%
    filter(Model==model) %>%
    filter(group!="all") %>%
    ggplot(aes(y=OR,x=Chronotype,colour=group)) + 
    geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                  position = position_dodge(width = pd_width)) +
    geom_point(position = position_dodge(width = pd_width)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.position=c(0.8, 0.85),
          legend.background = element_blank())+
    ylab("Odds Ratio") +
    ylim(c(.6,1.5)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(plot_data$Chronotype)))+
    scale_colour_manual(#values = cbPalette[2:4],
      #values = c("transparent","transparent","black"),
      values = c(cbPalette[c(2,3)],"black"),
      name = element_blank(),guide = guide_legend(reverse=TRUE)) +
    ggtitle(title) -> plot_ms2
  
  return(plot_ms2)
}

# JiNS -----------------------------------------------------------------

model_vec<-c("Chronotype + Sex + Year_of_birth",
             "Chronotype + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
             "Chronotype + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1: Age adjusted.",
                 #"Model 2: Adjusted by age, smoking status, pack years, alcohol status, daily alcohol intake, ethnicity, TDI, days exercised (walked, moderate, vigorous), chronotype, length of working week, job asthma risk, job medical required.",
                 "Model 2: Adjusted by multivariate covariates.",
                 "Model 3: Model 2 covariates + mediators")

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
   filter(!is.na(LengthofWW)) %>%
  # filter(!is.na(BMI)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE))-> model_data_temp
DependentVar <- "Asthma_def_ms"

#ORmodelrun_chrono(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_chrono(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women
ORmodelrun_chrono(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men


model_vec<-c("Chronotype  + Sex + Year_of_birth",
             "Chronotype  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired ",
             "Chronotype  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired  + Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med",
             "Chronotype + Chronotype*Sex  + Sex + Year_of_birth",
             "Chronotype + Chronotype*Sex  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired ",
             "Chronotype + Chronotype*Sex  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired  + Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1",
                 "Model 2",
                 "Model 3",
                 "Model 1 int",
                 "Model 2 int",
                 "Model 3 int")


model_data  %>%
  filter(!is.na(Packyears_nn)) %>%
  filter(!is.na(LengthofWW)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE))-> model_data_temp
DependentVar <- "Asthma_def_ms"

ORmodelrun_chrono(model_data_temp,DependentVar,model_vec,model_names) -> temp
lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3


####

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"A.","Model 2")+annotate("text", x=3.4, y=.67, label= "p = 0.91") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma") + guides(colour="none") +
sex_plots(asthma_all,asthma_men,asthma_women,"B.","Model 2")+annotate("text", x=3.4, y=.67, label= "p = 0.15") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of any asthma")+
  sex_plots(WW_all,WW_men,WW_women,"C.","Model 2")+annotate("text", x=3.4, y=.67, label= "p = 0.01") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of experiencing\nwheeze or whistling in chest")+ guides(colour="none") +
  sex_plots(fev_all,fev_men,fev_women,"D.","Model 2")+annotate("text", x=3.4, y=.67, label= "p = 0.31") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of having critical\nFEV1 predicted percentage")+ guides(colour="none") -> p_fig2

ggsave(plot= p_fig2,filename="plots/fig2.png",width=8,height=6.5)


fev_women %>% mutate(n=`FALSE`+`TRUE`) %>% 
  mutate(p=round(`TRUE`/n*100,2)) %>% 
  mutate(case=paste0(`TRUE`," (",p,"%)")) %>%
  dplyr::select(Chronotype,case,n,everything()) %>% dplyr::select(-`FALSE`,-`TRUE`,-p) %>% t
fev_men %>% mutate(n=`FALSE`+`TRUE`) %>% 
  mutate(p=round(`TRUE`/n*100,2)) %>% 
  mutate(case=paste0(`TRUE`," (",p,"%)")) %>%
  dplyr::select(Chronotype,case,n,everything()) %>% dplyr::select(-`FALSE`,-`TRUE`,-p) %>% t 

## (A; n=403,242; 175,325 men and 227,917 women), 
###moderate-severe asthma (B; n=385,734; 168,552 men and 217,182 women), 
###experiencing wheeze or whistling in chest within the last year (C; n=425,708; 184,346 men and 241,362 women) 
### FEV1 predicted percentage (D; n=136,325; 55,146 men and 81,179 women). 