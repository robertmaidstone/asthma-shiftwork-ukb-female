

library(tidyverse)
library(officer)
library(flextable)
library(lubridate)
setwd("~/Women & Asthma/UKB")
source("load_data.R")
source("ORmodelfunctions.R")
source("ORdatawrangling_operations.R")

rm(ukb_data_processed)

# funcs -------------------------------------------------------------
ORmodelrun.hormone_group<-function(model_data,DependentVar,model_vec,model_names,hormone_group){
  model_vec %>% str_split(pattern ="[+]",n = 2) %>% unlist %>% .[c(2,4,6)] -> model_vec
  model_vec<-paste(DependentVar,"~",hormone_group,"+",model_vec)
  
  model_data %>%
    dplyr::select(DependentVar,hormone_group) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(",hormone_group,")) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq)  -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(hormone_group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:4],confint.default(mod3,2:4))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = hormone_group) %>% unlist %>% .[(1:3)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x=hormone_group,by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}

trend.GRS<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  tab_1 <- c()
  str_replace(model_vec,"GRS_group","SCORE") -> model_vec
  
  
  model_list <-list()
  ptrend <- c()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(GRS_group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    anova(mod3,test="Chisq")[2,5]->ptrend[i]
    
    exp(cbind(coef(mod3)[2],confint.default(mod3,2))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    
    tab_1<-rbind(tab_1,c(i,kk_temp$OR,summary(mod3)$coefficients[2,1:2]))
  }
  colnames(tab_1)<-c("model","OR","beta","SE")
  return(list(ptrend,tab_1))
}

##

model_data %>% mutate(ovariesremoved=ovariesremoved_OPCS3|ovariesremoved_OPCS4) %>%
  mutate(Menopause=ifelse(Menopause==2,ifelse(ovariesremoved==TRUE,2.5,2),Menopause)) -> model_data
model_data %>% filter(Sex==1) -> model_data_men
model_data %>% filter(Sex==0) -> model_data_women

md2<-model_data
model_data %>% mutate(JiNS=JiNS_o)  -> model_data

model_vec<-c("JiNS + Sex + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "JiNS + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "JiNS + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired +Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates +  mediators (95% CI)")

model_vec %>% strsplit(split = "[+ ]") %>% unlist -> mv
mv[mv!=""] %>% unique -> mv

sum_na <- function(x){sum(is.na(x))}
model_data %>% dplyr::select(JiNS,mv,WW="X2316.0.0","FEV1lt80","Asthma_def","Asthma_def_ms") %>% apply(2,sum_na)

model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) -> model_data_temp
DependentVar <- "Asthma_def_ms"

###

model_data_women %>% mutate(JiNS=JiNS_o)%>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                                                     (Asthma_def_ms==TRUE)) %>% filter(!is.na(Packyears_nn)) -> model_data_women
model_data_men %>% mutate(JiNS=JiNS_o)%>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                                                   (Asthma_def_ms==TRUE)) %>% filter(!is.na(Packyears_nn)) -> model_data_men

ORmodelrun_4shift(model_data_women%>% filter(Menopause==1),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause
ORmodelrun_4shift(model_data_women%>% filter(Menopause==0),DependentVar,model_vec,model_names)[[1]] -> ms_women_premenopause
ORmodelrun_4shift(model_data_women%>% filter(Menopause%in%c(2,2.5)),DependentVar,model_vec,model_names)[[1]] -> ms_women_hyst
#ORmodelrun_4shift(model_data_women%>% filter(Menopause==2.5),DependentVar,model_vec,model_names)[[1]] -> ms_women_hystovaries
ORmodelrun_4shift(model_data_women%>% filter((Menopause==0)&(Age<=50)),DependentVar,model_vec,model_names)[[1]] -> ms_women_premenopause2
ORmodelrun_4shift(model_data_women%>% filter((Menopause==1)|(Age>50 & !(Menopause %in% c(2,2.5)))),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause2
ORmodelrun_4shift(model_data_women%>% filter((Menopause%in%c(2,2.5))),DependentVar,model_vec,model_names)[[1]] -> ms_women_hyst2
#ORmodelrun_4shift(model_data_women%>% filter((Menopause==2.5)),DependentVar,model_vec,model_names)[[1]] -> ms_women_hystovaries2
#ORmodelrun_4shift(model_data_women%>% filter(((Menopause==1)|(Age>50))&((HRT==0))&(OCP==0)),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause3
#ORmodelrun_4shift(model_data_women%>% filter(((Menopause==1)|(Age>50))&(HRTcurr==0)&(OCPcurr==0)),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause4

ORmodelrun_4shift(model_data_women%>% filter(((Menopause==1)|(Age>50 & !(Menopause %in% c(2,2.5))))&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause4
ORmodelrun_4shift(model_data_women%>% filter(((Menopause==0)&(Age<=50))&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))),DependentVar,model_vec,model_names)[[1]] -> ms_women_premenopause4
ORmodelrun_4shift(model_data_women%>% filter(((Menopause%in%c(2,2.5)))&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))),DependentVar,model_vec,model_names)[[1]] -> ms_women_hyst4
#ORmodelrun_4shift(model_data_women%>% filter(((Menopause==2.5))&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))),DependentVar,model_vec,model_names)[[1]] -> ms_women_hystovaries4

##

rbind(
  ms_women_postmenopause %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`)  %>% mutate(Menopause="Postmenopausal"),
  ms_women_postmenopause2 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`)  %>% mutate(Menopause="Postmenopausal2"),
  ms_women_postmenopause4 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`)  %>% mutate(Menopause="Postmenopausal4"),
  ms_women_premenopause %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Premenopausal"),
  ms_women_premenopause2 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Premenopausal2"),
  ms_women_premenopause4 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Premenopausal4")

) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) -> plot_data

##

rbind(
  ms_women_postmenopause %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`)  %>% mutate(Menopause="Postmenopausal"),
  ms_women_postmenopause2 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`)  %>% mutate(Menopause="Postmenopausal2"),
  ms_women_postmenopause4 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`)  %>% mutate(Menopause="Postmenopausal4"),
  ms_women_premenopause %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Premenopausal"),
  ms_women_premenopause2 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Premenopausal2"),
  ms_women_premenopause4 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Premenopausal4"),
  ms_women_hyst %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Hyst"),
  ms_women_hyst2 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Hyst2"),
  ms_women_hyst4 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="Hyst4")
#  ms_women_hystovaries %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="HystOvaries"),
 # ms_women_hystovaries2 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="HystOvaries2"),
#  ms_women_hystovaries4 %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(Menopause="HystOvaries4")
) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) -> plot_data

####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pd_width <- 0.6

plot_data %>%
  filter(Model=="Model 2") %>%
  filter(Menopause%in%c("Premenopausal","Postmenopausal","Hyst","HystOvaries")) %>%
  mutate(Menopause=factor(Menopause,levels=rev(c("Premenopausal","Postmenopausal","Hyst","HystOvaries")),labels=rev(c("Premenopausal","Postmenopausal","Unsure - Hysterectomy","Unsure - Hysterectomy\nwith removal of ovaries")))) %>%
  mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Irregular shift work","Always"),labels=c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work"))) %>%
  ggplot(aes(y=OR,x=JiNS,colour=Menopause)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  coord_flip(ylim=c(0,5)) +
  scale_x_discrete(limits = rev(c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work")))+ 
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c("blue","black","red"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma")+
  ggtitle("A. Self-reported") -> p1

plot_data %>%
  filter(Model=="Model 2") %>%
  filter(Menopause%in%c("Premenopausal2","Postmenopausal2","Hyst2","HystOvaries2")) %>%
  mutate(Menopause=factor(Menopause,levels=rev(c("Premenopausal2","Postmenopausal2","Hyst2","HystOvaries2")),labels=rev(c("Premenopausal","Postmenopausal","Unsure - Hysterectomy","Unsure - Hysterectomy\nwith removal of ovaries")))) %>%
  mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Irregular shift work","Always"),labels=c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work"))) %>%
  ggplot(aes(y=OR,x=JiNS,colour=Menopause)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  coord_flip(ylim=c(0,5)) +
  scale_x_discrete(limits = rev(c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work")))+ 
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c("blue","black","red"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma")+
  ggtitle("A. Self-reported or >50") -> p2


plot_data %>%
  filter(Model=="Model 2") %>%
  filter(Menopause%in%c("Premenopausal4","Postmenopausal4","Hyst4","HystOvaries4")) %>%
  mutate(Menopause=factor(Menopause,levels=rev(c("Premenopausal4","Postmenopausal4","Hyst4","HystOvaries4")),labels=rev(c("Premenopausal","Postmenopausal","Unsure-\nHysterectomy","Unsure-Hysterectomy\nwith Oophorectomy")))) %>%
  mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Irregular shift work","Always"),labels=c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work"))) %>%
  ggplot(aes(y=OR,x=JiNS,colour=Menopause)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  #ylim(c(0,80)) +
  coord_flip(ylim=c(0,5)) +
  scale_x_discrete(limits = rev(c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work")))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c("blue","black","red"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma") +
  ggtitle("B. A with no HRT or current OCP")-> p4


library(patchwork)
p1+p2 + p4

ggsave(filename="plots/menoshift_hystonly.png",p1+guides(colour="none")+p2+guides(colour="none")+p4+ theme(legend.position = c(0.75, 0.85)),width=12,height=4)
ggsave(filename="plots/menoshift_hystonly_n.png",p2+guides(colour="none")+p4+ theme(legend.position = c(0.75, 0.85)),width=8,height=3.5)


model_vec<-c("JiNS + Sex + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "JiNS + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "JiNS + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired +Smoking_n + Packyears_nn + BMI_o + SleepDur")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates +  mediators (95% CI)")

ORmodelrun_4shift(model_data_women%>% filter(((Menopause==1)|(Age>50))&((HRTcurr==0)|(HRT==0))&((OCPcurr==0)|OCP==0)),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause3


p1 + ggtitle("A.") + theme(plot.title.position = "plot") + guides(colour="none") + ylab("Odds ratio of moderate-\nsevere asthma")


#####

##p-values

# model_vec<-c("JiNS + Sex + Year_of_birth  + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype_o + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
#              "JiNS + JiNS*Sex + Sex + Year_of_birth  + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype_o + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired")
# model_names <- c("No Int",
#                  "Int")




model_vec<-c("JiNS  + Sex + Year_of_birth + Menopause",
             "JiNS  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Menopause",
             "JiNS  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med + Menopause",
             "JiNS   + Sex + Year_of_birth + Menopause + Menopause*JiNS",
             "JiNS   + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Menopause + Menopause*JiNS",
             "JiNS   + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med + Menopause + Menopause*JiNS")
model_names <- c("Model 1",
                 "Model 2",
                 "Model 3",
                 "Model 1 int",
                 "Model 2 int",
                 "Model 3 int")

model_data_women %>%
  mutate(Menopause = case_when(
    Menopause == 0  ~ 0,
    (Menopause == 1) ~ 1,
    (Menopause == 2) | (Menopause == 2.5) ~ 2,
  )) %>%
  filter(!is.na(Menopause)) %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3

# ORmodelrun_4shift(model_data_women%>% filter((Menopause==1)|(Age>50 & !(Menopause %in% c(2,2.5)))),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause2


model_data_women %>%
  filter(((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))) %>%
  mutate(Menopause = case_when(
    Menopause == 0 & Age <=50 ~ 0,
    (Menopause == 1) | (Age > 50 & !((Menopause %in% c(2,2.5)))) ~ 1,
    (Menopause == 2) | (Menopause == 2.5) ~ 2,
  )) %>%
  filter(!is.na(Menopause)) %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3


model_data_women %>%
  mutate(Menopause = case_when(
    Menopause == 0 & Age <=50 ~ 0,
    (Menopause == 1) | (Age > 50 & !((Menopause %in% c(2,2.5)))) ~ 1,
    (Menopause == 2) | (Menopause == 2.5) ~ 2,
  )) %>%
  filter(!is.na(Menopause)) %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"
model_data_women  %>%
  filter(Menopause%in%c(0,1)) %>%
  mutate(Menopause=Menopause&(Age<=50)) %>%
  filter(((HRTcurr==0)|(HRT==0))&((OCPcurr==0)|OCP==0)) %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3
