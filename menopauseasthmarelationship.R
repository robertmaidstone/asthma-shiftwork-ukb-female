library(tidyverse)
library(officer)
library(flextable)
library(lubridate)

source("load_data.R")
source("modelfunctions.R")
source("data_wrangling_operations.R")

rm(ukb_data_processed)

model_data %>% dplyr::select(ovariesremoved_OPCS3) %>% table
model_data %>% dplyr::select(ovariesremoved_OPCS4) %>% table

model_data %>% mutate(ovariesremoved=ovariesremoved_OPCS3|ovariesremoved_OPCS4)%>%
  mutate(hysterectomy=hysterectomy_OPCS3|hysterectomy_OPCS4) -> model_data

model_data %>% dplyr::select(ovariesremoved_OPCS4,hysterectomy_OPCS4) %>% table
model_data %>% dplyr::select(ovariesremoved_OPCS3,hysterectomy_OPCS3) %>% table
model_data %>% dplyr::select(ovariesremoved,hysterectomy) %>% table

model_vec<-c("Menopause + Sex + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "Menopause + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "Menopause + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired +Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates +  mediators (95% CI)")



#ORmodelrun_4shift(model_data_women%>% filter(Menopause==1),DependentVar,model_vec,model_names)[[1]] -> ms_women_postmenopause

DependentVar <- "Asthma_def_ms"

  model_vec2<-paste("Asthma_def_ms","~",model_vec)
  
  model_data %>%
    filter(!is.na(Packyears_nn)) %>%
    filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
    mutate(Menopause=ifelse(Menopause==2,ifelse(ovariesremoved==TRUE,2.5,2),Menopause)) %>%
    mutate(Menopause=ifelse(Menopause==2,ifelse(hysterectomy==TRUE,2,NA),Menopause)) %>%
    dplyr::select(all_of("Asthma_def_ms"),Menopause) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(Menopause)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq)  -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec2[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(Packyears_nn)) %>%
      filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
      mutate(Menopause=ifelse(Menopause==2,ifelse(ovariesremoved==TRUE,2.5,2),Menopause)) %>%
      mutate(Menopause=ifelse(Menopause==2,ifelse(hysterectomy==TRUE,2,NA),Menopause)) %>%
      mutate(Menopause=factor(Menopause,levels=c(0,1,2,2.5,3,-3))) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:6],confint.default(mod3,2:6))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = "Menopause") %>% unlist %>% .[(1:5)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x="Menopause",by.y="row",all = T)
  }

  tab_1 %>%
    dplyr::select(Menopause,values=5) %>%
    as_tibble %>% 
    separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
    mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
    mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
    mutate(OR=as.numeric(OR)) %>%
    mutate(OR=ifelse(is.na(OR),1,OR)) %>%
    mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
    mutate(UCI=ifelse(is.na(OR),NULL,UCI)) -> plot_data
  
  pd_width <- 0.6
  
  plot_data %>%
    mutate(Menopause=factor(Menopause,levels=rev(c(0,1,2,2.5,3,-3)),labels=rev(c("Premenopausal","Postmenopausal","Unsure - had a hysterectomy","Unsure - had a hysterectomy\n which involved removal of ovaries","Unsure - other reason","Prefer not to answer")))) %>%
    ggplot(aes(y=OR,x=Menopause)) + 
    geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                  position = position_dodge(width = pd_width)) +
    geom_point(position = position_dodge(width = pd_width)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.position=c(0.1, 0.85),
          legend.background = element_blank())+
    ylab("Adjusted odds ratio of moderate-\nsevere asthma") +
    ylim(c(.9,3)) +
    coord_flip()  -> plot_meno_1
  
ggsave(filename="plots/meno_asthma2.png",plot = plot_meno,height=3,width=5)  

####

model_data %>%
  filter(!is.na(Packyears_nn)) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  mutate(Menopause = case_when(
    Menopause == 0 & Age <=50 ~ 0,
    (Menopause == 1) | (Age > 50 & !(Menopause ==2)) ~ 1,
    (Menopause == 2) & (ovariesremoved==TRUE)  ~ 2.5,
    (Menopause == 2) & (ovariesremoved==FALSE)  & (hysterectomy==TRUE)  ~ 2,
    Menopause==3 & Age<=50 ~ 3,
    Menopause==-3 & Age<=50 ~ -3
  )) -> model_data_temp

model_data_temp %>%
   dplyr::select(all_of("Asthma_def_ms"),Menopause) %>% table(useNA = "always") %>%
  as.data.frame() -> tab_1
eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(Menopause)) -> tab_1")))
tab_1 %>%
  spread(DependentVar,Freq)  -> tab_1

model_list <-list()
for(i in 1:length(model_vec)){
  modeli<-model_vec2[i]
  model_name_i <- model_names[i]
  
  model_data_temp %>%
    mutate(Menopause=factor(Menopause,levels=c(0,1,2,2.5,3,-3))) %>%
    filter(!is.na(DependentVar)) %>%
    glm(data = .,modeli,family = binomial(link="logit")) -> mod3
  
  model_list[[i]] <- mod3
  
  exp(cbind(coef(mod3)[2:6],confint.default(mod3,2:6))) %>%
    as_tibble(rownames="row") %>% 
    round_df(2) %>%
    mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
  eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
  kk_temp %>%
    mutate(row=(strsplit(row,split = "Menopause") %>% unlist %>% .[(1:5)*2])) -> kk_temp
  
  tab_1<-merge(tab_1,kk_temp,by.x="Menopause",by.y="row",all = T)
}

tab_1 %>%
  dplyr::select(Menopause,values=5) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) -> plot_data

pd_width <- 0.6

plot_data %>%
  mutate(Menopause=factor(Menopause,levels=rev(c(0,1,2,2.5,3,-3)),labels=rev(c("Premenopausal","Postmenopausal","Unsure - had a hysterectomy","Unsure - had a hysterectomy\n which involved removal of ovaries","Unsure - other reason","Prefer not to answer")))) %>%
  ggplot(aes(y=OR,x=Menopause)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Adjusted odds ratio of moderate-\nsevere asthma") +
  ylim(c(.1,3)) +
  coord_flip()  -> plot_meno_2

ggsave(filename="plots/meno_asthma_def23.png",plot = plot_meno,height=3,width=5)  

####

model_data %>%
  filter(!is.na(Packyears_nn)) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  mutate(Menopause = case_when(
    Menopause == 0 & Age <=50 &((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))~ 0,
    ((Menopause == 1) | (Age > 50 & !(Menopause ==2)))&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr))) ~ 1,
    (Menopause == 2) & (ovariesremoved==TRUE) &((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr))) ~ 2.5,
    (Menopause == 2) & (ovariesremoved==FALSE)& (hysterectomy==TRUE)  &((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr)))~ 2,
    Menopause==3 & Age<=50&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr))) ~ 3,
    Menopause==-3 & Age<=50&((HRT==0))&((OCPcurr==0)|(is.na(OCPcurr))) ~ -3
  )) -> model_data_temp

model_data_temp %>%
  dplyr::select(all_of("Asthma_def_ms"),Menopause) %>% table(useNA = "always") %>%
  as.data.frame() -> tab_1
eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(Menopause)) -> tab_1")))
tab_1 %>%
  spread(DependentVar,Freq)  -> tab_1

model_list <-list()
for(i in 1:length(model_vec)){
  modeli<-model_vec2[i]
  model_name_i <- model_names[i]
  
  model_data_temp %>%
    mutate(Menopause=factor(Menopause,levels=c(0,1,2,2.5,3,-3))) %>%
    filter(!is.na(DependentVar)) %>%
    glm(data = .,modeli,family = binomial(link="logit")) -> mod3
  
  model_list[[i]] <- mod3
  
  exp(cbind(coef(mod3)[2:6],confint.default(mod3,2:6))) %>%
    as_tibble(rownames="row") %>% 
    round_df(2) %>%
    mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
  eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
  kk_temp %>%
    mutate(row=(strsplit(row,split = "Menopause") %>% unlist %>% .[(1:5)*2])) -> kk_temp
  
  tab_1<-merge(tab_1,kk_temp,by.x="Menopause",by.y="row",all = T)
}

tab_1 %>%
  dplyr::select(Menopause,values=5) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) -> plot_data

pd_width <- 0.6

plot_data %>%
  mutate(Menopause=factor(Menopause,levels=rev(c(0,1,2,2.5,3,-3)),labels=rev(c("Premenopausal","Postmenopausal","Unsure - had a hysterectomy","Unsure - had a hysterectomy\n which involved removal of ovaries","Unsure - other reason","Prefer not to answer")))) %>%
  ggplot(aes(y=OR,x=Menopause)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Adjusted odds ratio of moderate-\nsevere asthma") +
  ylim(c(0,5)) +
  coord_flip()  -> plot_meno_3

ggsave(filename="plots/meno_asthma_def33.png",plot = plot_meno,height=3,width=5)    
  
  
# p <- plot_meno_1+ theme(plot.title.position = "plot") +ggtitle ("A. Self-reported")+
#   plot_meno_2 +ggtitle("B. Self-reported or >50")+ theme(plot.title.position = "plot")+ 
#   plot_meno_3 + ggtitle("C. B with no HRT or Current OCP")+ theme(plot.title.position = "plot")
#   
# ggsave(filename="plots/meno_asthma_together.png",p,width=13,height=4)

p <- plot_meno_2 +ggtitle("A. Self-reported or >50")+ theme(plot.title.position = "plot")+ 
  plot_meno_3 + ggtitle("B. A with no HRT or Current OCP")+ theme(plot.title.position = "plot")

ggsave(filename="plots/meno_asthma_together_n.png",p,width=8,height=3.5)


model_data %>% mutate(ovariesremoved=ovariesremoved_OPCS3|ovariesremoved_OPCS4) %>%
  dplyr::select(Menopause,ovariesremoved) %>% table
  
  
  
  
  
  
  
####

model_data %>%
  mutate(Menopause = case_when(
    Menopause == 0 & Age <=50 & (ovariesremoved==FALSE) ~ 0,
    ((Menopause == 1) | (Age > 50 & !(Menopause ==2)))&(ovariesremoved==FALSE) ~ 1,
    (Menopause == 2) & (ovariesremoved==TRUE) & (Age<=50) ~ 2.5,
    (Menopause == 2) & (ovariesremoved==FALSE) & (Age<=50) ~ 2,
    (Menopause == 2) & (ovariesremoved==TRUE) & (Age>50) ~ 2.55,
    (Menopause == 2) & (ovariesremoved==FALSE) & (Age>50) ~ 2.05,
    (Menopause==3 & Age<=50)&(ovariesremoved==FALSE) ~ 3,
    (Menopause==-3 & Age<=50 )&(ovariesremoved==FALSE)~ -3
  )) -> model_data_temp

model_data_temp %>%
  dplyr::select(all_of("Asthma_def_ms"),Menopause) %>% table(useNA = "always") %>%
  as.data.frame() -> tab_1
eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(Menopause)) -> tab_1")))
tab_1 %>%
  spread(DependentVar,Freq)  -> tab_1

model_list <-list()
for(i in 1:length(model_vec)){
  modeli<-model_vec2[i]
  model_name_i <- model_names[i]
  
  model_data_temp %>%
    mutate(Menopause=factor(Menopause,levels=c(0,1,2,2.05,2.5,2.55,3,-3))) %>%
    filter(!is.na(DependentVar)) %>%
    glm(data = .,modeli,family = binomial(link="logit")) -> mod3
  
  model_list[[i]] <- mod3
  
  exp(cbind(coef(mod3)[2:8],confint.default(mod3,2:8))) %>%
    as_tibble(rownames="row") %>% 
    round_df(2) %>%
    mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
  eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
  kk_temp %>%
    mutate(row=(strsplit(row,split = "Menopause") %>% unlist %>% .[(1:7)*2])) -> kk_temp
  
  tab_1<-merge(tab_1,kk_temp,by.x="Menopause",by.y="row",all = T)
}

tab_1 %>%
  dplyr::select(Menopause,values=5) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) -> plot_data

pd_width <- 0.6

plot_data %>%
  mutate(Menopause=factor(Menopause,levels=rev(c(0,1,2,2.05,2.5,2.55,3,-3)),labels=rev(c("Premenopausal","Postmenopausal","Unsure - had a hysterectomy\n50 or younger","Unsure - had a hysterectomy\nover 50","Unsure - had a hysterectomy\n which involved removal of ovaries\n50 or younger","Unsure - had a hysterectomy\n which involved removal of ovaries\nover 50","Unsure - other reason","Prefer not to answer")))) %>%
  ggplot(aes(y=OR,x=Menopause)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Adjusted odds ratio of moderate-\nsevere asthma") +
  ylim(c(.1,3)) +
  coord_flip()  -> plot_meno
