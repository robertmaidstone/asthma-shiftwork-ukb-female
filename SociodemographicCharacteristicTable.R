library(lubridate)
library(flextable)
library(officer)
library(MASS)
library(lmtest)
library(nnet)

source("load_data.R")

rd <- 2 # digits to round to
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

source("data_wrangling.R")

model_data  %>%
  filter(!is.na(JiNS)) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(Year_of_birth)) %>%
  filter(!is.na(Ethnicity)) %>%
  filter(!is.na(TDI)) %>%
  filter(!is.na(SleepDur)) %>%
  #filter(!is.na(Smoking)) %>%
  #filter(!is.na(Packyears)) %>%
  filter(!is.na(Alc_daily)) %>%
  mutate(Packyears=ifelse(Smoking==0,0,Packyears)) %>%
  filter(!is.na(LengthofWW)) -> model_data_temp

dim(model_data_temp)[1]

(model_data_temp %>% dplyr::select(Age,Sex,BMI=BMI_o, 'Smoking Status'=Smoking, 'Pack-years'=Packyears, 'Drink alcohol daily'=Alcintake,
                                   'Monthly units of alcohol consumed'=Monthly_units, 'Sleep Duration'=SleepDur, Chronotype,Ethnicity,
                                   'Weekly work hours'=LengthofWW, 'Single Occupancy'=NuminHouse,
                                   'Urban area'=UrbanRural,TDI,'Maternal Smoking'=MatSmoking,
                                   'Breastfed as baby'=Breastfed, 'Birth weight'=Birthweight,
                                   'High cholesterol'=High_Cholesterol,'Type I Diabetes'=DiabetesI,'Type II Diabetes'=DiabetesII,
                                   'Hypertension'=AllHypertension,Depression=AllDepression,'Cardiovascular Disease'=CardiovascularDisease) %>%
  apply(2,function(x){sum(is.na(x))})) -> na_values

na_values/dim(model_data_temp)[1] * 100 -> na_values_pct

tibble(Names=names(na_values),'Missing Values'=as.numeric(na_values),'Percentage of total data' = round(as.numeric(na_values_pct),2)) %>%
  dplyr::filter(`Missing Values` != 0) -> tab_supp

SD_table_function <- function(model_data_temp){
model_data_temp %>% 
  filter(!(JiNS_o %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  group_by(JiNS_o) %>%
  dplyr::summarise(N=n(),
                   `Age (years)`=paste0(round(mean(Age),rd)," (",round(sd(Age),rd),")"), 
                   `Sex (% male)`=round(sum(Sex)/n()*100,rd),
                   `Never`=round(sum(Smoking==0,na.rm=T)/sum(!is.na(Smoking))*100,rd),
                   `Previous`=round(sum(Smoking==1,na.rm=T)/sum(!is.na(Smoking))*100,rd),
                   `Current`=round(sum(Smoking==2,na.rm=T)/sum(!is.na(Smoking))*100,rd),
                   `Smoking pack-years`=paste0(round(mean(Packyears,na.rm=T),rd)," (",round(sd(Packyears,na.rm=T),rd),")"),
                   `Drink alcohol daily (%)`=round(sum(Alcintake==1,na.rm=T)/sum(!is.na(Alcintake))*100,rd),
                   `Monthly units of alcohol consumed *`=paste0(round(mean(Monthly_units,na.rm=T),rd)," (",round(sd(Monthly_units,na.rm=T),rd),")"),
                   `Sleep Duration (h)`=paste0(round(mean(SleepDur_o,na.rm=T),rd)," (",round(sd(SleepDur_o,na.rm=T),rd),")"),
                   ##
                   `Morning`=round(sum(Chronotype=="Definitely a morning person",na.rm=T)/sum(!is.na(Chronotype))*100,rd),
                   `Intermediate`=round(sum(Chronotype=="Intermediate chronotype",na.rm=T)/sum(!is.na(Chronotype))*100,rd),
                   `Evening`=round(sum(Chronotype=="Definitely an evening person",na.rm=T)/sum(!is.na(Chronotype))*100,rd),
                   `Do Not Know`=round(sum(Chronotype=="Do not know",na.rm=T)/sum(!is.na(Chronotype))*100,rd), ######
                   ##
                   `White British`=round(sum(Ethnicity%in%c(1001),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `White Other`=round(sum(Ethnicity%in%c(1,1002,1003),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Mixed`=round(sum(Ethnicity%in%c(2,2001,2002,2003,2004),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Asian`=round(sum(Ethnicity%in%c(3,3001,3002,3003,3004),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Black`=round(sum(Ethnicity%in%c(4,4001,4002,4003),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Chinese`=round(sum(Ethnicity%in%c(5),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Other`=round(sum(Ethnicity%in%c(1),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   ##
                   `Weekly work hours`=paste0(round(mean(LengthofWW,na.rm=T),rd)," (",round(sd(LengthofWW,na.rm=T),rd),")"),
                   ##
                   `Single Occupancy (%)`=round(sum(NuminHouse==1,na.rm=T)/sum(!is.na(NuminHouse))*100,rd),
                   `Urban area (%)`=round(sum(UrbanRural%in%c(1,5,11,12),na.rm=T)/sum(!is.na(UrbanRural))*100,rd),
                   `Townsend Index`=paste0(round(median(TDI,na.rm=T),2)," (",paste0(round(quantile(TDI,c(0.25,0.75),na.rm=T),2),collapse = " to "),")"),
  ) -> tab_SDchar
return(tab_SDchar)
}

H_table_function <- function(model_data_temp){
model_data_temp %>% 
  filter(!(JiNS_o %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  group_by(JiNS_o) %>%
  dplyr::summarise(N=n(),
                   `BMI (kg/m^2)` = paste0(round(mean(BMI_o,na.rm=T),rd)," (",round(sd(BMI_o,na.rm=T),rd),")"),
                   `High Cholesterol (%)`=round(sum(High_Cholesterol)/n()*100,rd),
                   `Sleep Apnoea (%)`=round(sum(Sleep_Apnoea)/n()*100,rd),
                   `Chronic Obstructive Airways Disease/COPD`=round(sum(COPD)/n()*100,rd),
                   `Emphysema/Chronic Bronchitis`=round(sum(Emp_Chron_Bronchitis)/n()*100,rd),
                   `Bronchiectasis (%)`=round(sum(Bronchiectasis)/n()*100,rd),
                   `Interstitial Lung Disease (%)`=round(sum(Inter_lung_disease)/n()*100,rd),
                   `Other Respiratory Problems (%)`=round(sum(Other_resp_probs)/n()*100,rd),
                   `Gastro-Oesophageal Reflux (%)`=round(sum(GastroOesReflux)/n()*100,rd),
                   `Type 1 Diabetes (%)`=round(sum(DiabetesI)/n()*100,rd),
                   `Type 2 Diabetes (%)`=round(sum(DiabetesII)/n()*100,rd),
                   `Hypertension (%)`=round(sum(AllHypertension)/n()*100,rd),
                   `Depression (%)`=round(sum(AllDepression)/n()*100,rd),
                   `Cardiovascular Disease (%)`=round(sum(CardiovascularDisease)/n()*100,rd),
                   `Eosinophil Count`=NA,
  ) -> tab_Hchar
  return(tab_Hchar)
}

SD_table_function(model_data_temp %>% filter(Sex==0)) -> tab_SD_women
SD_table_function(model_data_temp %>% filter(Sex==1)) -> tab_SD_men

rbind(tab_SD_women %>% 
        gather(Key,Value,-JiNS) %>% 
        mutate(Key=factor(Key,levels=names(tab_SD_women))) %>% 
        mutate(Sex="F"),
      tab_SD_men %>%
        gather(Key,Value,-JiNS) %>%
        mutate(Key=factor(Key,levels=names(tab_SD_men))) %>% 
        mutate(Sex="M")) %>%
  as_tibble %>%
  spread(JiNS,Value) %>%
  dplyr::select(Key,Sex,`No shift work`,`Irregular shift work`,Always) %>%
  filter(Key!="Sex (% male)")-> out_tab

out_tab[1:2,-(1:2)]  %>% unlist %>% as.numeric() %>% sum() -> N_SD

out_tab %>% mutate(Key=as.character(Key)) %>%
  add_row(Key="Smoker (%)", .before = which(out_tab$Key=="Never")) %>%
  add_row(Key="Smoker (%)", .before = which(out_tab$Key=="Never"))-> out_tab 
out_tab %>%
  add_row(Key="Chronotype (%)", .before = which(out_tab$Key=="Morning")) %>%
  add_row(Key="Chronotype (%)", .before = which(out_tab$Key=="Morning")) -> out_tab
out_tab %>%
  add_row(Key="Ethnicity (%)", .before = which(out_tab$Key=="White British")) %>%
  add_row(Key="Ethnicity (%)", .before = which(out_tab$Key=="White British"))-> out_tab


tab_keys<-c("Never","Previous","Current",
            "Morning","Intermediate","Evening",
            "White British","White Other","Mixed","Asian","Black","Chinese","Other"
            ) %>% rep(each=2)
out_tab$Key[out_tab$Key%in%tab_keys] <- paste0("\t",tab_keys)

flextable(data = out_tab) %>% 
  theme_booktabs() %>% 
  add_header_row(values=c("","Sex","Current work schedule","Current work schedule","Current work schedule"),top=TRUE) %>%
  merge_at(i = 1, j = 3:5, part = "header") %>%
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 1:2, j = 2, part = "header") -> temp
  
for(ii in 1:(dim(out_tab)[1]/2)){
temp %>% merge_at(i = (ii*2-1):(ii*2), j = 1, part = "body") -> temp
}
  temp %>%
  theme_booktabs() %>% 
  set_header_labels(Key = "", 
                    `No shift work` = "Day workers",
                    `Irregular shift work` = "Irregular shift work",
                    Always = "Permanent night\nshift work") %>%
  align(j = 1:2, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 3:5, i = 1, align = "center", part = "head") %>%
  align(j = 3:5, i = 2, align = "center", part = "head") %>%
  align(j = 2, i = 1, align = "left", part = "head") %>%
  align(j = 3:5, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  bg(i = c((1:(dim(out_tab)[1]/4))*4-2,(1:(dim(out_tab)[1]/4))*4-3), bg = "gray90") %>% 
  fontsize(size=8,part = "all") %>%
  border(i = 2, j = 1, border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
  border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
  bold(j = 1, i = ~ is.na(Always), bold = TRUE, part = "body" ) %>%
  color(i = (1:(dim(out_tab)[1]/2))*2, j = 2:5, "red", part = "body") %>%
  rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
  autofit() %>% 
  width(3:5,rep(1.5,3)) %>% 
  height_all(.1) %>% 
  width(1,1.5) %>%
  width(2,0.5)



######## stats tests ######

model_data %>% 
    filter(!is.na(JiNS)) %>%
    filter(!is.na(Sex)) %>%
    filter(!is.na(Year_of_birth)) %>%
    filter(!is.na(Ethnicity)) %>%
    filter(!is.na(TDI)) %>%
    filter(!is.na(SleepDur)) %>%
    # filter(!is.na(Smoking)) %>%
    # filter(!is.na(Packyears)) %>%
    filter(!is.na(Alc_daily)) %>%
    filter(!is.na(LengthofWW))  %>%
    mutate(Packyears=ifelse(Smoking==0,0,Packyears)) %>%
  filter(!(JiNS %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  group_by(JiNS) %>%
  mutate(Alc_daily=Alcintake==1) %>%
  mutate(Chronotype=factor(Chronotype,levels=c("Definitely a morning person","Intermediate chronotype","Definitely an evening person"),ordered=T)) %>%
  mutate(Ethnicity=relevel(Ethnicity,ref=9))-> test_data

cbind(
  c("Age (years)","Townsend Index","Smoking pack-years","Sleep Duration (h)","Weekly work hours","Monthly units of alcohol consumed *")%>% rep(each=2),
  c(aov(Age ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(Age ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(TDI ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(TDI ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(Packyears ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(Packyears ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(SleepDur_o ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(SleepDur_o ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(LengthofWW ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(LengthofWW ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(Monthly_units ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(Monthly_units ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19]
    )) %>% as.data.frame()%>% mutate(V2=round(as.numeric(V2),3)) -> shift_aov

cbind(
  c("Ethnicity (%)","Smoker (%)","Single Occupancy (%)","Urban area (%)","Chronotype (%)","Drink alcohol daily (%)")%>% rep(each=2),
  c(
  #  glm(data = test_data,Ethnicity ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
  #     summary %>%coefficients() %>% .[4,4],
  #   lrtest(glm(data = test_data,Ethnicity ~ JiNS+as.character(Sex),family = binomial(link="logit")),
  #          glm(data = test_data,Ethnicity ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    multinom(data = test_data,formula=Ethnicity ~ JiNS+as.character(Sex)) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(multinom(data = test_data,formula=Ethnicity ~ JiNS+as.character(Sex)),
           multinom(data = test_data,formula=Ethnicity ~ JiNS*as.character(Sex)))$`Pr(>Chisq)`[2],
    #  glm(data = test_data,Smoking ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
    #   summary %>%coefficients() %>% .[4,4],
    # lrtest(glm(data = test_data,Smoking ~ JiNS+as.character(Sex),family = binomial(link="logit")),
    #        glm(data = test_data,Smoking ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    multinom(data = test_data %>% filter(Smoking!=-3),Smoking ~ JiNS+as.character(Sex)) %>%
      summary %>%coefficients() %>% .[2,4],
    lrtest(multinom(data = test_data %>% filter(Smoking!=-3),Smoking ~ JiNS+as.character(Sex)),
           multinom(data = test_data %>% filter(Smoking!=-3),Smoking ~ JiNS*as.character(Sex)))$`Pr(>Chisq)`[2],
    glm(data = test_data%>% dplyr::mutate(SingleOc=NuminHouse==1),SingleOc ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data%>% dplyr::mutate(SingleOc=NuminHouse==1),SingleOc ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data%>% dplyr::mutate(SingleOc=NuminHouse==1),SingleOc ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data%>% dplyr::mutate(Urban=UrbanRural%in%c(1,5,11,12)),Urban ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data%>% dplyr::mutate(Urban=UrbanRural%in%c(1,5,11,12)),Urban ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data%>% dplyr::mutate(Urban=UrbanRural%in%c(1,5,11,12)),Urban ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    # glm(data = test_data,Chronotype ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
    #   summary %>%coefficients() %>% .[4,4],
    # lrtest(glm(data = test_data,Chronotype ~ JiNS+as.character(Sex),family = binomial(link="logit")),
    #        glm(data = test_data,Chronotype ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    polr(data = test_data %>% filter(Chronotype%in%c("Intermediate chronotype","Definitely a morning person","Definitely an evening person")),Chronotype ~ JiNS+as.character(Sex)) %>%
      summary %>%coefficients() %>% .[3,3] %>% pt(dim(test_data)[1]-2,lower.tail = F)*2,
    lrtest(polr(data = test_data%>% filter(Chronotype%in%c("Intermediate chronotype","Definitely a morning person","Definitely an evening person")),Chronotype ~ JiNS+as.character(Sex)),
           polr(data = test_data%>% filter(Chronotype%in%c("Intermediate chronotype","Definitely a morning person","Definitely an evening person")),Chronotype ~ JiNS*as.character(Sex)))$`Pr(>Chisq)`[2],
    glm(data = test_data,Alc_daily ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data,Alc_daily ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data,Alc_daily ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2]
    )) %>% as.data.frame() %>% mutate(V2=round(as.numeric(V2),3))-> shift_lr



out_tab$Key

shift_aov %>% mutate(test=rep(c("sex","sexshiftinteraction"),6)) -> shift_aov
shift_lr %>% mutate(test=rep(c("sex","sexshiftinteraction"),6)) -> shift_lr
 
left_join(out_tab,shift_aov%>% filter(test=="sexshiftinteraction"),by=c("Key"="V1")) %>% left_join(shift_lr%>% filter(test=="sexshiftinteraction"),by=c("Key"="V1")) %>% 
  mutate(Significance=ifelse(!is.na(V2.x),V2.x,ifelse(!is.na(V2.y),V2.y,NA))) %>%
  mutate(Significance=ifelse(is.na(Significance),"",ifelse(Significance<0.01,"<0.01",Significance))) %>%
  dplyr::select(-V2.x,-V2.y,-test.x,-test.y) -> out_tab_new


flextable(data = out_tab_new) %>% 
  theme_booktabs() %>% 
  add_header_row(values=c("","Sex","Current work schedule","Current work schedule","Current work schedule","Sex/work schedule interaction"),top=TRUE) %>%
  merge_at(i = 1, j = 3:5, part = "header") %>%
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 1:2, j = 2, part = "header") %>%
  merge_at(i = 1:2, j = 6, part = "header")-> temp

for(ii in 1:(dim(out_tab_new)[1]/2)){
  temp %>% merge_at(i = (ii*2-1):(ii*2), j = 1, part = "body") %>%
    merge_at(i = (ii*2-1):(ii*2), j = 6, part = "body") -> temp
}
temp %>%
  theme_booktabs() %>% 
  set_header_labels(Key = "", 
                    `No shift work` = "Day workers",
                    `Irregular shift work` = "Irregular shift work",
                    Always = "Permanent night\nshift work") %>%
  align(j = 1:2, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 3:6, i = 1, align = "center", part = "head") %>%
  align(j = 3:6, i = 2, align = "center", part = "head") %>%
  align(j = 2, i = 1, align = "left", part = "head") %>%
  align(j = 3:6, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  bg(i = c((1:(dim(out_tab)[1]/4))*4-2,(1:(dim(out_tab)[1]/4))*4-3), bg = "gray90") %>% 
  fontsize(size=8,part = "all") %>%
  border(i = 2, j = 1, border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
  border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
  bold(j = 1, i = ~ is.na(Always), bold = TRUE, part = "body" ) %>%
  color(i = (1:(dim(out_tab)[1]/2))*2, j = 2:5, "red", part = "body") %>%
  rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
  autofit() %>% 
  width(3:5,rep(1.5,3)) %>% 
  height_all(.5) %>% 
  width(1,1.5) %>%
  width(2,.5) %>%
  width(6,1) -> table_SD


read_docx() %>% 
  body_add_par(paste0("Table 1: Participant characteristics by current night shift work exposure (N = ",formatC(N_SD,format="d",big.mark=","),")")) %>%
  body_add_flextable(table_SD) %>%
  body_add_par("Data are mean (SD) or percentages. * indicates variable with >20% missing data from participants (see Supplemental Table 1 for full breakdowns).") %>%
  print(target = "output/SociodemographicCharacteristicTable.docx")
