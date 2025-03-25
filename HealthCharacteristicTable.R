library(lubridate)
library(flextable)
library(officer)
library(MASS)
library(lmtest)
library(nnet)

source("load_data.R")

rd <- 2 # digits to round to
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

source("ORdatawrangling.R")

model_data  %>%
  filter(!is.na(JiNS)) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(Year_of_birth)) %>%
  filter(!is.na(Ethnicity)) %>%
  filter(!is.na(TDI)) %>%
  filter(!is.na(SleepDur)) %>%
  #filter(!is.na(Smoking_n)) %>%
  #filter(!is.na(Packyears_nn)) %>%
  filter(!is.na(Alc_daily)) %>%
  filter(!is.na(LengthofWW)) -> model_data_temp

dim(model_data_temp)[1]

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

model_data_temp %>% mutate(FT=cbat_v(Testosterone,SHBG)) %>% mutate(FAI=Testosterone/SHBG*100) -> model_data_temp

(model_data_temp %>% dplyr::select(Sex,BMI=BMI_o, 'Birth weight'=Birthweight,'High cholesterol'=High_Cholesterol,
                                   'Sleep Apnoea'= Sleep_Apnoea, COPD, 'Emphysema/Chronic Bronchitis'=Emp_Chron_Bronchitis,
                                   Bronchiectasis, 'Interstitial Lung Disease'=Inter_lung_disease,
                                   'Other Respiratory Problems'=Other_resp_probs,'Gastro-Oesophageal Reflux'=GastroOesReflux, 
                                   'Type 1 Diabetes'=DiabetesI,'Type 2 Diabetes'=DiabetesII,
                                   'Hypertension'=AllHypertension,Depression=AllDepression,'Cardiovascular Disease'=CardiovascularDisease,
                                   #'Eosinophil Count'=Eosinophil,
                                   'Hayfever/Allergic Rhinitis/Allergy to house dust mite'=HayfeverRhinitisHouseDustMite,
                                   Eczema, 'Allergy or Anaphylactic reaction to food/drug'=Allergy,
                                   'Anxiety/panic attacks'=AnxietyPanic) %>%
  apply(2,function(x){sum(is.na(x))})) -> na_values

na_values/dim(model_data_temp)[1] * 100 -> na_values_pct

tibble(Names=names(na_values),'Missing Values'=as.numeric(na_values),'Percentage of total data' = round(as.numeric(na_values_pct),2)) %>%
  dplyr::filter(`Missing Values` != 0) -> tab_supp

H_table_function <- function(model_data_temp){
model_data_temp %>% 
  filter(!(JiNS_o %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  group_by(JiNS_o) %>%
  dplyr::summarise(N=n(),
                   `BMI (kg/m^2)` = paste0(round(mean(BMI_o,na.rm=T),rd)," (",round(sd(BMI_o,na.rm=T),rd),")"),
                   `Birth Weight (kg)*` = paste0(round(mean(Birthweight,na.rm=T),rd)," (",round(sd(Birthweight,na.rm=T),rd),")"),
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
                   `Cardiovascular Disease (%)`=round(sum(CardiovascularDisease)/n()*100,rd),
                   `Depression (%)`=round(sum(AllDepression)/n()*100,rd),
                   `Anxiety/panic attacks (%)`=round(sum(AnxietyPanic)/n()*100,rd),
                   `Hayfever/Allergic Rhinitis/Allergy to house dust mite (%)`=round(sum(HayfeverRhinitisHouseDustMite)/n()*100,rd),
                   `Eczema/Dermatitis (%)`=round(sum(Eczema)/n()*100,rd),
                   `Allergy or Anaphylactic reaction to food/drug (%)`=round(sum(Allergy)/n()*100,rd),
                   `Eosinophil Count`=NA,
                   `Testosterone (nmol/L)` =  paste0(round(mean(Testosterone,na.rm=T),rd)," (",round(sd(Testosterone,na.rm=T),rd),")"),
                   `Oestradiol (pmol/L)` =  paste0(round(mean(Oestradiol,na.rm=T),rd)," (",round(sd(Oestradiol,na.rm=T),rd),")"),
                   `SHBG (nmol/L)` =  paste0(round(mean(SHBG,na.rm=T),rd)," (",round(sd(SHBG,na.rm=T),rd),")"),
                   `Free Androgen Index` =  paste0(round(mean(FAI,na.rm=T),rd)," (",round(sd(FAI,na.rm=T),rd),")"),
                   `Free Testosterone (nmol/L)` =  paste0(round(mean(FT,na.rm=T),rd)," (",round(sd(FT,na.rm=T),rd),")")
  ) -> tab_Hchar
  return(tab_Hchar)
}

H_table_function(model_data_temp %>% filter(Sex==0)) -> tab_H_women
H_table_function(model_data_temp %>% filter(Sex==1)) -> tab_H_men

rbind(tab_H_women %>% 
        gather(Key,Value,-JiNS) %>% 
        mutate(Key=factor(Key,levels=names(tab_H_women))) %>% 
        mutate(Sex="F"),
      tab_H_men %>%
        gather(Key,Value,-JiNS) %>%
        mutate(Key=factor(Key,levels=names(tab_H_men))) %>% 
        mutate(Sex="M")) %>%
  as_tibble %>%
  spread(JiNS,Value) %>%
  dplyr::select(Key,Sex,`No shift work`,`Irregular shift work`,Always) %>%
  filter(Key!="Sex (% male)")-> out_tab

out_tab[1:2,-(1:2)]  %>% unlist %>% as.numeric() %>% sum() -> N_H


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

  model_data_temp %>% 
  filter(!(JiNS %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  group_by(JiNS) %>%
  mutate(Alc_daily=Alcintake==1) %>%
  mutate(Chronotype=factor(Chronotype,levels=c("Definitely a morning person","Intermediate chronotype","Definitely an evening person"),ordered=T)) %>%
  mutate(Ethnicity=relevel(Ethnicity,ref=9))-> test_data

cbind(
  c("BMI (kg/m^2)","Birth Weight (kg)*","Testosterone (nmol/L)","Oestradiol (pmol/L)","SHBG (nmol/L)","Free Androgen Index","Free Testosterone (nmol/L)")%>% rep(each=2),
  c(aov(BMI_o ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(BMI_o ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(Birthweight ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(Birthweight ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(Testosterone ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(Testosterone ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(Oestradiol ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(Oestradiol ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(SHBG ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(SHBG ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(FAI ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(FAI ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19],
    aov(FT ~ JiNS+as.character(Sex), data = test_data) %>% summary %>%unlist %>% .[14],
    aov(FT ~ JiNS*as.character(Sex), data = test_data) %>% summary %>% unlist %>% .[19]
    )) %>% as.data.frame()%>% mutate(V2=round(as.numeric(V2),3)) -> shift_aov

cbind(
  c("High Cholesterol (%)","Sleep Apnoea (%)","Chronic Obstructive Airways Disease/COPD","Emphysema/Chronic Bronchitis",
    "Bronchiectasis (%)","Interstitial Lung Disease (%)","Other Respiratory Problems (%)","Gastro-Oesophageal Reflux (%)",
    "Type 1 Diabetes (%)","Type 2 Diabetes (%)","Hypertension (%)","Cardiovascular Disease (%)","Depression (%)",
    "Anxiety/panic attacks (%)","Hayfever/Allergic Rhinitis/Allergy to house dust mite (%)","Eczema/Dermatitis (%)",
    "Allergy or Anaphylactic reaction to food/drug (%)") %>% rep(each=2),
  c(
    glm(data = test_data, High_Cholesterol ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, High_Cholesterol ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, High_Cholesterol ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Sleep_Apnoea ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Sleep_Apnoea ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Sleep_Apnoea ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, COPD ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, COPD ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, COPD ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Emp_Chron_Bronchitis ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Emp_Chron_Bronchitis ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Emp_Chron_Bronchitis ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Bronchiectasis ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Bronchiectasis ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Bronchiectasis ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Inter_lung_disease ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Inter_lung_disease ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Inter_lung_disease ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Other_resp_probs ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Other_resp_probs ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Other_resp_probs ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, GastroOesReflux ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, GastroOesReflux ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, GastroOesReflux ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, DiabetesI ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, DiabetesI ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, DiabetesI ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, DiabetesII ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, DiabetesII ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, DiabetesII ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, AllHypertension ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, AllHypertension ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, AllHypertension ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, CardiovascularDisease ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, CardiovascularDisease ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, CardiovascularDisease ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, AllDepression ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, AllDepression ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, AllDepression ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, AnxietyPanic ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, AnxietyPanic ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, AnxietyPanic ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, HayfeverRhinitisHouseDustMite ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, HayfeverRhinitisHouseDustMite ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, HayfeverRhinitisHouseDustMite ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Eczema ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Eczema ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Eczema ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2],
    glm(data = test_data, Allergy ~ JiNS+as.character(Sex),family = binomial(link="logit")) %>%
      summary %>%coefficients() %>% .[4,4],
    lrtest(glm(data = test_data, Allergy ~ JiNS+as.character(Sex),family = binomial(link="logit")),
           glm(data = test_data, Allergy ~ JiNS*as.character(Sex),family = binomial(link="logit")))$`Pr(>Chisq)`[2]
    )) %>% as.data.frame() %>% mutate(V2=round(as.numeric(V2),3))-> shift_lr



out_tab$Key

shift_aov %>% mutate(test=rep(c("sex","sexshiftinteraction"),7)) -> shift_aov
shift_lr %>% mutate(test=rep(c("sex","sexshiftinteraction"),17)) -> shift_lr
 
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
  width(6,1) -> table_H


read_docx() %>% 
  body_add_par(paste0("Table 1: Participant characteristics by current night shift work exposure (N = ",formatC(N_H,format="d",big.mark=","),")")) %>%
  body_add_flextable(table_H) %>%
  body_add_par("Data are mean (SD) or percentages. * indicates variable with >20% missing data from participants (see Supplemental Table 1 for full breakdowns).") %>%
  print(target = "output/HealthCharacteristicTable.docx")
