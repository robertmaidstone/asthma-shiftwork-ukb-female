# common code -------------------------------------------------------------

nCI_code_cols <- paste("X20002.0.",0:32,sep="")
med_code_cols <- paste("X20003.0.",0:47,sep="")
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")
ICD10_date_cols <- paste("X41280.0.",0:212,sep="")

ukb_data_processed %>% 
  as_tibble %>%
  dplyr::select(eid,
                Job_involves_shift_work_int, 
                Job_involves_night_shift_work_int,
                Age_started_prev=X2867.0.0,
                Age_stopped_prev=X2897.0.0,
                Age_started_curr=X3436.0.0,
                Numperday_curr=X3456.0.0,
                Numperday_prev=X2887.0.0,
                Asthma_int,X2316.0.0,Sex,Year_of_birth,Smoking=X20116.0.0,Alcohol=X20117.0.0,
                Ethnicity=X21000.0.0,TDI=X189.0.0,BMI=X21001.0.0,Asthma_age_diagnosed_int,X20154.0.0,
                SleepDur=X1160.0.0,
                DaysWalked=X864.0.0,DaysModerate=X884.0.0,DaysVigorous=X904.0.0,
                all_of(med_code_cols),
                Job_SOC,
                nCI_code,
                Chronotype=X1180.0.0,
                LengthofWW=X767.0.0,
                Alcintake=X1558.0.0,
                ICD10_code,
                all_of(ICD10_code_cols),
                all_of(ICD10_date_cols),
                X22436.2.0,
                ALT=X30620.0.0,
                AST=X30650.0.0,
                platelets=X30080.0.0,
                EntryDate=X53.0.0,
                Glucose=X30740.0.0,
                Triglycerides=X30870.0.0,
                X4418.0.0,X4451.0.0,X4407.0.0,X1578.0.0,X1608.0.0,X1568.0.0,X4429.0.0,X1588.0.0,X4440.0.0,X1598.0.0,X4462.0.0,X5364.0.0,
                NuminHouse=X709.0.0,MatSmoking=X1787.0.0,Breastfed=X1677.0.0,
                Birthweight=X20022.0.0,UrbanRural=X20118.0.0,InitialDate=X53.0.0,Job_AsthmaRisk,Job_MedRequired,
                Testosterone=X30850.0.0,
                SHBG=X30830.0.0,
                Oestradiol=X30800.0.0,
                gen_sex=X22001.0.0,
                Menopause=X2724.0.0,
                HRT=X2814.0.0,
                HRTstart=X3536.0.0,
                HRTend=X3546.0.0,
                OvariesRemoved=X2834.0.0,
                OCP=X2784.0.0,
                OCPstart=X2794.0.0,
                OCPend=X2804.0.0,
                Albumin=X30600.0.0,
                gen_sex=X22001.0.0,
                menarche=X2714.0.0,
                livebirths=X2734.0.0,
                ageatmenopause=X3581.0.0,
                oestradiolmissing=X30805.0.0
                
  ) %>%
  ##
  mutate(Smoking_curr=ifelse(Smoking%in%c(2),ifelse(is.na(Age_started_curr),"occassional","yes"),"no")) %>%
  mutate(Smoking_prev=ifelse(Smoking%in%c(1,2),ifelse(is.na(Age_started_prev),"occassional","yes"),"no")) %>%
  mutate(Smoking_n=paste0(Smoking_curr,Smoking_prev))%>%
  mutate(Smoking_n=plyr::revalue(as.factor(Smoking_n),replace=c("nono"="not a smoker",
                                                                "nooccassional"="not a current smoker, occassionally smoked previously",
                                                                "noyes"="not a current smoker, smoked heavily previously",
                                                                "occassionaloccassional"="current occassional smoker, no history of heavy smoking",
                                                                "occassionalyes"="current occassional smoker, previously a heavy smoker",
                                                                "yesoccassional"="current heavy smoker"))) %>%
  mutate(Smoking_n=ifelse(Smoking==-3,"prefer not to say",as.character(Smoking_n))) %>%
  mutate(Smoking_n=ifelse(is.na(Smoking),NA,Smoking_n)) %>%
  
  mutate(Age_started_prev_n=ifelse(Age_started_prev%in%c(-3,-1),NA,Age_started_prev)) %>%
  mutate(Age_stopped_prev_n=ifelse(Age_stopped_prev%in%c(-3,-1),NA,Age_stopped_prev)) %>%
  mutate(Age_started_curr_n=ifelse(Age_started_curr%in%c(-3,-1),NA,Age_started_curr)) %>%
  mutate(Numperday_curr_n=ifelse(Numperday_curr%in%c(-3,-1),NA,ifelse(Numperday_curr==-10,1,Numperday_curr))) %>% #set as 1 if number smoked is less than 1
  mutate(Numperday_prev_n=ifelse(Numperday_prev%in%c(-3,-1),NA,ifelse(Numperday_prev==-10,1,Numperday_prev))) %>% #set as 1 if number smoked is less than 1
  ##
  mutate(Age=year(as.Date(EntryDate))-year(as.Date(paste0(Year_of_birth,"-01-01")))) %>%
  mutate(OCPcurr=ifelse(OCPend==-11,1,0)) %>%
  mutate(HRTcurr=ifelse(HRTend==-11,1,0)) %>%
  mutate(JiNS=ifelse(Job_involves_shift_work_int=="Never/rarely","No shift work",Job_involves_night_shift_work_int))%>%
  mutate(Packyears_prev=(Age_stopped_prev-Age_started_prev)*Numperday_prev/20) %>%
  mutate(Packyears_curr=(Age-Age_started_curr)*Numperday_curr/20) %>%
  mutate(Packyears=ifelse(is.na(Packyears_prev),Packyears_curr,Packyears_prev)) %>%
  mutate(Ethnicity_o=Ethnicity) %>%
  mutate(Ethnicity=ifelse(Ethnicity%in%c(1,1002,1003),1,
                          ifelse(Ethnicity%in%c(2,2001,2002,2003,2004),2,
                                 ifelse(Ethnicity%in%c(3,3001,3002,3003,3004),3,
                                        ifelse(Ethnicity%in%c(4,4001,4002,4003),4,Ethnicity))))) %>%
  mutate(Hypertension=grepl(",1065,",nCI_code)) %>%
  mutate(RA=grepl(",1464,",nCI_code)) %>%
  mutate(OA=grepl(",1465,",nCI_code)) %>%
  ##
  mutate(Asthma2=grepl(",1111,",nCI_code)) %>%
  unite(col=med_code,med_code_cols,sep=",",remove=TRUE) %>%
  mutate(med_code=paste0(",",med_code,",")) %>%
  mutate(Asthma_med_all=(grepl(patterns_all,med_code))) %>%
  mutate(Asthma_med_ms=(grepl(patterns_ms,med_code))) %>%
  mutate(Asthma_def=Asthma_med_all & Asthma2) %>%
  mutate(Asthma_def_ms=Asthma_med_ms & Asthma2) %>%
  mutate(FEV1lt80=X20154.0.0<80) %>%
  ##
  ##
  mutate(Packyears_prev_n=(Age_stopped_prev_n-Age_started_prev_n)*Numperday_prev_n/20) %>%
  mutate(Packyears_curr_n=(Age-Age_started_curr_n)*Numperday_curr_n/20) %>%
  mutate(Packyears_n=ifelse(is.na(Packyears_prev_n),Packyears_curr_n,Packyears_prev_n)) %>%
  ##
  mutate(HayfeverRhinitisHouseDustMite=grepl(",1387,|,1668,",nCI_code)) %>%
  mutate(Eczema=grepl(",1452,",nCI_code)) %>%
  mutate(Allergy=grepl(",1385,|,1386,",nCI_code)) %>%
  mutate(AnxietyPanic=grepl(",1287,",nCI_code)) %>%
  mutate(RespInfection=grepl(",1594,|,1398,|,1411,|,1498,",nCI_code)) %>%
  ##
  mutate(High_Cholesterol=grepl(",1473,",nCI_code)) %>%
  mutate(Sleep_Apnoea=grepl(",1123",nCI_code)) %>%
  mutate(COPD=grepl(",1112",nCI_code)) %>%
  mutate(Emp_Chron_Bronchitis=grepl(",1113",nCI_code)) %>%
  mutate(Bronchiectasis=grepl(",1114",nCI_code)) %>%
  mutate(Inter_lung_disease=grepl(",1115",nCI_code)) %>%
  mutate(Other_resp_probs=grepl(",1117",nCI_code)) %>%
  mutate(GastroOesReflux=grepl(",1138",nCI_code)) %>%
  mutate(Asthma2=grepl(",1111,",nCI_code)) %>%
  mutate(GestDiabetes=grepl(",1221,",nCI_code)) %>%
  mutate(DiabetesI=grepl(",1222,",nCI_code)) %>%
  mutate(DiabetesII=grepl(",1223,",nCI_code)) %>%
  mutate(DiabetesInsipidus=grepl(",1521,",nCI_code)) %>%
  mutate(AllDiabetes=grepl(",1220,|,1221,|,1222,|,1223,|,1521,",nCI_code)) %>%
  mutate(EssentialHypertension=grepl(",1072,",nCI_code)) %>%
  mutate(GestationalHypertension=grepl(",1073,",nCI_code)) %>%
  mutate(AllHypertension=grepl(",1065,|,1072,|,1073,",nCI_code)) %>%
  mutate(PNDepression=grepl(",1531,",nCI_code)) %>%
  mutate(AllDepression=grepl(",1286,|,1531,",nCI_code)) %>%
  mutate(CardiovascularDisease=grepl(",1074,|,1075,|,1077,|,1471,|,1483,|,1484,|,1485,|,1486,|,1487,|,1080,|,1589,|,1590,|,1078,|,1584,|,1586,|,1079,|,1588,",nCI_code)) %>%
  mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Sometimes","Usually","Always"))) %>%
  mutate(Chronotype_o=Chronotype) %>%
  mutate(Chronotype=factor(Chronotype))%>%
  mutate(Chronotype=plyr::revalue(Chronotype,c("-1"="Do not know","-3"="Prefer not to answer","4"="Definitely an evening person","3"="More an evening than a morning person","2"="More a morning than an evening person","1"="Definitely a morning person"))) %>%
  mutate(Smoking=factor(Smoking,levels=c(0,-3,1,2))) %>%
  mutate(Alcohol=factor(Alcohol,levels=c(0,-3,1,2))) %>% 
  mutate(Ethnicity_o=factor(Ethnicity_o,levels=c(1,sort(unique(Ethnicity_o))[-3]))) %>%
  mutate(Ethnicity=factor(Ethnicity,levels=c(1,sort(unique(Ethnicity))[-3]))) %>%
  mutate(JiNS_o=plyr::revalue(JiNS, c("Always"="Always","Usually"="Irregular shift work","Sometimes"="Irregular shift work","Never/rarely"="Never/rarely"))) %>%
  mutate(JiNS=plyr::revalue(JiNS, c("Always"="Always","Usually"="Irregular shift work","Sometimes"="Irregular shift work","Never/rarely"="Irregular shift work"))) %>%
  mutate(SleepDur_o = SleepDur) %>%
  mutate(SleepDur = ifelse(SleepDur < 6, "< 6",ifelse(SleepDur > 8, "> 8","6<=x<=8"))) %>%
  mutate(SleepDur=factor(SleepDur,levels=c("6<=x<=8","< 6","> 8")))%>%
  mutate(BMI_o=BMI) %>%
  mutate(BMI=as.factor(ifelse(BMI<18.5,1,ifelse(BMI<25,0,ifelse(BMI<30,2,3)))))  -> model_data


model_data %>% 
  mutate(Chronotype = ifelse(Chronotype%in% c("More a morning than an evening person",
                                              "More an evening than a morning person"),
                             "Intermediate chronotype",
                             as.character(Chronotype))) %>%
  mutate(Chronotype=factor(Chronotype,levels=c("Intermediate chronotype",
                                               "Definitely a morning person",
                                               "Definitely an evening person",
                                               "Do not know",
                                               "Prefer not to answer"))) %>%
  mutate(Alc_daily=Alcintake==1) %>%
  mutate(Glucose_mgdl=Glucose*18) %>%
  mutate(Triglycerides_mgdl=Triglycerides*18) %>%
  mutate(Alcmissing_m = (X4418.0.0<0)|(X4451.0.0<0)|(X4407.0.0<0)|(X4429.0.0<0)|(X4440.0.0<0)|(X4462.0.0<0)) %>%
  mutate(Alcmissing_w = (X1578.0.0<0)|(X1608.0.0<0)|(X1568.0.0<0)|(X1588.0.0<0)|(X1598.0.0<0)|(X5364.0.0<0)) %>%
  mutate(Wineglasses_m=X4418.0.0 + X4451.0.0 + X4407.0.0) %>%
  mutate(Wineglasses_w=X1578.0.0 + X1608.0.0 + X1568.0.0) %>%
  mutate(Beer_m=X4429.0.0) %>%
  mutate(Beer_w=X1588.0.0) %>%
  mutate(Spirits_m=X4440.0.0) %>%
  mutate(Spirits_w=X1598.0.0) %>%
  mutate(Otheralc_m=X4462.0.0) %>%
  mutate(Otheralc_w=X5364.0.0) %>%
  mutate(Units_m=ifelse(Alcmissing_m==TRUE,NA,1.5*Wineglasses_m + 2.8*Beer_m + 1*Spirits_m + 1.5 * Otheralc_m)) %>%
  mutate(Units_w=ifelse(Alcmissing_w==TRUE,NA,1.5*Wineglasses_w + 2.8*Beer_w + 1*Spirits_w + 1.5 * Otheralc_w)) %>%
  mutate(Monthly_units=ifelse(Alcintake%in%c(1,2,3),Units_w*4.345,ifelse(Alcintake%in%c(4,5),Units_m,ifelse(Alcintake==6,0,NA)))) %>%
  mutate(Packyears_nn=Packyears_n) %>%
  mutate(Packyears_n=ifelse(Smoking==0,0,Packyears_n)) %>% ##
  mutate(Packyears_nn=ifelse(Smoking_n%in%c("not a smoker",
                                            "not a current smoker, occassionally smoked previously",
                                            "current occassional smoker, no history of heavy smoking"),0,Packyears_n)) %>% ##
  
  mutate(Current_worker=!is.na(LengthofWW)) %>%
  mutate(LengthofWW_oo=LengthofWW) %>%
  mutate(LengthofWW_o=ifelse(is.na(LengthofWW),0,LengthofWW)) %>%
  mutate(LengthofWW=ifelse(Current_worker==FALSE,0,ifelse(LengthofWW<0,NA,LengthofWW))) %>%
  mutate(Sex=gen_sex)-> model_data
