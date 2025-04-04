source("load_data.R")

# ORmodelrun function -----------------------------------------------------

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

tf<-function(table_input){
  table_input %>% mutate(samplesize=`FALSE`+`TRUE`,trueprop=round(`TRUE`/samplesize*100,2)) %>% 
    dplyr::select(sum.MNSorNS.group,`TRUE`,trueprop,samplesize,everything()) %>% dplyr::select(-`FALSE`) %>% t
}

ORmodelrun.sumNSW<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(DependentVar,sum.MNSorNS.group) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(sum.MNSorNS.group)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq)  -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(sum.MNSorNS.group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:4],confint.default(mod3,2:4))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = "sum.MNSorNS.group") %>% unlist %>% .[(1:3)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x="sum.MNSorNS.group",by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}

trend.sumNSW<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  str_replace(model_vec,"sum.MNSorNS.group","num_yiMNSorNSj") -> model_vec
  
  model_data %>%
    dplyr::select(DependentVar,sum.MNSorNS.group) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(sum.MNSorNS.group)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq)  -> tab_1
  
  model_list <-list()
  ptrend<-c()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(sum.MNSorNS.group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    anova(mod3,test="Chisq")[2,5]->ptrend[i]
  }
  return(ptrend)
}

# common code -------------------------------------------------------------

nCI_code_cols <- paste("X20002.0.",0:32,sep="")
med_code_cols <- paste("X20003.0.",0:47,sep="")
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

medlist %>% dplyr::select(Code_description) %>%
  unlist %>%
  lapply(function(x){strsplit(as.character(x),split = " ")[[1]][1]}) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_all

medlist %>% 
  filter(Relevance=="mod/severe") %>%
  dplyr::select(Code_description) %>%
  unlist %>%
  lapply(function(x){strsplit(as.character(x),split = " ")[[1]][1]}) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_ms

ukb_data_processed %>% 
  as_tibble %>%
  dplyr::rename(Age_started_prev=X2867.0.0,
                Age_stopped_prev=X2897.0.0,
                Age_started_curr=X3436.0.0,
                Numperday_curr=X3456.0.0,
                Numperday_prev=X2887.0.0,
                EntryDate=X53.0.0,
                Smoking=X20116.0.0
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
  mutate(JiNS=ifelse(Job_involves_shift_work_int=="Never/rarely","No shift work",Job_involves_night_shift_work_int))%>%
  mutate(Packyears_prev=(Age_stopped_prev-Age_started_prev)*Numperday_prev/20) %>%
  mutate(Packyears_curr=(Age-Age_started_curr)*Numperday_curr/20) %>%
  mutate(Packyears=ifelse(is.na(Packyears_prev),Packyears_curr,Packyears_prev)) %>%
  ##
  mutate(Packyears_prev_n=(Age_stopped_prev_n-Age_started_prev_n)*Numperday_prev_n/20) %>%
  mutate(Packyears_curr_n=(Age-Age_started_curr_n)*Numperday_curr_n/20) %>%
  mutate(Packyears_n=ifelse(is.na(Packyears_prev_n),Packyears_curr_n,Packyears_prev_n)) %>%
  ##
  dplyr::select(Asthma_int,X2316.0.0,JiNS,Sex,Year_of_birth,Smoking,Alcohol=X20117.0.0,
                Ethnicity=X21000.0.0,TDI=X189.0.0,BMI=X21001.0.0,Asthma_age_diagnosed_int,X20154.0.0,
                SleepDur=X1160.0.0,
                DaysWalked=X864.0.0,DaysModerate=X884.0.0,DaysVigorous=X904.0.0,
                med_code_cols,
                Job_SOC,
                nCI_code,
                Packyears,Chronotype=X1180.0.0,Packyears,
                LengthofWW=X767.0.0,
                Alcintake=X1558.0.0,
                Job_AsthmaRisk,
                Job_MedRequired,
                X22601.0.0:X22601.0.39,
                X22602.0.0:X22602.0.39,
                X22603.0.0:X22603.0.39,
                X22650.0.0:X22650.0.32,
                X22640.0.0:X22640.0.32,
                X22643.0.0:X22643.0.19,
                X22653.0.0:X22653.0.32,Packyears_n,Smoking_n,
                gen_sex=X22001.0.0) %>%
  mutate(X22650.0.33=NA,X22650.0.34=NA,X22650.0.35=NA,X22650.0.36=NA,X22650.0.37=NA,X22650.0.38=NA,X22650.0.39=NA) %>%
  mutate(X22640.0.33=NA,X22640.0.34=NA,X22640.0.35=NA,X22640.0.36=NA,X22640.0.37=NA,X22640.0.38=NA,X22640.0.39=NA) %>%
  mutate(Hypertension=grepl(",1065,",nCI_code)) %>%
  mutate(High_Cholesterol=grepl(",1473,",nCI_code)) %>%
  mutate(Sleep_Apnoea=grepl(",1123",nCI_code)) %>%
  mutate(COPD=grepl(",1112",nCI_code)) %>%
  mutate(Emp_Chron_Bronchitis=grepl(",1113",nCI_code)) %>%
  mutate(Bronchiectasis=grepl(",1114",nCI_code)) %>%
  mutate(Inter_lung_disease=grepl(",1115",nCI_code)) %>%
  mutate(Other_resp_probs=grepl(",1117",nCI_code)) %>%
  mutate(GastroOesReflux=grepl(",1138",nCI_code)) %>%
  mutate(Asthma2=grepl(",1111,",nCI_code)) %>%
  unite(col=med_code,med_code_cols,sep=",",remove=TRUE) %>%
  mutate(med_code=paste0(",",med_code,",")) %>%
  mutate(Asthma_med_all=(grepl(patterns_all,med_code))) %>%
  mutate(Asthma_med_ms=(grepl(patterns_ms,med_code))) %>%
  mutate(Asthma_def=Asthma_med_all & Asthma2) %>%
  mutate(Asthma_def_ms=Asthma_med_ms & Asthma2) %>%
  mutate(FEV1lt80=X20154.0.0<80) %>%
  mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Sometimes","Usually","Always"))) %>%
  mutate(Smoking=factor(Smoking,levels=c(0,-3,1,2))) %>%
  mutate(Alcohol=factor(Alcohol,levels=c(0,-3,1,2))) %>% 
  mutate(Ethnicity=factor(Ethnicity,levels=c(1,sort(unique(Ethnicity))[-3]))) %>%
  mutate(Packyears_nn=Packyears_n) %>%
  mutate(Packyears_n=ifelse(Smoking==0,0,Packyears_n)) %>% ##
  mutate(Packyears_nn=ifelse(Smoking_n%in%c("not a smoker",
                                            "not a current smoker, occassionally smoked previously",
                                            "current occassional smoker, no history of heavy smoking"),0,Packyears_n)) %>% ##
  mutate(JiNS=plyr::revalue(JiNS, c("Always"="Always","Usually"="Sometimes","Sometimes"="Sometimes"))) -> model_data

# model_data %>%
#   filter(#Hypertension==T,
#     #High_Cholesterol==T,
#     #Sleep_Apnoea==F,
#     COPD==F,
#     Emp_Chron_Bronchitis==F#,
#     #Bronchiectasis==F,
#     #Inter_lung_disease==F,
#     #Other_resp_probs==F#,
#     #GastroOesReflux==F
#   ) -> model_data

se_seq<-function(start,end){
  if(is.na(end)){
    out<-NA
  } else{
    end2<-end
    if(end==-313){
      end2 <- 2017
    }
    out<-paste(end2:start,sep=",")
  }
  return(out)
}

for(i in 0:39){
  eval(parse(text=paste0("model_data %>% mutate(yij.",i,"= map2(X22602.0.",i,",X22603.0.",i,",se_seq)) -> model_data")))
} 
for(i in 0:39){
  eval(parse(text=paste0("model_data %>% mutate(yinsj.",i,"= ifelse(X22650.0.",i,"%in%c(0,1),map2(X22602.0.",i,",X22603.0.",i,",se_seq),NA)) -> model_data")))
} 
for(i in 0:39){
  eval(parse(text=paste0("model_data %>% mutate(yimnsj.",i,"= ifelse(X22640.0.",i,"%in%c(0,1),map2(X22602.0.",i,",X22603.0.",i,",se_seq),NA)) -> model_data")))
} 

model_data %>% mutate(yij_all=NA) -> model_data
for(i in 0:39){
  eval(parse(text=paste0("model_data %>% mutate(yij_all= map2(yij.",i,",yij_all,c)) -> model_data")))
} 
model_data %>% mutate(yinsj_all=NA) -> model_data
for(i in 0:39){
  eval(parse(text=paste0("model_data %>% mutate(yinsj_all= map2(yinsj.",i,",yinsj_all,c)) -> model_data")))
} 
model_data %>% mutate(yimnsj_all=NA) -> model_data
for(i in 0:39){
  eval(parse(text=paste0("model_data %>% mutate(yimnsj_all= map2(yimnsj.",i,",yimnsj_all,c)) -> model_data")))
} 

model_data %>% mutate(yiMNSorNSj_all= map2(yinsj_all,yimnsj_all,c)) -> model_data

uni_num_2008 <- function(x){
  x %>% unique %>% as.numeric() %>% .[.<=2008] %>% length %>% .[] -1 -> x_out
  return(x_out)
}

model_data %>% 
  mutate(num_yij=map(yij_all,uni_num_2008)) %>%
  #mutate(num_yij=unlist(map(yij_all_u,length))-1) %>% 
  mutate(num_yinsj=map(yinsj_all,uni_num_2008)) %>%
  #mutate(num_yinsj=unlist(map(yinsj_all_u,length))-1) %>%
  mutate(num_yimnsj=map(yimnsj_all,uni_num_2008)) %>%
  #mutate(num_yimnsj=unlist(map(yimnsj_all_u,length))-1) %>%
  mutate(num_yiMNSorNSj=map(yiMNSorNSj_all,uni_num_2008)) %>%
  #mutate(num_yiMNSorNSj=unlist(map(yiMNSorNSj_all_u,length))-1) %>%
  mutate(sum.MNSorNS.group = ifelse(num_yiMNSorNSj==0,"none",
                                    ifelse(num_yiMNSorNSj<5,"lessthan5",
                                           ifelse(num_yiMNSorNSj<10,"gt5lt10","greaterthan10")))) %>%
  mutate(sum.MNSorNS.group=factor(sum.MNSorNS.group,levels=c("none","lessthan5","gt5lt10","greaterthan10")))    -> model_data

save(model_data,file="data/lifetime_data.RData")

###########################
load("data/lifetime_data.RData")

model_vec<-c("sum.MNSorNS.group + Sex + Year_of_birth",
             "sum.MNSorNS.group + Sex + Year_of_birth + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous  + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "sum.MNSorNS.group + Sex + Year_of_birth + Smoking_n + Alcohol + Ethnicity + TDI + SleepDur + DaysWalked + DaysModerate + DaysVigorous + BMI + Packyears_nn + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates + mediators (95% CI)")

model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def==TRUE)) %>%filter(num_yij!=0)%>% filter(!is.na(Packyears_nn))  -> model_data_temp
DependentVar <- "Asthma_def"
ORmodelrun.sumNSW(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab6asthmadef.sumNSW

model_data %>% 
  mutate(num_yiMNSorNSj=unlist(num_yiMNSorNSj)) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) %>% filter(num_yij!=0)%>% filter(!is.na(Packyears_nn))  -> model_data_temp
DependentVar <- "Asthma_def_ms"
ORmodelrun.sumNSW(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms.sumNSW
trend.sumNSW(model_data_temp,DependentVar,model_vec,model_names)-> pval.sumNSW

###########################


model_vec<-c("sum.MNSorNS.group  + Year_of_birth",
             "sum.MNSorNS.group  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW + Job_AsthmaRisk + Job_MedRequired + Chronotype",
             "sum.MNSorNS.group  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW + Job_AsthmaRisk + Job_MedRequired + Chronotype + Smoking_n + Packyears_nn + BMI + SleepDur")
model_names <- c("Model 1: Age adjusted.",
                 #"Model 2: Adjusted by age, smoking status, pack years, alcohol status, daily alcohol intake, ethnicity, TDI, days exercised (walked, moderate, vigorous), chronotype, length of working week, job asthma risk, job medical required.",
                 "Model 2: Adjusted by multivariate covariates.",
                 "Model 3: Model 2 covariates + mediators")

model_data  %>%
  filter(!is.na(JiNS)) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(Packyears_nn)) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  filter(num_yij!=0) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"

ORmodelrun.sumNSW(model_data_temp%>% filter(gen_sex==0),DependentVar,model_vec,model_names)[[1]] -> mslife_women
ORmodelrun.sumNSW(model_data_temp%>% filter(gen_sex==1),DependentVar,model_vec,model_names)[[1]] -> mslife_men
