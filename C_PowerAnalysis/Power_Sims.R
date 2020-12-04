library(tidyverse)
library(rstan)
library(brms)
library(broom)
library(modelr)
#memory.limit(size=14000)

bashInput<-commandArgs(trailingOnly = TRUE)## its only an index for saving the results


# i dont need this anymore
#rstan_options(auto_write = TRUE) 
linMap <- function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}
# this transforms my age regressors.
#ARQs<-readRDS("A_RawData/ARQs.rds")
ARQs<-readRDS("A_RawData/ARQs.rds")

ARQs$age=as.numeric(as.character(ARQs$age))# get data
# here i bring the quadratic and linear age onto the same scale as the other predictors, so that we can better interpret theplot()
#Betas.
ARQs$LinearAgeTrans<-(poly(ARQs$age,2)[,1])
ARQs$QuadraticAgeTrans<-(poly(ARQs$age,2)[,2]*-1)

#TidyQuestionnaireLongSums$TestPart
#TidyQuestionnaireLongSums<-
# get only the Sumscores

# some datawrangling
normalEng<-ARQs%>%mutate(DiffEng=(HowOften-Recom)
)%>%mutate(
  NormalizedDiffEng=as.vector(scale(DiffEng)),
  Bin= case_when(
    age<=13~1,
    (age<=15 & age>13)~2,
    (age<=17 & age>15)~2,
    (age<=19 & age>17)~3,
    (age>19)~3
    #(age>=22)~4
  ),
  SubScale=case_when(
    Scale=="Rebellious"~"Rebellious",
    Scale=="Reckless"~"Reckless",
    Scale=="Antisocial"~"Antisocial",
    Scale=="Thrill_Seeking"~"Thrill Seeking"
  ),
  Presence=case_when(
    Scale=="Rebellious"~"A",
    Scale=="Reckless"~"A",
    Scale=="Antisocial"~"A",
    Scale=="Thrill_Seeking"~"B"
  ),
  Bin=as.factor(Bin)
)

if (as.numeric(bashInput[2])==1){# if flag is one make power for the desirable scale
  #load the brms fit object which we will later use to simulate data.
  MultivariateDesirableQuadraticC<-readRDS("B_ModelFits/MultiThrillSeekQuadLinLinMapC.rds")
  
  
  #MultivariateUndesirableLinearQuadratic%>%spread_draws(b_HowOften_HowMany)%>%
  #  head(10)
  
  #create dummy data from the real data.
  newdataDes=expand.grid(
    Scale=unique(normalEng[normalEng$Scale=="Thrill_Seeking",]$Scale),
    subject=unique(normalEng$subject),
    LinearAgeTrans=NaN,
    QuadraticAgeTrans=NaN,
    Risk=c(0,1,2,3,4),
    sex=("Weiblich"),
    HowMany=c(0,1,2,3,4)# take only extreme things.
  )
  newdataDes$sex=as.character(newdataDes$sex)
  newdataDes$LinearAgeTrans=as.numeric(newdataDes$LinearAgeTrans)
  newdataDes$QuadraticAgeTrans=as.numeric(newdataDes$QuadraticAgeTrans)
  
  for (i in 1:length(unique(newdataDes$subject))){
    Sub<-unique(newdataDes$subject)[i]
    #print(Sub)
    Age=normalEng[normalEng$subject==Sub,]$LinearAgeTrans[1]
    Agesq=normalEng[normalEng$subject==Sub,]$QuadraticAgeTrans[1]
    Sex=normalEng[normalEng$subject==Sub,]$sex[1]
    
    newdataDes[newdataDes$subject==Sub,]$LinearAgeTrans=Age
    newdataDes[newdataDes$subject==Sub,]$QuadraticAgeTrans=Agesq
    newdataDes[newdataDes$subject==Sub,]$sex=Sex
    
    #normalEng[normalEng$subject==newdata$subject[i],]$LinearAgeTrans[1]
  }
  #recode
  newdataDes$sex=as.factor(newdataDes$sex)
  na.omit(newdataDes)->newdataDes
  #make new predictions
  PowerData=newdataDes
  print("alreadyThere?")
  PredictionsDes<-predict(MultivariateDesirableQuadraticC, newdata = newdataDes, re_formula = NULL,
                          transform = NULL, allow_new_levels = TRUE, summary = TRUE,
                          probs = c(0.025, 0.975), ntrys = 5)
  
  sim_d_and_fit <- function(seed) {
    set.seed(seed)
    PowerData$HowOften=NULL
    PowerData$Recom=NULL
    for (i in 1:length(PowerData$subject)){
      PowerData$HowOften[i]=sample( 1:5, 1, replace=TRUE, prob=PredictionsDes[i,,2] )
      PowerData$Recom[i]=sample( 1:5, 1, replace=TRUE, prob=PredictionsDes[i,,1] )
    }
    
    update(MultivariateDesirableQuadraticC,newdata=PowerData,cores=4,chains=4,iter=2000,warmup=200, seed = seed,control = list(adapt_delta=0.8,max_treedepth=10))%>% 
      tidy(prob = .95)
  }
  #Now iterate 100 times once more.
  print("here")
  n_sim=10
  s3 <-# this is where the magic happens. First i create an array of new seeds which i loop over in the next step and then i throw away all the sampling info and only keep the model esitamtes
    tibble(seed = 1:n_sim) %>% 
    mutate(tidy = map(seed, sim_d_and_fit)) %>% 
    unnest(tidy)
  
  saveRDS(s3,file=paste0("C_PowerAnalysis/iterDesirable",bashInput[1],".rds"))
}else if(as.numeric(bashInput[2]==2)){
  #load the brms fit object which we will later use to simulate data.
  MultivariateUnDesirableQuadraticC<-readRDS("B_ModelFits/MultiNoThrillSeekQuadLinLinMapC.rds")
  
  
  #MultivariateUndesirableLinearQuadratic%>%spread_draws(b_HowOften_HowMany)%>%
  #  head(10)
  
  #create dummy data from the real data.
  newdataUnDes=expand.grid(
    Scale=unique(normalEng[normalEng$Scale!="Thrill_Seeking",]$Scale),
    subject=unique(normalEng$subject),
    LinearAgeTrans=NaN,
    QuadraticAgeTrans=NaN,
    Risk=c(0,1,2,3,4),
    sex=("Weiblich"),
    HowMany=c(0,1,2,3,4)# take only extreme things.
  )
  newdataUnDes$sex=as.character(newdataUnDes$sex)
  newdataUnDes$LinearAgeTrans=as.numeric(newdataUnDes$LinearAgeTrans)
  newdataUnDes$QuadraticAgeTrans=as.numeric(newdataUnDes$QuadraticAgeTrans)
  
  for (i in 1:length(unique(newdataUnDes$subject))){
    Sub<-unique(newdataUnDes$subject)[i]
    #print(Sub)
    Age=normalEng[normalEng$subject==Sub,]$LinearAgeTrans[1]
    Agesq=normalEng[normalEng$subject==Sub,]$QuadraticAgeTrans[1]
    Sex=normalEng[normalEng$subject==Sub,]$sex[1]
    
    newdataUnDes[newdataUnDes$subject==Sub,]$LinearAgeTrans=Age
    newdataUnDes[newdataUnDes$subject==Sub,]$QuadraticAgeTrans=Agesq
    newdataUnDes[newdataUnDes$subject==Sub,]$sex=Sex
    
    #normalEng[normalEng$subject==newdata$subject[i],]$LinearAgeTrans[1]
  }
  #recode
  newdataUnDes$sex=as.factor(newdataUnDes$sex)
  na.omit(newdataUnDes)->newdataUnDes
  #make new predictions
  PowerData=newdataUnDes
  
  PredictionsUnDes<-predict(MultivariateUnDesirableQuadraticC, newdata = newdataUnDes, re_formula = NULL,
                          transform = NULL, allow_new_levels = TRUE, summary = TRUE,
                          probs = c(0.025, 0.975), ntrys = 5)
  
  # a function that simulates new responses from the posterior and then fits the model. I map over the function with a new random seed each time
  sim_d_and_fit <- function(seed) {
    set.seed(seed)
    PowerData$HowOften=NULL
    PowerData$Recom=NULL
    for (i in 1:length(PowerData$subject)){
      PowerData$HowOften[i]=sample( 1:5, 1, replace=TRUE, prob=PredictionsUnDes[i,,2] )
      PowerData$Recom[i]=sample( 1:5, 1, replace=TRUE, prob=PredictionsUnDes[i,,1] )
    }
    
    update(MultivariateUnDesirableQuadraticC,newdata=PowerData,cores=4,chains=4,iter=2000,warmup=200, seed = seed,control = list(adapt_delta=0.8,max_treedepth=10))%>% 
      tidy(prob = .95)
  }
  #Now iterate 100 times once more.
  
  n_sim=10
  s3 <-
    tibble(seed = 1:n_sim) %>% 
    mutate(tidy = map(seed, sim_d_and_fit)) %>% 
    unnest(tidy)
  
  saveRDS(s3,file=paste0("C_PowerAnalysis/iterUnDesirable",bashInput[1],".rds"))
}
