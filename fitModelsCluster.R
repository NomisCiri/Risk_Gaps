
library(tidyverse)
library(here)
library(brms)
library(mice)

bashInput <- commandArgs(trailingOnly = TRUE)


#Function Defining
split_facets <- function(x) {
  facet_expr <- unlist(x[["facet"]][["params"]][c("cols", "rows", "facets")])
  facet_levels <- lapply(facet_expr, rlang::eval_tidy, data = x[["data"]])
  facet_id <- do.call(interaction, facet_levels)
  panel_data <- split(x[["data"]], facet_id)
  plots <- vector("list", length(panel_data))
  for (ii in seq_along(plots)) {
    plots[[ii]] <- x
    plots[[ii]][["data"]] <- panel_data[[ii]]
    plots[[ii]][["facet"]] <- facet_null()
  }
  plots
}





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

ARQs$age=as.numeric(as.character(ARQs$age))
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

cftPath = "A_RawData/Covariates/cft/logs"
FilesCFT=list.files(path = cftPath)

cftResults=tibble(
  cftScore=1:length(FilesCFT),
  subject=1:length(FilesCFT)
)

for (i in 1:length(FilesCFT)){
  cft=read.delim(paste0(cftPath,"/",FilesCFT[i]),header = FALSE)
  #print(Files[i])
  cftResults$cftScore[i]=as.numeric(as.character(cft[3,2]))
  cftResults$subject[i]=strsplit(FilesCFT[i], "_")[[1]][3]
}
cftResults=unique(cftResults)
numbersPath = "A_RawData/Covariates/Numbers/logs"

FilesNumbers=list.files(path = numbersPath)
numbersResults=tibble(
  numberFowards=1:length(FilesNumbers),
  numberBackwards=1:length(FilesNumbers),
  subject=1:length(FilesNumbers)
)

for (i in 1:length(FilesNumbers)){
  numbers=read.delim(paste0(numbersPath,"/",FilesNumbers[i]),header = FALSE)
  #print(Files[i])
  numbersResults$numberFowards[i]=as.numeric(as.character(numbers[5,2]))
  numbersResults$numberBackwards[i]=as.numeric(as.character(numbers[6,2]))
  numbersResults$subject[i]=as.numeric(strsplit(FilesNumbers[i], "_")[[1]][3])
}

numbersResults=unique(numbersResults)
numbersResults<-numbersResults%>%mutate(numbersTotal=numberFowards+numberBackwards)

Eyes<-read_csv(file = "./A_RawData/Covariates/MindInEyesAll.csv")
Eyes%<>%mutate(subject=as.double(subject))%>%select(-X1)

cftResults<-cftResults%>%mutate(subject=as.double(subject))
  
normalEng%<>%as_tibble()%>%left_join(x=.,y=numbersResults%>%select(subject,numbersTotal),by="subject")%>%
  left_join(x=.,y=cftResults,by="subject")%>%left_join(x=.,y=Eyes,by="subject")

# normalEng[is.na(normalEng$MinInEyesSum),]$MinInEyesSum=mean(normalEng$MinInEyesSum,na.rm=T)
# normalEng[is.na(normalEng$numbersTotal),]$numbersTotal=mean(normalEng$numbersTotal,na.rm=T)
# normalEng[is.na(normalEng$cftScore),]$cftScore=mean(normalEng$cftScore,na.rm=T)

if (as.numeric(bashInput)==1){
  MultiNoIllegal <-  brm(
    formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany*LinearAgeTrans+HowMany*QuadraticAgeTrans+(1|p|subject)+(1|Scale),
    data = normalEng[normalEng$Scale!="Thrill_Seeking" & normalEng$Scale!="Reckless",],
    family="cumulative",
    # this next line is only to keep the 40000 small in size!
    chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10))
  saveRDS(MultiNoIllegal,file="../ModelFits/MultiNoIllegal.rds")
} else if (as.numeric(bashInput)==2){
  MultiIQ <-  brm(
    formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany*LinearAgeTrans+HowMany*QuadraticAgeTrans+Risk*LinearAgeTrans+Risk*QuadraticAgeTrans+(1|p|subject)+(1|Scale),
    data = normalEng[normalEng$Scale!="Thrill_Seeking",],
    family="cumulative",
    # this next line is only to keep the 40000 small in size!
    chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10)
  )
  saveRDS(MultiIQ,file="../ModelFits/MultiRiskAgeIna.rds")
  
} else if (as.numeric(bashInput)==3){
  library(mice)
  imp <- mice(normalEng[normalEng$Scale!="Thrill_Seeking",], m = 10)
  MultiIQ <-  brm_multiple(
    formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany*LinearAgeTrans+HowMany*QuadraticAgeTrans+MinInEyesSum*LinearAgeTrans+MinInEyesSum*QuadraticAgeTrans+
      cftScore*LinearAgeTrans+cftScore*QuadraticAgeTrans+numbersTotal*LinearAgeTrans+numbersTotal*QuadraticAgeTrans+(1|p|subject)+(1|Scale),
    data = imp,
    family="cumulative",
    # this next line is only to keep the 40000 small in size!
    chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10)
    )
  saveRDS(MultiIQ,file="../ModelFits/MultiThrillSeekQuadLinMapC_IQ.rds")
}else if (as.numeric(bashInput)==4){
 # library(mice)
  imp <- mice(normalEng[normalEng$Scale=="Thrill_Seeking",], m = 10, print = TRUE)
  MultiIQNoThrill <-  brm_multiple(
    formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany*LinearAgeTrans+HowMany*QuadraticAgeTrans+MinInEyesSum*LinearAgeTrans+MinInEyesSum*QuadraticAgeTrans+
      cftScore*LinearAgeTrans+cftScore*QuadraticAgeTrans+numbersTotal*LinearAgeTrans+numbersTotal*QuadraticAgeTrans+(1|p|subject)+(1|Scale),
    data = imp,
    family="cumulative",
    # this next line is only to keep the 40000 small in size!
    chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10)
  )
  saveRDS(MultiIQNoThrill,file="../ModelFits/MultiNoThrillSeekQuadLinMapC_IQ.rds")
  
}else if (as.numeric(bashInput)==5){
MultiNoAgeQ <-  brm(
  formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany+(1|p|subject)+(1|Scale),
  data = normalEng[normalEng$Scale!="Thrill_Seeking",],
  family="cumulative",
  # this next line is only to keep the example small in size!
  chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10))
saveRDS(MultiNoAgeQ,file="../ModelFits/MultiNoAgeNoThrill.rds")

}else if (as.numeric(bashInput)==6){
MultiNoAgeQ <-  brm(
  formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany+(1|p|subject)+(1|Scale),
  data = normalEng[normalEng$Scale=="Thrill_Seeking",],
  family="cumulative",
  # this next line is only to keep the example small in size!
  chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10))
  saveRDS(MultiNoAgeQ,file="../ModelFits/MultiNoAgeThrill.rds")
} else if (as.numeric(bashInput)==7){
  MultiIQ <-  brm(
    formula = mvbind(Recom+1,HowOften+1)~Risk+sex+HowMany*LinearAgeTrans+HowMany*QuadraticAgeTrans+Risk*LinearAgeTrans+Risk*QuadraticAgeTrans+(1|p|subject)+(1|Scale),
    data = normalEng[normalEng$Scale=="Thrill_Seeking",],
    family="cumulative",
    # this next line is only to keep the 40000 small in size!
    chains = 6, cores = 6,init=0, iter = 20000,control = list(adapt_delta = 0.9999999999, stepsize = 0.01, max_treedepth = 10)
  )
  saveRDS(MultiIQ,file="../ModelFits/MultiRiskAgeIna_Thrill.rds")
  
}


