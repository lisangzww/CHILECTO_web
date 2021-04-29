setwd("D:/KUL-MSCA/PhDsupervision/Xiaoyu_Tian/ACL_paper/data")

Sys.setlocale(category = "LC_ALL", locale = "Chinese")

library(readr)
library(caret)
library(dplyr)
library(lattice)
library(pdp) 
library(nnet)
library(car)
library(effects)
library(sjPlot)
library(ggeffects)
library(sjmisc)
library(ggplot2)
library(reshape)

data <- read_csv("slr20201122.csv")

slr2 <- data %>%
  select(Item,CrLocus,CrSynt,CrSem,CrPers,CrDef,CrIntent,CseModality,CseAdv,CseNeg,
         CeSynt,CeSem,CePers,CeDef,CeControl,CeRole,CsedModality,CsedAdv,CsedNeg,CrCeCoref,
         PredSynt,PredSem,CsedProsody,CsedSemS,CsedSemT,Structure,ClauseType,
         Variety)  

slr2[] <- lapply(slr2, factor)

set.seed(33)

trainsamples2 <- createDataPartition(slr2$Item, times = 1, p = 0.70)
trainsamples2 <- unlist(trainsamples2)
data_train2 <- slr2[trainsamples2, ]
data_test2 <- slr2[-trainsamples2, ]

fit<-multinom(Item ~ PredSynt+Variety+CeSynt+CsedProsody+ClauseType+CsedSemT+Variety:CsedSemT, data=data_train2)
Anova(fit)

sjPlot::tab_model(fit)

tab_model(fit)
Var_CsedSemT<-ggeffect(fit, type = "pred", terms = c( "CsedSemT","Variety") ,ci.lvl = 0.95)

plot(Var_CsedSemT)

###
head(pp <- fitted(fit))
d1 <- data.frame(Variety = c("ml", "tw", "sg"), CsedSemT = c("ment"))
predict(fit, newdata = d1, "probs")




inter <- effect("Variety*CsedSemT",fit)
inter <- as.data.frame(inter)
inter <- melt(inter, id.vars = c("Variety", "CsedSemT"), value.name = "probability") %>%
    filter(variable == c("prob.ling","prob.rang","prob.shi"))
inter

ggplot(inter, aes(x = Variety, y = value, colour = CsedSemT)) + facet_grid(variable ~
                                                                                        ., scales = "free")

ggplot(inter, aes(y = Variety, x = CsedSemT, colour = value)) + geom_line() + facet_grid(variable ~
                                                                                        ., scales = "free")

ggplot(inter, aes(x = Variety, y = value, colour = CsedSemT))
ggplot(inter, aes(x = Variety, y = value, colour = CsedSemT)) 



+ facet_grid(variable ~
                                                                                        ., scales = "free")
ggplot(data=inter, 
       aes(x=CsedSemT, y=value, 
           group=Variety,
           shape=Variety,
           color=Variety)) + 
  geom_line() + 
  geom_point() +
  facet_grid(.~value )


