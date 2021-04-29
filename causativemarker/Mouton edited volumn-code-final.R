setwd("~/Desktop")
library(readr)
library(formatR)
library(party)
library(caret)
library(dplyr)
library(lattice)
library(pdp) 
library(nnet)
library(car)
library(ggeffects)
library(sjmisc)
library(sjPlot)

# Preparing the data:
data <- read_csv("slr20200930.csv")

slr <- data %>%
  select(Item,CrLocus,CrSynt,CrSem,CrPers,CrDef,CrIntent,CseModality,CseAdv,CseNeg,
         CeSynt,CeSem,CePers,CeDef,CeControl,CeRole,CsedModality,CsedAdv,CsedNeg,CrCeCoref,
         PredSynt,PredSem,CsedProsody,CsedSemS,CsedSemT,Structure,ClauseType,
         Variety)  

slr[] <- lapply(slr, factor)

# Data splitting (70% training set, 30% test set):
set.seed(18)

trainsamples <- createDataPartition(slr$Item, times = 1, p = 0.70)
trainsamples <- unlist(trainsamples)
data_train <- slr[trainsamples, ]
data_test <- slr[-trainsamples, ]

# Creating a random forest using {party} package:
m_cf <- cforest(Item ~ ., data=data_train, 
                 control = cforest_unbiased(ntree= 1000, mtry = 5))

m_cf.varimp <- varimp(m_cf, conditional=TRUE)

dotplot(sort(m_cf.varimp), main="Random forest (ntree=1000)", xlab="variable importance", 
        panel=function(x,y){
  panel.dotplot(x,y,col="darkblue",pch=16,cex=1.2)
  panel.abline(v=abs(min(m_cf.varimp)),col="red",lty="longdash",lwd=2)
  panel.abline(v=0, col="blue")
}
)

# Prediction of random forest on the 30% test data:
pred <- predict(m_cf1,newdata = data_test, OOB = TRUE, type = "response")
table(observed=slr$Item[-trainsamples],predicted=pred) 

# Building a conditional inference tree with the variables evaluated to be significant 
# by the random forest using {party} package:
ctree_model <- ctree(Item~Variety + CsedSemT + CeSynt + CsedProsody + ClauseType 
                     + PredSynt + PredSem, data=data_train, 
                     controls = ctree_control(testtype = "MonteCarlo", mincriterion = 0.95, minbucket = 50))
ctree_model
plot(ctree_model, main="Conditional Inference Tree (alpha=0.05, min = 50)")

data_test$pred <- predict(ctree_model, data_test[,-1])
confusionMatrix(data_test$Item, factor(data_test$pred))

# Building a multinomial logistic regression model with the seven variables 
# proved to be significant in the random forest model as well as their interactions 
# with the variable Variety:

# We noticed that there are only seven observations with the value CeSynt = cl. 
# In order to avoid the effect of data sparsity, we removed the seven observations 
# from the data and built the multinomial logistic regression model.

data <- read_csv("slr20201122.csv")

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

fit0 <- multinom(Item ~ PredSynt + Variety + CeSynt + CsedProsody + ClauseType + 
                   PredSem + CsedSemT + Variety:PredSynt + Variety:CeSynt + Variety:CsedProsody
                 + Variety:ClauseType + Variety:PredSem + CsedSemT:Variety, data=data_train2)
Anova(fit0)


fit<-multinom(Item ~ PredSynt+Variety+CeSynt+CsedProsody+ClauseType+CsedSemT+Variety:CsedSemT, data=data_train2)
Anova(fit)
tab_model(fit)
Var_CsedSemT<-ggeffect(fit, type = "pred", terms = c( "CsedSemT","Variety") ,ci.lvl = 0.95)

plot(Var_CsedSemT)
