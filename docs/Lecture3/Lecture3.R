
#train.csv 불러오기
library(tidyverse)
library(readr)
library(car)
library(xlsx)




train <- read_csv("Lecture3/train.csv")
test <- read_csv("Lecture3/test.csv")

test$KitchenQual <- ifelse(is.na(test$KitchenQual),"TA", test$KitchenQual)
test$GarageCars <- ifelse(is.na(test$GarageCars),0, test$GarageCars)
is.na(test$KitchenQual)


sample <- read_csv("Lecture3/sample_submission.csv")

              


EXP1 <- lm(SalePrice ~ OverallQual,data = train)

EXP2 <- lm(SalePrice ~ OverallQual + OverallCond + LotArea + YearBuilt + YearRemodAdd + MasVnrArea + TotalBsmtSF + GrLivArea + FullBath +      
             HalfBath + BedroomAbvGr + KitchenQual +TotRmsAbvGrd + Fireplaces + GarageCars + PoolArea + MiscVal , data = train)
EXP3 <- lm(SalePrice ~ OverallQual + OverallCond + LotArea + YearBuilt + YearRemodAdd + Fireplaces + TotalBsmtSF + GrLivArea + FullBath +      
             HalfBath + BedroomAbvGr + KitchenQual +TotRmsAbvGrd + GarageCars + PoolArea + MiscVal , data = train)


summary(EXP1)
summary(EXP3)


#StepWise Regression

## MasVnrArea NA가 데이터 값 중에 있었다. 그래서 자꾸만 오류가 생김
sum(is.na(train$MasVnrArea))
# 총 8개 건수 발견, 차라리 변수로 고려하지 않는 게 낫다고 판단하였다.

Train.Forward <- step(EXP1,scope=list(lower=EXP1,upper=EXP3),direction = "forward")
Train.Backward <- step(EXP3,scope=list(lower=EXP1,upper=EXP3),direction = "backward")
Train.Both <- step(EXP3,scope=list(lower=EXP1,upper=EXP3),direction = "both")
summary(Train.Forward)
summary(Train.Backward)
summary(Train.Both)
names(Train.Both$coefficients)

# 다행히도(?) 3가지 모든 같은 독립 변수들을 선택하였다.


#다중공선성을 구하여 변수를 제거해보자.
#Variance Inflation Factor 가 10이 넘으면 문제가 있다는 것이다.

vif(Train.Both) 

plot(Train.Both)

##524,692,1183,1299번 Outlier

Modified_Train <- lm(SalePrice ~ OverallQual + OverallCond + LotArea + YearBuilt +   
                YearRemodAdd + Fireplaces +  GrLivArea + FullBath +  
                HalfBath + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + GarageCars  
                 , data=train[-c(314,336,524,692,1183,1299), ])
summary(Modified_Train)

par(mfrow=c(2,2))
plot(Modified_Train)

## R^2 값이 소폭 상승하였다 (From 0.7825 to 0.8298)

sample[,3] <- predict(Modified_Train, newdata = test)


#TEST MSE 예시

a<-((sample[,2]-sample[,3])^2)
a<-na.exclude(a)
sum(a)



EXP11 <- glm(SalePrice~ OverallQual, family = Gamma(link="log"), data = train)

EXP12 <- glm(SalePrice ~ OverallQual + OverallCond + LotArea + YearBuilt + YearRemodAdd + Fireplaces + GrLivArea + BsmtHalfBath + FullBath +      
               HalfBath + BedroomAbvGr + KitchenQual +TotRmsAbvGrd + GarageCars + PoolArea + MiscVal , Gamma(link="log"), data = train)
summary(EXP11)
summary(EXP12)


#StepWise Regression

Train.Forward2 <- step(EXP11,scope=list(lower=EXP11,upper=EXP12),direction = "forward")
Train.Backward2 <- step(EXP12,scope=list(lower=EXP11,upper=EXP12),direction = "backward")
Train.Both2 <- step(EXP12,scope=list(lower=EXP11,upper=EXP12),direction = "both")
summary(Train.Forward2)
summary(Train.Backward2)
summary(Train.Both2)
names(Train.Both2$coefficients)

# 모두 같은 독립 변수들을 선택하였다.


#다중공선성을 구하여 변수를 제거해보자.
#Variance Inflation Factor 가 10이 넘으면 문제가 있다는 것이다.

vif(Train.Both2) 

plot(Train.Both2)

##250,524,689,1299번 이상치

Modified_Train2 <- glm(SalePrice ~ OverallQual + OverallCond + LotArea + YearBuilt +   
                         YearRemodAdd + Fireplaces +  GrLivArea + FullBath +  
                         HalfBath + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + GarageCars , family = Gamma(link="log"), data=train[-c(250,524,689,1299), ])
summary(Modified_Train2)

par(mfrow=c(2,2))
plot(Modified_Train2)



## AIC 값이 소폭 하락하였다

sample[,4] <- exp(predict(Modified_Train2, newdata = test))

EXP21 <- lm(SalePrice ~ -1 + I(OverallQual^2),data = train)
EXP22 <- lm(SalePrice ~ -1 + I(OverallQual^2) + I(OverallCond^2) + LotArea + YearBuilt + YearRemodAdd + Fireplaces + GrLivArea + BsmtHalfBath + FullBath +      
              HalfBath + BedroomAbvGr + KitchenQual +TotRmsAbvGrd + GarageCars + PoolArea + MiscVal , data = train)


summary(EXP21)
summary(EXP22)


#StepWise Regression

Train.Forward3 <- step(EXP21,scope=list(lower=EXP21,upper=EXP22),direction = "forward")
Train.Backward3 <- step(EXP22,scope=list(lower=EXP21,upper=EXP22),direction = "backward")
Train.Both3 <- step(EXP22,scope=list(lower=EXP21,upper=EXP22),direction = "both")
summary(Train.Forward3)
summary(Train.Backward3)
summary(Train.Both3)
names(Train.Both$coefficients)

vif(Train.Both3) 
#YearBuilt 변수 삭제 (VIF=108.7이 나옴)

VIF_Train <- lm(SalePrice ~ -1 + I(OverallQual^2) + I(OverallCond^2) + LotArea +   
                  YearRemodAdd + Fireplaces +  TotalBsmtSF + GrLivArea + FullBath +  
                  HalfBath + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + GarageCars , data=train)

vif(VIF_Train)
summary(VIF_Train)

VIF_Train2 <- lm(SalePrice ~ -1 + I(OverallQual^2) + I(OverallCond^2) + LotArea +   
                  Fireplaces +  TotalBsmtSF + GrLivArea + FullBath +  
                  HalfBath + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + GarageCars , data=train)

vif(VIF_Train2)
summary(VIF_Train2)




par(mfrow=c(2,2))
plot(VIF_Train)

##524,692,1183,1299번 Outlier
Modified_Train3<-lm(SalePrice ~ -1 + I(OverallQual^2) + I(OverallCond^2) + LotArea +   
                       Fireplaces +  TotalBsmtSF + GrLivArea + FullBath +  
                      HalfBath + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + GarageCars , data=train[-c(524,692,1183,1299), ])
summary(Modified_Train3)

par(mfrow=c(2,2))
plot(Modified_Train3)

## R^2 값이 소폭 상승하였다 (From 0.9673 to 0.9767)


sample[,5] <- predict(Modified_Train3, newdata = test)


## Overfitting(과적합)의 문제가 발생


FinalEXP <- lm(SalePrice ~ -1 + I(OverallQual^2) +  YearRemodAdd + I(sqrt(LotArea)) + GarageCars+ TotRmsAbvGrd + KitchenQual + PavedDrive + FullBath + GrLivArea ,data = train)
vif(FinalEXP)
summary(FinalEXP)
plot(FinalEXP)
FinalEXP <- lm(SalePrice ~ -1 + I(OverallQual^2) + I(sqrt(LotArea)) + GarageCars + TotRmsAbvGrd +  KitchenQual + PavedDrive + FullBath + GrLivArea ,data = train[-c(314,336,692,1183,1299),])
vif(FinalEXP)
summary(FinalEXP)
plot(FinalEXP)


FinalEXP2 <- glm(SalePrice ~ I(OverallQual^2) + YearRemodAdd + I(sqrt(LotArea)) + GarageCars+ TotRmsAbvGrd + KitchenQual + PavedDrive + FullBath + GrLivArea , family = Gamma(link="log")  ,data = train)
vif(FinalEXP2)
summary(FinalEXP2)
plot(FinalEXP2)
FinalEXP2 <- glm(SalePrice ~ I(OverallQual^2) + YearRemodAdd + I(sqrt(LotArea)) + GarageCars + TotRmsAbvGrd +  KitchenQual + PavedDrive + FullBath + GrLivArea , family = Gamma(link="log") ,data = train[-c(314,336,692,1183,1299),])
vif(FinalEXP2)
summary(FinalEXP2)
plot(FinalEXP2)






sample[,6] <- predict(FinalEXP, newdata = test)
sample[,7] <- exp(predict(FinalEXP2, newdata = test))



for (i in 1:nrow(sample)) { 
  if ((abs(sample[i,2]-sample[i,6]) - abs(sample[i,2]-sample[i,7])) >= 0){
  sample[i,8] <- sample [i,7]
} else {sample[i,8] <- sample [i,6]}
}

SqrtTESTMSE <- data.frame(lm=c(0) , glm=c(0), lmwithoutintercept=c(0) , lm2=c(0) , glm2 =c(0) , mix =c(0))

for (i in 1:6) {
  Error <- ((sample[,2]-sample[,i+2])^2)
  Error<-na.exclude(Error)
  SqrtTESTMSE[i]<-sqrt(sum(Error)/nrow(sample))
  }

SqrtTESTMSE

colnames(sample) <- c("Id", "SalePrice","lm","glm","lmwithoutintercept","lm2","glm2","MIX")


Newsample <- sample %>% select("Id", "SalePrice", "lm2")

colnames(Newsample) <- c("Id", "SalePrice","SalePrice(Fitted)")



view(Newsample)

write.xlsx(Newsample, file = "Prediction.xlsx",
           sheetName = "Prediction", append = FALSE)

