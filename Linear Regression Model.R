library(tidyverse)
library(car)
library(ggplot2)
library(moments)
library(corrplot)
library(GGally)
library(caret)
library(forcats)

#######################W8GROW
rm(list=ls())
options(max.print=2000)
##load data
ordata <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
#check the characteristic of W8GROW
summary(ordata$W8GROW)
str(ordata)

#delete missing value in dependent variable
ordata_clean <- ordata[ordata$W8GROW >= 0,]
summary(ordata_clean$W8GROW)
str(ordata_clean)

#plots of all categorical variable and W8GROW
vec <- c(1,2,3,30,41,43,50)
for ( i in 1:50 ) {
  x <- i %in% vec
  if (x == FALSE) {
    p1 <- ggplot(ordata_clean, aes(x = factor(ordata_clean[,i]), y =W8GROW )) + geom_boxplot() + xlab(colnames(ordata_clean[i]))
    print(p1)
    
  } else {
    print(i)
    
  }
}

par(mfrow = c(1, 1))
# boxplot of W8GROW
boxplot(ordata_clean$W8GROW)
ggplot(ordata_clean, aes(x = W8GROW)) + geom_density()

# QQplot of W8GROW
qqnorm(ordata_clean$W8GROW)
qqline(ordata_clean$W8GROW)

# check skewness and kurtosis of W8GROW
skewness(ordata_clean$W8GROW)
kurtosis(ordata_clean$W8GROW)


##whole model
model1 <- lm(W8GROW ~ W1GrssyrMP + W1yschat1 + factor(W1wrk1aMP) + factor(W1condur5MP) + factor(W1hea2MP) + factor(W1NoldBroHS) + 
               factor(W1InCarHH) + factor(W1hous12HH) + factor(W1usevcHH) + factor(W1hiqualdad) + factor(W1wrkfulldad) +
               factor(W1wrkfullmum) + factor(W1empsdad) + factor(W1ch0_2HH) + factor(W1ch3_11HH) + factor(W1ch12_15HH) + 
               factor(W1ch16_17HH) + factor(W1IndSchool) + factor(W1marstatmum) + factor(W1famtyp2) + factor(W1nssecfam) + 
               factor(W1ethgrpYP) +factor(W1heposs9YP) + factor(W1hwndayYP) + factor(W1truantYP) + factor(W1alceverYP) + 
               factor(W1bulrc) + factor(W1disabYP) + W2ghq12scr + factor(W2disc1YP) + factor(W2depressYP) + 
               factor(W6JobYP) + factor(W6UnivYP) + factor(W6EducYP) + factor(W6Apprent1YP) + factor(W6acqno) + factor(W6gcse) + 
               factor(W6als) + factor(W6OwnchiDV) + W6DebtattYP + factor(W8DDEGP) + W8DGHQSC + factor(W8DMARSTAT) + 
               factor(W8DACTIVITYC) + factor(W8DWRK) + factor(W8CMSEX) + factor(W8TENURE) + factor(W8QMAFI), data = ordata_clean)
summary(model1)



#log transformation
ordata_cleanlog <- ordata_clean
ordata_cleanlog$W8GROW <- log(ordata_cleanlog$W8GROW+1)

# boxplot of W8GROW after log
boxplot(ordata_cleanlog$W8GROW)
ggplot(ordata_cleanlog, aes(x = W8GROW)) + geom_density()


# QQplot of W8GROW after log
qqnorm(ordata_cleanlog$W8GROW)
qqline(ordata_cleanlog$W8GROW)

# check skewness and kurtosis of W8GROW after log
skewness(ordata_cleanlog$W8GROW)
kurtosis(ordata_cleanlog$W8GROW)

##box plos of categorical variables with log(W8GROW)
vec <- c(1,2,3,30,41,43,50)
for ( i in 1:50 ) {
  x <- i %in% vec
  if (x == FALSE) {
    p1 <- ggplot(ordata_cleanlog, aes(x = factor(ordata_cleanlog[,i]), y =W8GROW )) + geom_boxplot() + xlab(colnames(ordata_cleanlog[i]))
    print(p1)
    
  } else {
    print(i)
    
  }
}


##from the plot we can see that the W1truantYP, W1alceverYP and W1bulrc share the same box plots for missing values lower than -96
#test correlation 
cor_matrix <- cor(ordata_cleanlog[,c("W8GROW", "W1GrssyrMP", "W1yschat1", "W1wrk1aMP", "W1condur5MP", "W1hea2MP", "W1NoldBroHS",
                                     "W1InCarHH", "W1hous12HH", "W1usevcHH", "W1hiqualdad", "W1wrkfulldad", "W1wrkfullmum", 
                                     "W1empsdad", "W1ch0_2HH", "W1ch3_11HH", "W1ch12_15HH", "W1ch16_17HH", "W1IndSchool", "W1marstatmum", 
                                     "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP", "W1truantYP", "W1alceverYP", 
                                     "W1bulrc", "W1disabYP", "W2ghq12scr", "W2disc1YP", "W2depressYP", "W6JobYP", "W6UnivYP", "W6EducYP", 
                                     "W6Apprent1YP", "W6acqno", "W6gcse", "W6als", "W6OwnchiDV", "W6DebtattYP", "W8DDEGP", "W8DGHQSC", 
                                     "W8DMARSTAT", "W8DACTIVITYC", "W8DWRK", "W8CMSEX", "W8TENURE", "W8QMAFI")])

cor_matrix
corrplot(cor_matrix)


##whole model with log(W8GROW)
model2 <- lm(W8GROW ~ W1GrssyrMP + W1yschat1 + factor(W1wrk1aMP) + factor(W1condur5MP) + factor(W1hea2MP) + factor(W1NoldBroHS) + 
               factor(W1InCarHH) + factor(W1hous12HH) + factor(W1usevcHH) + factor(W1hiqualdad) + factor(W1wrkfulldad) +
               factor(W1wrkfullmum) + factor(W1empsdad) + factor(W1ch0_2HH) + factor(W1ch3_11HH) + factor(W1ch12_15HH) + 
               factor(W1ch16_17HH) + factor(W1IndSchool) + factor(W1marstatmum) + factor(W1famtyp2) + factor(W1nssecfam) + 
               factor(W1ethgrpYP) +factor(W1heposs9YP) + factor(W1hwndayYP) + factor(W1truantYP) + factor(W1alceverYP) + 
               factor(W1bulrc) + factor(W1disabYP) + W2ghq12scr + factor(W2disc1YP) + factor(W2depressYP) + 
               factor(W6JobYP) + factor(W6UnivYP) + factor(W6EducYP) + factor(W6Apprent1YP) + factor(W6acqno) + factor(W6gcse) + 
               factor(W6als) + factor(W6OwnchiDV) + W6DebtattYP + factor(W8DDEGP) + W8DGHQSC + factor(W8DMARSTAT) + 
               factor(W8DACTIVITYC) + factor(W8DWRK) + factor(W8CMSEX) + factor(W8TENURE) + factor(W8QMAFI), data = ordata_cleanlog)
summary(model2)


##Subset the dataset to only include variables with * in lm1
datasub <- ordata_cleanlog[, c("W1hiqualdad","W1empsdad","W1nssecfam","W2disc1YP","W6JobYP", "W6DebtattYP",
                               "W8DDEGP", "W8DACTIVITYC","W8GROW","W8CMSEX","W8QMAFI")]
head(datasub)
summary(datasub)
str(datasub)


##collinearity 
cor_matrix1 <- cor(ordata_cleanlog[,c("W1hiqualdad","W1empsdad","W1nssecfam","W2disc1YP","W6JobYP", "W6DebtattYP",
                             "W8DDEGP", "W8DACTIVITYC","W8GROW","W8CMSEX","W8QMAFI")])
cor_matrix1

vif1 <- vif(lm(W8GROW ~ ., datasub))
vif1

#collinearity plot
corrplot(cor_matrix1)

##delete W1hiqualdad & W1empsdad because of high collinearity 
datasubset <- ordata_cleanlog[, c("W1nssecfam","W2disc1YP","W6JobYP", "W6DebtattYP",
                                  "W8DDEGP", "W8DACTIVITYC","W8GROW","W8CMSEX","W8QMAFI")]

##collinearity 
cor_matrix2 <- cor(datasubset[,c("W1nssecfam","W2disc1YP","W6JobYP", "W6DebtattYP",
                                "W8DDEGP", "W8DACTIVITYC","W8GROW","W8CMSEX","W8QMAFI")])
cor_matrix2
vif2 <- vif(lm(W8GROW ~ ., datasubset))
vif2

#collinearity plot
corrplot(cor_matrix2)

#percetage of negative value
datasubmissing <- datasubset
sapply(datasubmissing, function(x) mean(x < 0)) * 100
nrow(datasubset)

cols <- c("W1nssecfam", "W2disc1YP","W6JobYP", "W8QMAFI")
datasubmissing[cols][datasubmissing[cols] <= 0] <- NA
datasubmissing <- na.omit(datasubmissing)
nrow(datasubmissing)
summary(datasubmissing)

##merge & changing levels' name
table(datasubmissing$W1nssecfam)
table(datasubmissing$W2disc1YP)
table(datasubmissing$W6JobYP)
table(datasubmissing$W8DACTIVITYC)
table(datasubmissing$W8QMAFI)


#W1nssecfam 
table(datasubmissing$W1nssecfam)
datasubmissing$W1nssecfam<- factor(datasubmissing$W1nssecfam)
datasubmissing$W1nssecfam<-fct_collapse(datasubmissing$W1nssecfam, other_employ = c('4','3','5','6','7'))
datasubmissing$W1nssecfam<-fct_collapse(datasubmissing$W1nssecfam, mangerial = c('1','2'))
datasubmissing$W1nssecfam<-fct_collapse(datasubmissing$W1nssecfam, W1unemploy = c('8'))



#W2disc1YP
table(datasubmissing$W2disc1YP)
datasubmissing$W2disc1YP <- factor(datasubmissing$W2disc1YP)
levels(datasubmissing$W2disc1YP)[levels(datasubmissing$W2disc1YP)%in% c("1")] <- "W2Unfairly treated because of race"
levels(datasubmissing$W2disc1YP)[levels(datasubmissing$W2disc1YP)%in% c("2")] <- "no"

#W6JobYP
table(datasubmissing$W6JobYP)
datasubmissing$W6JobYP <- factor(datasubmissing$W6JobYP)
levels(datasubmissing$W6JobYP)[levels(datasubmissing$W6JobYP)%in% c("1")] <- "W6Doingjob"
levels(datasubmissing$W6JobYP)[levels(datasubmissing$W6JobYP)%in% c("2")] <- "no"

#W8DDEGP
table(datasubmissing$W8DDEGP)
datasubmissing$W8DDEGP <- factor(datasubmissing$W8DDEGP)
levels(datasubmissing$W8DDEGP)[levels(datasubmissing$W8DDEGP)%in% c("-1")] <- "not applicable"
levels(datasubmissing$W8DDEGP)[levels(datasubmissing$W8DDEGP)%in% c("0")] <- "no degree"
levels(datasubmissing$W8DDEGP)[levels(datasubmissing$W8DDEGP)%in% c("1")] <- "first or high"

#W8SEX
table(datasubmissing$W8CMSEX)
datasubmissing$W8CMSEX<- factor(datasubmissing$W8CMSEX)
levels(datasubmissing$W8CMSEX)[levels(datasubmissing$W8CMSEX)%in% c("1")] <- "male"
levels(datasubmissing$W8CMSEX)[levels(datasubmissing$W8CMSEX)%in% c("2")] <- "female"


#W8DACTIVITYYC <- employee & unemployee
table(datasubmissing$W8DACTIVITYC)
datasubmissing$W8DACTIVITYC <- factor(datasubmissing$W8DACTIVITYC)
datasubmissing$W8DACTIVITYC<-fct_collapse(datasubmissing$W8DACTIVITYC, unemployee = c('2','3','5','6','8','9','10'))
datasubmissing$W8DACTIVITYC<-fct_collapse(datasubmissing$W8DACTIVITYC, employee = c('1'))

#W8QMAFI <- Living_comfortably & Doing alright & Other
datasubmissing$W8QMAFI <- factor(datasubmissing$W8QMAFI)
levels(datasubmissing$W8QMAFI)[levels(datasubmissing$W8QMAFI)%in% c("1")] <- "Living_comfortably"
levels(datasubmissing$W8QMAFI)[levels(datasubmissing$W8QMAFI)%in% c("2")] <- "Doing alright"
levels(datasubmissing$W8QMAFI)[levels(datasubmissing$W8QMAFI)%in% c("3","4","5")] <- "Other"
summary(datasubmissing$W8QMAFI)






#run regression
model3 <- lm(W8GROW ~  factor(W1nssecfam) + factor(W2disc1YP) + factor(W6JobYP) + W6DebtattYP + factor(W8DDEGP)
             + factor(W8DACTIVITYC) + factor(W8CMSEX) + factor(W8QMAFI) , data = datasubmissing )
summary(model3)
Anova(model3, type = "II", test.statistic = "F", term = "categorical")


#interaction 
# Create scatterplots to visualize the relationship
ggplot(datasubmissing, aes(x = factor(W8DACTIVITYC), y = W8GROW)) +
  geom_jitter(aes(color = factor(W6JobYP))) +
  facet_wrap(~factor(W6JobYP)) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(datasubmissing, aes(x = factor(W6JobYP), y = W8GROW)) +
  geom_jitter(aes(color = factor(W8DACTIVITYC))) +
  facet_wrap(~factor(W8DACTIVITYC)) +
  geom_smooth(method = "lm", se = FALSE)



p1<-ggplot(data=datasubmissing,aes(x=W6JobYP,y=W8GROW,col=W8CMSEX)) + geom_boxplot() 
p1



#run the model with interaction
model4 <- lm(W8GROW ~  factor(W1nssecfam) + factor(W2disc1YP)+ W6DebtattYP + factor(W8DDEGP)
             + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) , data = datasubmissing )
summary(model4)
Anova(model4, type = "II", test.statistic = "F", term = "categorical")
vif3 <- vif(lm(W8GROW ~ ., datasubmissing))
vif3

##delete W2disYP
model5 <- lm(W8GROW ~  factor(W1nssecfam) + W6DebtattYP + factor(W8DDEGP)
             + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) , data = datasubmissing )
summary(model5)
plot(model5)


# residual vs fitted
plot(model5, which = 1)

# cooks' distance
plot(model5, which = 4)

#residuals vs leverage
plot(model5, which = 5)

# DFFITS plot
dffits <- dffits(model5)
plot(dffits)


# combine
par(mfrow = c(2, 2))
plot(model5, which = 1)
plot(model5, which = 4)
plot(model5, which = 5)
plot(dffits)



par(mfrow = c(1, 1))

#create two vectors to contain the mse's for each iteration
in.sample.mse<-rep(NA,100)
out.sample.mse<-rep(NA,100)
#repeat 100 times
for(i in 1:100){
  cross.val<-sample(1:nrow(datasubmissing),0.7*nrow(datasubmissing), replace=FALSE)
  training.set<-datasubmissing[cross.val,]
  test.set<-datasubmissing[-cross.val,]
  #regression
  lm5train<-lm(W8GROW ~  factor(W1nssecfam) + W6DebtattYP + factor(W8DDEGP)
               + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) , data=training.set)
  #in sample prediction error
  in.sample.error=predict(lm5train,training.set)-training.set$W8GROW
  #out of sample prediction error
  out.sample.error=predict(lm5train,test.set)-test.set$W8GROW
  #in sample mse
  in.sample.mse[i]<-sum(in.sample.error^2)/length(in.sample.error)
  #out of sample mse
  out.sample.mse[i]<-sum(out.sample.error^2)/length(out.sample.error)
}
#take the means of the two
mean(out.sample.mse)

mean(in.sample.mse)

plot(model5)
predict.1<-predict(model5,datasubmissing)
plot(predict.1)

#10-fold cross validation
ctrllm5 <- trainControl(method="cv", number=10)

#train the model
fitlm5 <- train(W8GROW ~  factor(W1nssecfam) + W6DebtattYP + factor(W8DDEGP)
                + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) ,
                data=datasubmissing, method="lm", trControl=ctrllm5)

#print RMSE Rsquared and MAE
print(fitlm5)


nrow(datasubmissing)

#test outliers 
outlierTest(lm(W8GROW ~ 1, data=datasubmissing))
# calculs outliers
stats <- boxplot.stats(datasubmissing$W8GROW)$out
# identify outliers
outliers <- which(datasubmissing$W8GROW %in% stats)
# delete outliers
datafinal <- datasubmissing[-outliers, ]
nrow(datafinal)



# delete outliers
finalmodel <- lm(W8GROW ~  factor(W1nssecfam) + W6DebtattYP + factor(W8DDEGP)
                 + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) , data = datafinal)
summary(finalmodel)
plot(finalmodel)

#create two vectors to contain the mse's for each iteration
in.sample.mse.outlier<-rep(NA,100)
out.sample.mse.outlier<-rep(NA,100)
#repeat 100 times
for(i in 1:100){
  cross.val.outlier<-sample(1:nrow(datafinal),0.7*nrow(datafinal), replace=FALSE)
  training.set.outlier<-datafinal[cross.val.outlier,]
  test.set.outlier<-datafinal[-cross.val.outlier,]
  #regression
  lm6train<-lm(W8GROW ~  factor(W1nssecfam) + W6DebtattYP + factor(W8DDEGP)
               + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) , data=training.set)
  #in sample prediction error
  in.sample.error.outlier=predict(lm6train,training.set.outlier)-training.set.outlier$W8GROW
  #out of sample prediction error
  out.sample.error.outlier=predict(lm6train,test.set.outlier)-test.set.outlier$W8GROW
  #in sample mse
  in.sample.mse.outlier[i]<-sum(in.sample.error.outlier^2)/length(in.sample.error.outlier)
  #out of sample mse
  out.sample.mse.outlier[i]<-sum(out.sample.error.outlier^2)/length(out.sample.error.outlier)
}
#take the means of the two
mean(out.sample.mse.outlier)

mean(in.sample.mse.outlier)

plot(finalmodel)
predict.2<-predict(finalmodel,datafinal)

plot(predict.2)

#10-fold cross validation
ctrllm6 <- trainControl(method="cv", number=10)

#train the model
fitlm6 <- train(W8GROW ~  factor(W1nssecfam) + W6DebtattYP + factor(W8DDEGP)
                + factor(W8DACTIVITYC)*factor(W6JobYP) + factor(W8CMSEX) + factor(W8QMAFI) ,
                data=datafinal, method="lm", trControl=ctrllm6)

#print RMSE Rsquared and MAE
print(fitlm6)


par(mfrow = c(2, 2))
# residuals vs fitted
plot(finalmodel, which = 1)

# cooks' distance
plot(finalmodel, which = 4)

# residuals vs leverage
plot(finalmodel, which = 5)

# DFFITS
dffits4 <- dffits(finalmodel)
plot(dffits4)


# combine
par(mfrow = c(2, 2))
plot(finalmodel, which = 1)
plot(finalmodel, which = 4)
plot(finalmodel, which = 5)
plot(dffits4)



##############################################################################W8PUSA
rm(list=ls())
# increase view of rows in 'summary()'of regression
options(max.print=2000)
#load data
pusa.dat <-  read.csv("EOTST2112023_PUSA.csv", header = T)


#plots of all categorical variable and W8PUSA
vec <- c(1,2,29,40,42,43,50)
for ( i in 1:50 ) {
  x <- i %in% vec
  if (x == FALSE) {
    p1 <- ggplot(pusa.dat, aes(x = factor(pusa.dat[,i]), y =W8PUSA )) + geom_boxplot() + xlab(colnames(pusa.dat[i]))
    print(p1)
    
  } else {
    print(i)
    
  }
}

##check the characteristic of W8PUSA
par(mfrow = c(1, 1))
mean(pusa.dat$W8PUSA)
hist(pusa.dat$W8PUSA)
boxplot(pusa.dat$W8PUSA)
summary(boxplot(pusa.dat$W8PUSA)$stats)

# boxplot
boxplot(pusa.dat$W8PUSA)
ggplot(pusa.dat, aes(x = W8PUSA)) + geom_density()

# QQplot
qqnorm(pusa.dat$W8PUSA)

qqline(pusa.dat$W8PUSA)


# check skewness and kurtosis
skewness(pusa.dat$W8PUSA)
kurtosis(pusa.dat$W8PUSA)




# initial regression with all the predictors and non transformed dependent variable
model1 <- lm(W8PUSA ~ W1GrssyrMP + W1yschat1 + factor(W1wrk1aMP) + factor(W1condur5MP) + factor(W1hea2MP) + factor(W1NoldBroHS) + 
               factor(W1InCarHH) + 
               factor(W1hous12HH) + factor(W1usevcHH) + factor(W1hiqualmum) + factor(W1wrkfulldad) +
               factor(W1wrkfullmum) + factor(W1ch0_2HH) + factor(W1ch3_11HH) + factor(W1ch12_15HH) + factor(W1ch16_17HH) +
               factor(W1IndSchool) + factor(W1marstatmum) + factor(W1famtyp2) + factor(W1nssecfam) + factor(W1ethgrpYP) +
               factor(W1heposs9YP) + factor(W1hwndayYP) + factor(W1truantYP) + factor(W1alceverYP) + factor(W1bulrc) + factor(W1disabYP) +
               W2ghq12scr + factor(W2disc1YP) + factor(W2depressYP) + factor(W6JobYP) + factor(W6UnivYP) + factor(W6EducYP) +
               factor(W6Apprent1YP) + factor(W6acqno) + factor(W6gcse) + factor(W6als) + factor(W6OwnchiDV) + W6DebtattYP +
               factor(W8DDEGP) + W8DGHQSC + factor(W8DMARSTAT) + factor(W8DACTIVITYC) + factor(W8DWRK) + factor(W8CMSEX) + 
               factor(W8TENURE) + factor(W8QMAFI) + W8GROW + W8DAGEYCH, data = pusa.dat)
summary(model1)



#log transformation
pusa.dat$logW8PUSA <- log((pusa.dat$W8PUSA)+1)

par(mfrow = c(1, 1))

# boxplot
boxplot(pusa.dat$logW8PUSA)
ggplot(pusa.dat, aes(x = logW8PUSA)) + geom_density()

# QQplot
qqnorm(pusa.dat$logW8PUSA)
qqline(pusa.dat$logW8PUSA)

# check skewness and kurtosis
skewness(pusa.dat$logW8PUSA)
kurtosis(pusa.dat$logW8PUSA)


# initial regression after the logged dependent predictor
model2 <- lm(logW8PUSA ~ W1GrssyrMP + W1yschat1 + factor(W1wrk1aMP) + factor(W1condur5MP) + factor(W1hea2MP) + factor(W1NoldBroHS) + 
               factor(W1InCarHH) + factor(W1hous12HH) + factor(W1usevcHH) + factor(W1hiqualmum) + factor(W1wrkfulldad) +
               factor(W1wrkfullmum) + factor(W1ch0_2HH) + factor(W1ch3_11HH) + factor(W1ch12_15HH) + factor(W1ch16_17HH) +
               factor(W1IndSchool) + factor(W1marstatmum) + factor(W1famtyp2) + factor(W1nssecfam) + factor(W1ethgrpYP) +
               factor(W1heposs9YP) + factor(W1hwndayYP) + factor(W1truantYP) + factor(W1alceverYP) + factor(W1bulrc) + factor(W1disabYP) +
               W2ghq12scr + factor(W2disc1YP) + factor(W2depressYP) + factor(W6JobYP) + factor(W6UnivYP) + factor(W6EducYP) +
               factor(W6Apprent1YP) + factor(W6acqno) + factor(W6gcse) + factor(W6als) + factor(W6OwnchiDV) + W6DebtattYP +
               factor(W8DDEGP) + W8DGHQSC + factor(W8DMARSTAT) + factor(W8DACTIVITYC) + factor(W8DWRK) + factor(W8CMSEX) + 
               factor(W8TENURE) + factor(W8QMAFI) + W8GROW + W8DAGEYCH, data = pusa.dat)
summary(model2)





##Subset the dataset to only include variables with * in model2
datasub <- pusa.dat[, c("W1wrk1aMP","W2disc1YP","W6OwnchiDV","W8DDEGP","W8DGHQSC","W8DACTIVITYC","W8TENURE","W8DAGEYCH","logW8PUSA")]
head(datasub)
summary(datasub)
str(datasub)



#Collinearity
cor_matrix <- cor(datasub[,c("W1wrk1aMP","W2disc1YP","W6OwnchiDV","W8DDEGP","W8DGHQSC","W8DACTIVITYC","W8TENURE","W8DAGEYCH","logW8PUSA")])
cor_matrix
vif <- vif(lm(logW8PUSA ~ ., datasub))
vif
#collinearity plot
corrplot(cor_matrix)


##delete missing value
# Get percentage of missing values for each continuous predictor(here categorial predictors are given values so they are treated as numeric)、
datasub1 <- datasub[, c("W1wrk1aMP","W2disc1YP","W6OwnchiDV")]
datasub2 <- datasub[, c("W8DDEGP","W8DGHQSC","W8DACTIVITYC","W8TENURE","W8DAGEYCH","logW8PUSA")]

# Process sub_1
cont_vars <- sapply(datasub1, is.numeric)
cont_vars

missing_pct <- colMeans(datasub1[cont_vars] == -999 | datasub1[cont_vars] == -1.0, na.rm = TRUE) * 100
missing_pct

# Process sub_2
cont_vars <- sapply(datasub2, is.numeric)
cont_vars
missing_pct <- colMeans(datasub2[cont_vars] == -9.0 | datasub2[cont_vars] == -8.0 | datasub2[cont_vars] == -1.0, na.rm = TRUE) * 100

missing_pct

table(datasub2$W8DAGEYCH)

#Since all the  categarical predictors has less than 10% missing in sub_1, thus we delete those missing points for both categorial and numeric variables
# First, replace all occurrences of -99 and -999 with NA
datasub1[datasub1 == -999 | datasub1 == -1.0] <- NA
# Then process sub_2
# Next, change all the missing in W8QMAFI into [missing value]
#since W8DDEGP has high percentage of missing, then keep it
datasub2$W8DDEGP[datasub2$W8DDEGP == -1.0] <- 'Not_applicable'
datasub2$W8DAGEYCH[datasub2$W8DAGEYCH == -9.0 & datasub2$W8DAGEYCH== -8.0 & datasub2$W8DAGEYCH == -1.0] <- 'missing'
datasub2[datasub2 == -9.0 & datasub2 == -8.0 & datasub2 == -1.0] <- NA
datamid <- cbind(datasub1,datasub2)
datasubmissing <- na.omit(datamid)


#merge


#W8DACTIVITYYC <- employee & unemployee
datasubmissing$W8DACTIVITYC <- factor(datasubmissing$W8DACTIVITYC)
datasubmissing$W8DACTIVITYC<-fct_collapse(datasubmissing$W8DACTIVITYC, unemployee = c('2','4','5','6','8','9','10'))
datasubmissing$W8DACTIVITYC<-fct_collapse(datasubmissing$W8DACTIVITYC, employee = c('1'))



#W8TENURE <- others, rent_inc_housing and own_with_log
datasubmissing$W8TENURE <- factor(datasubmissing$W8TENURE)
datasubmissing$W8TENURE <- fct_collapse(datasubmissing$W8TENURE, others = c('1','3','5','6','7'))
datasubmissing$W8TENURE<-fct_collapse(datasubmissing$W8TENURE, rent_inc_housing = c('4'))
datasubmissing$W8TENURE<-fct_collapse(datasubmissing$W8TENURE, own_with_loan = c('2'))

#W1wrk1aMP <- others, look_after_family, part_time_employee and full_time_employee
datasubmissing$W1wrk1aMP <- factor(datasubmissing$W1wrk1aMP)
datasubmissing$W1wrk1aMP <- fct_collapse(datasubmissing$W1wrk1aMP, others = c('3','4','5','6','8','9','11','12','-99'))
datasubmissing$W1wrk1aMP <-fct_collapse(datasubmissing$W1wrk1aMP, look_after_family= c('10'))
datasubmissing$W1wrk1aMP <-fct_collapse(datasubmissing$W1wrk1aMP, part_time_employee = c('2'))
datasubmissing$W1wrk1aMP <-fct_collapse(datasubmissing$W1wrk1aMP, full_time_employee = c('1'))


#W2disc1YP <- others, unfairly and fairly
datasubmissing$W2disc1YP <- factor(datasubmissing$W2disc1YP)
datasubmissing$W2disc1YP <- fct_collapse(datasubmissing$W2disc1YP, others = c('-92','-96','-97','-99'))
datasubmissing$W2disc1YP <-fct_collapse(datasubmissing$W2disc1YP, unfairly= c('1'))
datasubmissing$W2disc1YP <-fct_collapse(datasubmissing$W2disc1YP, fairly = c('2'))



#W60wnchiDV <- others, with_child and without_child
datasubmissing$W6OwnchiDV <- factor(datasubmissing$W6OwnchiDV)
datasubmissing$W6OwnchiDV <- fct_collapse(datasubmissing$W6OwnchiDV, others = c('-92','-94'))
datasubmissing$W6OwnchiDV <-fct_collapse(datasubmissing$W6OwnchiDV,with_child= c('1'))
datasubmissing$W6OwnchiDV <-fct_collapse(datasubmissing$W6OwnchiDV, without_child = c('2'))

#W8DAGEYCH <- missing and with_child
datasubmissing$W8DAGEYCH <- factor(datasubmissing$W8DAGEYCH)
datasubmissing$W8DAGEYCH <- fct_collapse(datasubmissing$W8DAGEYCH, missing = c('-1','-8'))
datasubmissing$W8DAGEYCH <-fct_collapse(datasubmissing$W8DAGEYCH,with_child= c('0','1','2','3','4','5','6','7','8','9','10'))



#run the new model
model3 <- lm(logW8PUSA ~ factor(W1wrk1aMP) + factor(W2disc1YP) + factor(W8DACTIVITYC)+ factor(W6OwnchiDV) + factor(W8DAGEYCH) +
               W8DDEGP + W8DGHQSC + factor(W8TENURE), data = datasubmissing)
summary(model3)
Anova(model3, type = "II", test.statistic = "F", term = "categorical")

##interaction
model4 <- lm(logW8PUSA ~ factor(W1wrk1aMP) + factor(W6OwnchiDV) +  factor(W2disc1YP) + factor(W8DAGEYCH) +
               W8DDEGP + W8DGHQSC + factor(W8DACTIVITYC)*factor(W8TENURE), data = datasubmissing)
summary(model4)
Anova(model4, type = "II", test.statistic = "F", term = "categorical")



#delete W8DGHQSC, W2disc1YP from model4
model5 <- lm(logW8PUSA ~ factor(W1wrk1aMP) +  factor(W6OwnchiDV) + factor(W8DAGEYCH) +
               W8DDEGP+ factor(W8TENURE)*factor(W8DACTIVITYC), data = datasubmissing)
summary(model5)



#create two vectors to contain the mse's for each iteration
in.sample.mse<-rep(NA,100)
out.sample.mse<-rep(NA,100)
#repeat 100 times
for(i in 1:100){
  cross.val<-sample(1:nrow(datasubmissing),0.7*nrow(datasubmissing), replace=FALSE)
  training.set<-datasubmissing[cross.val,]
  test.set<-datasubmissing[-cross.val,]
  #regression
  cv.pusa.lm<- lm(logW8PUSA ~ factor(W1wrk1aMP) +  factor(W6OwnchiDV) + factor(W8DAGEYCH) +
                    W8DDEGP+ factor(W8TENURE)*factor(W8DACTIVITYC), data = training.set)
  #in sample prediction error
  in.sample.error=predict(cv.pusa.lm,training.set)-training.set$logW8PUSA
  #out of sample prediction error
  out.sample.error=predict(cv.pusa.lm,test.set)-test.set$logW8PUSA
  #in sample mse
  in.sample.mse[i]<-sum(in.sample.error^2)/length(in.sample.error)
  #out of sample mse
  out.sample.mse[i]<-sum(out.sample.error^2)/length(out.sample.error)
}
#take the means of the two
mean(out.sample.mse)
mean(in.sample.mse)

#predict plot
par(mfrow = c(2, 2))
plot(model5)
predict.1<-predict(model5,datasubmissing)
plot(predict.1)

nrow(datasubmissing)


#test outliers 
outlierTest(lm(logW8PUSA ~ 1, data=datasubmissing))
# calculs outliers
stats <- boxplot.stats(datasubmissing$logW8PUSA)$out
# identify outliers
outliers <- which(datasubmissing$logW8PUSA %in% stats)
# delete outliers
datafinal <- datasubmissing[-outliers, ]
nrow(datafinal)

par(mfrow = c(1, 1))
model6 <- lm(logW8PUSA ~ factor(W1wrk1aMP) +  factor(W6OwnchiDV) + factor(W8DAGEYCH) +
               W8DDEGP+ factor(W8TENURE)*factor(W8DACTIVITYC), data = datafinal)
summary(model6)
plot(model6)

#we can't remove outliers bc predictors turn to insignificant from the model7


# so our final model will be like:
finalmodel <- lm(logW8PUSA ~ factor(W1wrk1aMP) +  factor(W6OwnchiDV) + factor(W8DAGEYCH) +
                   W8DDEGP+ factor(W8TENURE)*factor(W8DACTIVITYC), data = datasubmissing)
summary(finalmodel)
plot(finalmodel)







