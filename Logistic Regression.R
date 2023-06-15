rm(list=ls())
library(arm)
library(ggplot2)
library(dplyr)
library(car)
library(moments)
library(ggpubr)
library(GGally)

# increase view of rows in 'summary()'of regression
options(max.print=2000)

ind.dat <-  read.csv("EOTST211_2033_individual.csv", header = T)

mean(ind.dat$W8QDEB2)
hist(ind.dat$W8QDEB2)
boxplot(ind.dat$W8QDEB2)
summary(boxplot(ind.dat$W8QDEB2)$stats)


# boxplot
boxplot(ind.dat$W8QDEB2)
ggplot(ind.dat, aes(x = W8QDEB2)) + geom_density()

# QQplot
qqnorm(ind.dat$W8QDEB2)
qqline(ind.dat$W8QDEB2)

# check skewness and kurtosis
skewness(ind.dat$W8QDEB2)
kurtosis(ind.dat$W8QDEB2)


#outliers
#test outliers 
outlierTest(lm(W8QDEB2 ~ 1, data=ind.dat))
# calculs outliers
stats <- boxplot.stats(ind.dat$W8QDEB2)$out
# identify outliers
outliers <- which(ind.dat$W8QDEB2 %in% stats)
# delete outliers
data_without_outliers <- ind.dat[-outliers, ]


# Create boxplot of data with outliers
boxplot(ind.dat$W8QDEB2, main="Boxplot with Outliers")

# Create boxplot of data without outliers
boxplot(data_without_outliers$W8QDEB2, main="Boxplot without Outliers")


# Create histogram of data with outliers
hist(ind.dat$W8QDEB2, main="Histogram with Outliers")

# Create histogram of data without outliers
hist(data_without_outliers$W8QDEB2, main="Histogram without Outliers")


#determine the threshold
ggplot(ind.dat, aes(x = W8QDEB2)) +
  geom_histogram()
summary(ind.dat$W8QDEB2)
# turn hasdebt into binary
ind.dat$hasdebt <- ifelse(ind.dat$W8QDEB2 > 0, 1, 0)
summary(ind.dat)


# check skewness and kurtosis
skewness(ind.dat$hasdebt)
kurtosis(ind.dat$hasdebt)


#outliers
#test outliers 
outlierTest(lm(W8QDEB2 ~ 1, data=ind.dat))
# calculs outliers
stats <- boxplot.stats(ind.dat$W8QDEB2)$out
# identify outliers
outliers <- which(ind.dat$W8QDEB2 %in% stats)
# delete outliers
data_without_outliers <- ind.dat[-outliers, ]




#initial linear regression with all variables
model1 <- glm(hasdebt ~ W1GrssyrMP + W1yschat1 + factor(W1wrk1aMP) + factor(W1condur5MP) + factor(W1hea2MP) + factor(W1NoldBroHS) + 
           factor(W1InCarHH) + 
           factor(W1hous12HH) + factor(W1usevcHH) + factor(W1wrkfullmum) + factor(W1empsmum) + factor(W1hiqualmum) + 
           factor(W1IndSchool) + factor(W1marstatmum) + factor(W1depkids) + factor(W1famtyp2) + factor(W1nssecfam) + factor(W1ethgrpYP) +
           factor(W1heposs9YP) + factor(W1hwndayYP) + factor(W1truantYP) + factor(W1alceverYP) + factor(W1bulrc) + factor(W1disabYP) +
           W2ghq12scr + factor(W2disc1YP) + factor(W2depressYP) + factor(W6JobYP) + factor(W6UnivYP) + factor(W6acqno) +
           factor(W6gcse) + factor(W6als) + factor(W6OwnchiDV) + W6DebtattYP + factor(W8DDEGP) + W8DGHQSC + 
           factor(W8DMARSTAT) + factor(W8DACTIVITYC) + factor(W8DWRK) + factor(W8CMSEX) + factor(W8TENURE) + 
           factor(W8QMAFI) + W8DAGEYCH + W8DINCW , data = ind.dat,family = binomial)



summary(model1)
plot(model1)

# subset of siginficant ones + SEX
datasub <- ind.dat[,c("W1yschat1","W1wrk1aMP","W1truantYP","W1alceverYP", "W1disabYP","W6acqno",
                        "W6DebtattYP", "W8DDEGP","W8DGHQSC", "W8DAGEYCH","hasdebt","W8CMSEX")]
summary(datasub)
str(datasub)


# Collinearity
cor_matrix <- cor(datasub[,c("W1yschat1","W1wrk1aMP","W1truantYP","W1alceverYP", "W1disabYP","W6acqno",
                             "W6DebtattYP", "W8DDEGP","W8DGHQSC", "W8DAGEYCH","hasdebt","W8CMSEX")])
cor_matrix
vif <- vif(lm(hasdebt ~ ., datasub))
vif




#
#create a list of categorical variables 
categorical_vars <- c("W1yschat1","W1wrk1aMP","W1truantYP","W1alceverYP", "W1disabYP","W6acqno",
                      "W6DebtattYP", "W8DDEGP","W8DGHQSC","W8DAGEYCH","hasdebt","W8CMSEX")

# initialize an empty list to store the boxplots
barplots <- list()
# loop through each categorical variable and create a barplot
for (var in categorical_vars) {
  plot_data <- data.frame(categorical_variable = datasub[[var]], binary_outcome = datasub$hasdebt)
  barplot <- ggplot(plot_data, aes(x = categorical_variable, fill = binary_outcome)) +
    geom_bar() +
    ggtitle(paste(var)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate x-axis labels
  barplots[[var]] <- barplot
}

# print the barplots
barplots
ggarrange(plotlist = barplots, ncol = 3)









# dealing with the missing values
# Group 1: variables with prefix "W8"
datasub1 <- datasub[, c("W1yschat1","W1wrk1aMP","W1truantYP","W1alceverYP", "W1disabYP","W6acqno",
                        "W6DebtattYP")]
# Group 2: variables without prefix "W8"
datasub2 <- datasub[, c("W8DDEGP","W8DGHQSC", "W8DAGEYCH","W8CMSEX","hasdebt")]

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


#Since all the  categarical predictors has less than 10% missing in sub_1, thus we delete those missing points for both categorical and numeric variables
# First, replace all occurrences of -99 and -999 with NA
datasub1[datasub1 == -999 | datasub1 == -1.0] <- NA
# Then process sub_
# Next, change all the missing in W8QMAFI into [missing value]
#since W8DDEGP has high percentage of missing, then keep it
datasub2$W8DDEGP[datasub2$W8DDEGP == -1.0] <- -999
datasub2$W8DAGEYCH[datasub2$W8DAGEYCH %in% c(-9, -8, -1)] <- -999 
datasub2[datasub2 == -9 | datasub2 == -8.0 | datasub2 == -1.0] <- NA
datasub2$W8DDEGP[datasub2$W8DDEGP == -999.0] <- -1
datasub2$W8DAGEYCH[datasub2$W8DAGEYCH == -999.0] <- -1
datamid <- cbind(datasub1,datasub2)
datasub <- na.omit(datamid)




library(tidyverse)
write_csv(datasub, file = "datasubind.csv")
#merge

#W1wrk1aMP 
datasub$W1wrk1aMP <- factor(datasub$W1wrk1aMP)
datasub$W1wrk1aMP <- fct_collapse(datasub$W1wrk1aMP, others = c('3','4','5','6','7','8','9','11','12','-91','-99'))
datasub$W1wrk1aMP <-fct_collapse(datasub$W1wrk1aMP, look_after_family= c('10'))
datasub$W1wrk1aMP <-fct_collapse(datasub$W1wrk1aMP, part_time_employee = c('2'))
datasub$W1wrk1aMP <-fct_collapse(datasub$W1wrk1aMP, full_time_employee = c('1'))
summary(datasub$W1wrk1aMP)



#W1truantYP
datasub$W1truantYP <- factor(datasub$W1truantYP)
levels(datasub$W1truantYP)[levels(datasub$W1truantYP)%in% c("1")] <- "truancy_yes"
levels(datasub$W1truantYP)[levels(datasub$W1truantYP)%in% c("2")] <- "truancy_no"
levels(datasub$W1truantYP)[levels(datasub$W1truantYP)%in% c("-92","-96","-97", "-99")] <- "Other"
summary(datasub$W1truantYP)


#W1alceverYP
datasub$W1alceverYP <- factor(datasub$W1alceverYP)
levels(datasub$W1alceverYP)[levels(datasub$W1alceverYP)%in% c("1")] <- "alcoholic_drink"
levels(datasub$W1alceverYP)[levels(datasub$W1alceverYP)%in% c("2")] <- "no_alcoholic_drink"
levels(datasub$W1alceverYP)[levels(datasub$W1alceverYP)%in% c("-92","-96","-97", "-99")] <- "Other"
summary(datasub$W1alceverYP)

#W1disabYP
table(datasub$W1disabYP)
datasub$W1disabYP <- factor(datasub$W1disabYP)
levels(datasub$W1disabYP)[levels(datasub$W1disabYP)%in% c("3")] <- "No_disability"
levels(datasub$W1disabYP)[levels(datasub$W1disabYP)%in% c("1","2")] <- "Has_disability"
levels(datasub$W1disabYP)[levels(datasub$W1disabYP)%in% c("-94","-97","-99")] <- "Other"
summary(datasub$W1disabYP)



#W6acqno
datasub$W6acqno <- factor(datasub$W6acqno)
levels(datasub$W6acqno)[levels(datasub$W6acqno)%in% c("1")] <- "First_Degree"
levels(datasub$W6acqno)[levels(datasub$W6acqno)%in% c("9")] <- "No_academic_study_aim"
levels(datasub$W6acqno)[levels(datasub$W6acqno)%in% c('2','3','4','5','6','7','8')] <- "Other"
summary(datasub$W6acqno)



#W8DDEGP
table(datasub$W8DDEGP)
datasub$W8DDEGP <- factor(datasub$W8DDEGP)
levels(datasub$W8DDEGP)[levels(datasub$W8DDEGP)%in% c("-1")] <- "not applicable"
levels(datasub$W8DDEGP)[levels(datasub$W8DDEGP)%in% c("0")] <- "no degree"
levels(datasub$W8DDEGP)[levels(datasub$W8DDEGP)%in% c("1")] <- "first or high"
summary(datasub$W8DDEGP)


#W8CMSEX <- Living_comfortably & Doing alright & Other
datasub$W8CMSEX <- factor(datasub$W8CMSEX)
levels(datasub$W8CMSEX)[levels(datasub$W8CMSEX)%in% c("1")] <- "Male"
levels(datasub$W8CMSEX)[levels(datasub$W8CMSEX)%in% c("2")] <- "Female"
summary(datasub$W8CMSEX)

#W8DAGEYCH <- missing and with_child
datasub$W8DAGEYCH <- factor(datasub$W8DAGEYCH)
datasub$W8DAGEYCH <- fct_collapse(datasub$W8DAGEYCH, missing = c('-1'))
datasub$W8DAGEYCH <-fct_collapse(datasub$W8DAGEYCH,with_child= c('0','1','2','3','4','5','6','7','8','9','10','11','13'))


#model 1
model2 <- glm(hasdebt ~ W1yschat1 + factor(W1wrk1aMP) + factor(W1truantYP) + factor(W1alceverYP) + factor(W1disabYP)+factor(W6acqno)+
              W6DebtattYP + factor(W8DDEGP) +W8DGHQSC+ factor(W8CMSEX) +W8DAGEYCH, data = datasub, family = binomial)
summary(model2)




# interaction

# Create scatterplots to visualize the relationship
ggplot(datasub, aes(x = factor(W8CMSEX), y = hasdebt)) +
  geom_jitter(aes(color = W8DGHQSC)) +
  facet_wrap(~W8DGHQSC) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(datasub, aes(x = W8DGHQSC, y = hasdebt)) +
  geom_jitter(aes(color = factor(W8CMSEX))) +
  facet_wrap(~factor(W8CMSEX)) +
  geom_smooth(method = "lm", se = FALSE)






model3 <- glm(hasdebt ~ W1yschat1 + factor(W1wrk1aMP)  +
                factor(W1truantYP) + factor(W1alceverYP) + factor(W1disabYP) +
                factor(W6acqno) + W6DebtattYP + factor(W8DDEGP) + factor(W8DAGEYCH) + W8DGHQSC*factor(W8CMSEX),
              family = binomial, data = datasub)

summary(model3)
anova(model2, model3, test = "Chisq")


#delete W1NoldBroHS W1truantYP W1disabYP
model4 <- glm(hasdebt ~ W1yschat1 + factor(W1wrk1aMP) + factor(W1alceverYP)  +
                factor(W6acqno) + W6DebtattYP + factor(W8DDEGP) + factor(W8DAGEYCH) + W8DGHQSC*factor(W8CMSEX),
              family = binomial, data = datasub)
summary(model4)
plot(model4)
anova(model3, model4, test = "Chisq")

# Residuals vs Fitted plot
plot(model3, which = 1)

# Cook's Distance
cooksd <- cooks.distance(model3)
summary(cooksd)
plot(cooksd, pch = "o", cex = 1, main = "Influential Obs by Cooks distance")
abline(h = 4/nrow(datasub), col = "red")

# Residuals vs Leverage plot
plot(model3, which = 5)

threshold <- 4/nrow(datasub)
influential_obs <- which(cooksd > threshold)
print(data.frame(Observation = influential_obs, Cooks_Distance = cooksd[influential_obs]))



dffits<- dffits(model3)
plot(dffits)
n <- nrow(datasub)  
p <- length(model3$coefficients) - 1  
threshold <- 2 * sqrt((p + 1) / n)
which(abs(dffits) > threshold)


# Extract Cook's Distance and DFFITS
cooksd <- cooks.distance(model3)
dffits <- dffits(model3)

# Set thresholds
n <- nrow(datasub)  
p <- length(model3$coefficients) - 1  
cook_threshold <- 4/n 
dffits_threshold <- 2 * sqrt((p + 1) / n)

# Find influential observations
influential_cook <- which(cooksd > cook_threshold)
influential_dffits <- which(abs(dffits) > dffits_threshold)

# Identify overlapping influential observations
influential_overlap <- intersect(influential_cook, influential_dffits)

# Remove overlapping influential observations and save to datafinal
datafinal <- datasub[-influential_overlap, ]



outlierTest(model4)
datafinal <- subset(datafinal, rownames(datafinal) != "352")

finalmodel <- glm(hasdebt ~ W1yschat1  + factor(W1wrk1aMP) + factor(W1alceverYP) 
                  + factor(W6acqno) +factor(W8DDEGP) +W6DebtattYP + W8DGHQSC * factor(W8CMSEX) 
              + factor(W8DAGEYCH), 
              data = datafinal, family = binomial)
summary(finalmodel)
anova(finalmodel)

plot(finalmodel)

coefficients <- coef(finalmodel)
odds_ratios <- exp(coefficients)


result <- cbind(coefficients, odds_ratios)
rownames(result) <- names(coefficients)
colnames(result) <- c("Coefficients", "Odds Ratios")
result

#prediction
predicted <- predict(finalmodel, data=datafinal, type="response")
predicted_classes <- ifelse(predicted > 0.5, 1, 0)


# Accuracy
actual_classes <- datafinal$hasdebt
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
print(paste0("Accuracy: ", accuracy))
confusionMatrix(table(datafinal$hasdebt, predicted_classes))




