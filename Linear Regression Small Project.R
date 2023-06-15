library(ggplot2)
library(arm)
library(car)
rw<-read.csv("RWNS_final.csv", header = T, stringsAsFactors = TRUE)
ks4score.lm <- lm(ks4score ~ fiveac + fiveem + k3en + k3ma + k3sc + gender + SECshort + hiquamum + 
                    singlepar + house + fsm + parasp + computer + tuition + pupasp + homework + attitude + sen + truancy
                  + absent + exclude + IDACI_n + FSMband, data = rw)
display(ks4score.lm)
summary(ks4score.lm)


# this step used to convert "NA" in FSMband into "missing"
new_rw <- subset(rw, is.na(FSMband) == FALSE)
new_rw$FSMband <- factor(new_rw$FSMband, levels = c(levels(new_rw$FSMband), "missing"))
new_rw$FSMband[is.na(new_rw$FSMband)] <- "missing"



# this step used to delete all the missing values in the sample to see how much has been deleted
new_rw <- subset(new_rw, (hiquamum!='missing' & homework!='missing' & attitude!='missing'& 
                            sen != 'missing' & truancy != 'missing' & exclude != 'missing' & FSMband !='missing'))

summary(new_rw)
summary(rw)
# Here found we deleted 4507 figures, which accounts 30.1% of the initial figure (X)


# seen from the graph, the figure for [sen, truancy and exclude] is so similar, test using vif to see whether there correlation
ks4score.lm.t1 <- lm(ks4score ~ sen + truancy + exclude, data = new_rw)
vif(ks4score.lm.t1)
ks4score.lm.t2 <- lm(ks4score ~ truancy + exclude, data = new_rw)
vif(ks4score.lm.t2)
ks4score.lm.t3 <- lm(ks4score ~ sen + exclude, data = new_rw)
vif(ks4score.lm.t3)
ks4score.lm.t4 <- lm(ks4score ~ sen + truancy, data = new_rw)
vif(ks4score.lm.t4)
#obviously, no, meaning that they are not the same group of people who [sen, truancy and exclude]



# I combine the k3 score together first as high sig and no missing in them
rw$k3score <- rw$k3en + rw$k3ma + rw$k3sc
ks4score.lm <- lm(ks4score ~ k3score + gender + hiquamum + 
                    pupasp + homework + attitude + sen + truancy + exclude + FSMband, data = rw)
display(ks4score.lm)
summary(ks4score.lm)

#This time I found the sig in "sen" is poor, deleted it
rw$k3score <- rw$k3en + rw$k3ma + rw$k3sc
ks4score.lm <- lm(ks4score ~ k3score + gender + hiquamum + 
                    pupasp + homework + attitude + truancy + exclude + FSMband, data = rw)
display(ks4score.lm)
summary(ks4score.lm)

#The missing value is still high, evalute the figure


# found 1 : in 'attitude', the 'missing' value could be combined into 'low' with similar results 
# Implementation here: combine [low, very_low] <- [low]; [high, very_high] <- [high]
rw$newatt <- rw$attitude
levels(rw$newatt)[levels(rw$newatt)%in% c("low", "very_low")] <- "Low"
levels(rw$newatt)[levels(rw$newatt)%in% c("high", "very_high")] <- "High"
summary(rw$newatt)


#found 2 : in 'homework'
# Implementation here: combine [none, 1, 2，3] <- [seldom]; [4,5] <- [always]
rw$newhw <- rw$homework
levels(rw$newhw)[levels(rw$newhw)%in%c("none", "1_evening", "2_evenings", "3_evenings")] <- "seldom"
#levels(rw$newhw)[levels(rw$newhw)%in%c("2_evenings", "3_evenings")] <- "often"
levels(rw$newhw)[levels(rw$newhw)%in%c("4_evenings", "5_evenings")] <- "always"

summary(rw$newhw)




#repeat initial steps:
# this step used to convert "NA" in FSMband into "missing"
new_rw <- subset(rw, is.na(FSMband) == FALSE)
new_rw$FSMband <- factor(new_rw$FSMband, levels = c(levels(new_rw$FSMband), "missing"))
new_rw$FSMband[is.na(new_rw$FSMband)] <- "missing"


# this step used to delete all the missing values in the sample to see how much has been deleted
new_rw <- subset(new_rw, (hiquamum!='missing' & FSMband !='missing' & truancy !='missing' & exclude !='missing'))

summary(new_rw)

#Here we deleted 1063 figures, which accounts for 7.9% (V)


# run regression again
ks4score.lm <- lm(ks4score ~ k3score + gender + hiquamum + 
                    pupasp + newhw + newatt + truancy + exclude + FSMband, data = new_rw)
display(ks4score.lm)
summary(ks4score.lm)

# found high and FSM less relevant, delete them

# first here we don't delete missing values at all in truancy and exclude
ks4score.lm <- lm(ks4score ~ k3score + gender +  
                    pupasp + newhw + truancy + exclude + newatt, data = rw)
display(ks4score.lm)
summary(ks4score.lm)
plot(ks4score.lm)

#second we delete the missing value in truancy and exclude
new_rw <- subset(new_rw, (truancy !='missing' & exclude !='missing'))
# which is not suitable as we delelte over 2000 figures here
ks4score.lm <- lm(ks4score ~ k3score + gender +  
                    pupasp + newhw + truancy + exclude + newatt, data = new_rw)
display(ks4score.lm)
summary(ks4score.lm)

#we also face the problem of overfitting
vif(ks4score.lm)

# we have to delete one variable with large missing value as its more bised, choose truancy
rw <- subset(rw, (truancy !='missing' & newatt !='missing' & newhw != 'missing'))
ks4score.lm <- lm(ks4score ~ k3score + gender +  
                    pupasp + newhw + truancy+ newatt, data = rw)
display(ks4score.lm)
summary(ks4score.lm)
plot(ks4score.lm)








