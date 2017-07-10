library(dplyr)
library(ggplot2)

raw_data <- read.csv('BeautyAds_July 6, 2017_17.33.csv')

data <- raw_data[3:63,]

data <- data %>% 
  filter(Finished == 'True', Status == 'IP Address', Welcome == 'I agree', Group != '')

recode <- function(field){
  out <- 0
  if(field == 'Agree'){
    out <- 1
  } else if(field == 'Strongly agree'){
    out <- 2
  } else if(field == 'Disagree'){
    out <- -1
  } else if(field == 'Strongly disagree'){
    out <- -2
  }
  out <- as.numeric(out)
  return(out)
}


data$Beautiful <- sapply(data$Personal_Views_Beautiful, FUN =  recode)
data$Confident <- sapply(data$Personal_Views_Confident, FUN =  recode)
data$Importance <- sapply(data$Personal_Views_Beauty_Importance, FUN =  recode)
data$Relate <- sapply(data$Personal_Views_Relate_To_Model, FUN =  recode)

treat <- filter(data, Group == 'Treatment')
control <- filter(data, Group == 'Control')

ate_Beautiful <- mean(treat$Beautiful) - mean(control$Beautiful)
ate_Confident <- mean(treat$Confident) - mean(control$Confident)
ate_Importance <- mean(treat$Importance) - mean(control$Importance)
ate_Relate <- mean(treat$Relate) - mean(control$Relate)

n <- 1e4
copy <- data
taus <- data.frame(matrix(NA, nrow = n, ncol = 4))
names(taus) <- c('Beauty','Confidence','Importance','Relate')
for(i in 1:n){
  copy$Group <- sample(data$Group)
  treat <- filter(copy, Group == 'Treatment')
  control <- filter(copy, Group == 'Control')
  
  taus[i,1] <- mean(treat$Beautiful) - mean(control$Beautiful)
  taus[i,2] <- mean(treat$Confident) - mean(control$Confident)
  taus[i,3] <- mean(treat$Importance) - mean(control$Importance)
  taus[i,4] <- mean(treat$Relate) - mean(control$Relate)
}

p_beauty <- sum(taus$Beauty > ate_Beautiful)/n
p_confident <- sum(taus$Confidence > ate_Confident)/n
p_relate <- sum(taus$Relate > ate_Relate)/n
p_importance <- sum(taus$Importance > ate_Importance)/n

qplot(taus$Beauty, bins = 30, main = paste('RI for Beauty Pvalue =', p_beauty)) + geom_vline(xintercept = ate_Beautiful)
qplot(taus$Confidence, bins = 30, main = paste('RI for Confidence Pvalue =',p_confident)) + geom_vline(xintercept = ate_Confident)
qplot(taus$Importance, bins = 30, main = paste('RI for Importance Pvalue =',p_importance)) + geom_vline(xintercept = ate_Importance)
qplot(taus$Relate, bins = 30, main = paste('RI for Relating Pvalue =',p_relate)) + geom_vline(xintercept = ate_Relate)


