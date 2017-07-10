
####################################################
####### Theoritical Sample Size Calculator #########
####################################################

# Fix parameters for first run 
effect_size <- 0.05
effect_ate <- 0.5 + effect_size
pop <- 100000
sample_size <- 20

# Create a true_population of 50/50 split
true_population <- c(rep(0, pop * (1/2)), rep(1, pop * (1/2)))
true_ATE <- mean(true_population)
  
# Rely on simulated CLT to get the distribution of the ATE
sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)))
p_value = mean(effect_ate <= sharp.null.hypothesis)
par(mfrow=c(1,1))
hist(sharp.null.hypothesis, n = 20, main = paste('sample: ', sample_size, ' effect: ', effect_ate, ' p :', p_value))
abline(v = effect_ate, col = "blue")

# Simulate p-value for each sample size 
par(mfrow=c(2,4))
effect_size <- 0.05
effect_ate <- 0.5 + effect_size
sample_size <- c(10, 20, 50, 100, 200, 500, 1000, 2000)                                   
for (s in sample_size) {
  sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, s)))
  p_value = mean(effect_ate <= sharp.null.hypothesis)
  plot(density(sharp.null.hypothesis),  main = paste('samp: ', s, ' eff: ', effect_ate, ' p :', p_value), cex.main= 0.9)
  abline(v = effect_ate, col = "blue")
  #hist(sharp.null.hypothesis, n = 20)
}

# Simulate p_value for each effect size
par(mfrow=c(2,3))
effect_ate <- c(0.55, 0.54, 0.53, 0.52, 0.51)
sample_size <- 500
for (e in effect_ate) {
  sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)))
  p_value = mean(e <= sharp.null.hypothesis)
  plot(density(sharp.null.hypothesis),  main = paste('s: ', sample_size, ' e: ', e, 'p :', p_value), cex.main= 1)
  abline(v = e, col = "blue")
  #hist(sharp.null.hypothesis, n = 20)
}

# Compiling a complete table
summary_table <- data.frame()
sample_size <- c(10, 20, 50, 100, 200, 500, 1000, 2000)
effect_ate <- c(0.55, 0.54, 0.53, 0.52, 0.51)
for (s in sample_size) {
  p_values <- c(s)
  for (e in effect_ate) {
    sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, s)))
    p_value <- mean(e <= sharp.null.hypothesis)
    p_values <- c(p_values, p_value)
  }
  summary_table <- rbind(summary_table, p_values)
}

row.names(summary_table) <- summary_table[, 1]
summary_table <- summary_table[, -1]
colnames(summary_table) <- effect_ate
summary_table

####################################################
############### First results ATE ##################
####################################################

library(dplyr)
library(ggplot2)

# Create a summary table 
setwd('/Users/ozimmer/GoogleDrive/berkeley/w241/BeatuyAd_CausalExperiment')
d <- read.csv('BeautyAds_July 6, 2017_17.33.csv')

# Filter out irrelavant entries
d <- d[-c(1,2),]
d <- d[d$Status == 'IP Address',] #Remove survey preview 
d <- d[d$Welcome == 'I agree',] #Remove users who didn't agree to participate
d <- d[d$Finished == 'True',] #Remove users who didn't finisn the survey
d <- d[d$Group %in% c('Treatment', 'Control'),]

# Recoding of values
recode_values <- function(d, column){
  d[[column]] <- as.character(d[[column]])
  d[[column]] <- recode(d[[column]], 'Strongly disagree' = -2, 
                                       'Disagree' = -1, 'Agree' = 1, 'Strongly agree' = 2, 
                                       .missing = 0, .default = 0)
}

columns_to_analyse <- c('Personal_Views_Confident', 'Personal_Views_Beautiful', 
                       'Personal_Views_Beauty_Importance', 'Personal_Views_Relate_To_Model')
for (column in columns_to_analyse){
  d[[column]] <- recode_values(d, column)
}

# Correct column names misspellings
d <- rename(d, Coffee_validate_1 = Coffe_validate_1, 
            Fit_i_identify_1 = FIt_i_identify_1, 
            Work_i_identify_2 = work_i_identify_2)

# Combine and recode randomization 1 & 2 for the images
images <- c('Passion', 'Coffee', 'Couple', 'Work', 'Fit')
questions <- c('_i_identify_', '_i_prefer_', '_o_prefer_', '_validate_')
randomization <- c('1', '2')

for (image in images){
  for (question in questions){
    column1 <- paste(image, question, '1', sep = "")
    d[[column1]] <- ifelse(d[[column1]] == 'Ad 1', 2, ifelse(d[[column1]] == 'Ad 2', 1, 0))
    column2 <- paste(image, question, '2', sep = "")
    d[[column2]] <- ifelse(d[[column2]] == 'Ad 2', 2, ifelse(d[[column2]] == 'Ad 1', 1, 0))
    new_column <- paste(image, question, sep ="")
    d[[new_column]] <- d[[column1]] + d[[column2]] - 1
    d[[new_column]] <- ifelse(d[[new_column]] == -1, NA, d[[new_column]])
  }
}

######## Getting the ATE for TEXT questions #########

get_ATE <- function(d, column){
  return(mean(d[d$Group == 'Treatment',][[column]], na.rm = TRUE)- mean(d[d$Group == 'Control',][[column]], na.rm = TRUE))
}
for (column in columns_to_analyse){
  print(column)
  print(get_ATE(d, column))
}

# How likely is it to get this answer by chance only?
column <- 'Personal_Views_Confident'
pop <- 100000
sample_size <- 30

#Re-shuffle the sample of 30, 10000 to get the distribution

true_population <- c(rep(-2, pop * (1/4)), rep(-1, pop * (1/4)), rep(1, pop * (1/4)), rep(2, pop * (1/4)))
mean(true_population)
# Rely on simulated CLT to get the distribution of the ATE
sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)))

par(mfrow=c(2,2))
for (column in columns_to_analyse){
  ATE <- get_ATE(d, column)
  p_value <- mean(ATE <= sharp.null.hypothesis)
  print(column)
  print(ATE)
  print(p_value)
  plot(density(sharp.null.hypothesis),  main = paste('Samp: ', sample_size, ' ATE: ', round(ATE, 3), 
                                                     'p-value :', round(p_value, 3)), cex.main= 0.8,
       xlab=column)
  abline(v = ATE, col = "blue")
}

par(mfrow=c(2,2))
sample_sizes <- c(10, 20, 30, 50, 100, 200, 500, 1000, 2000)
for (column in columns_to_analyse){
  p_values <- vector(mode="numeric", length=0)
  for (sample_size in sample_sizes){
    sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)))
    ATE <- get_ATE(d, column)
    p_value <- mean(ATE <= sharp.null.hypothesis)
    p_values <- c(p_values, p_value)
  }
  plot(sample_sizes, p_values, type ='l', main = 'Sample to reach significancy', cex.main= 0.8, xlab=column)
  abline(h = 0.05, col = "blue")
}

######## Getting the ATE for IMAGES questions #########

images <- c('Passion', 'Coffee', 'Couple', 'Work', 'Fit')
questions <- c('_i_identify_', '_i_prefer_', '_o_prefer_', '_validate_')

column <- 'Passion_i_identify_'
pop <- 100000
sample_size <- 400

true_population <- c(rep(0, pop * (1/2)), rep(1, pop * (1/2)))
mean(true_population)
# Rely on simulated CLT to get the distribution of the ATE
sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)) - mean(sample(true_population, sample_size)))

par(mfrow=c(1,1))
plot(density(sharp.null.hypothesis),  main = paste('Sample_size: ', sample_size), cex.main= 0.8)

for (image in images){
  for (question in questions){
    column <- paste(image, question, sep = "")
    ATE <- get_ATE(d, column)
    p_value <- mean(abs(ATE) <= sharp.null.hypothesis)
    print(column)
    print(paste('ATE:', ATE))
    print(paste('p-value: ', p_value, ifelse(p_value < 0.05, '***', ''), sep = ""))
    
    if (p_value < 0.05){
      abline(v = abs(ATE), col = "blue")
    } else {
      abline(v = abs(ATE), col = "red")
    }
    percentage_similar <- sum(d[[column]], na.rm = TRUE)/nrow(d[!is.na(d[[column]]),])
    print(paste('percentage_similar: ', round(percentage_similar,2)))
  }
  print('======================================')
}

#Doing the analysis for all images per user - instead of one by one

#With n=30 - none of the results are significant except for couple_o_prefer -> but this results can be 
# considered as a fishing expedition results... 

#Divergence of opinion


######## Running the formula for sample size ########
#https://www.isixsigma.com/tools-templates/sampling-data/how-determine-sample-size-determining-sample-size/
z = 1.96
E = 0.05 #For a 5% effect size 

for (image in images){
  for (question in questions){
    column <- paste(image, question, sep = "")
    s_d <- sd(d[[column]], na.rm = TRUE)
    n <- (z * s_d / E)**2 
    print(column)
    print(paste('sample_size :', round(n, 0)))
  }
}


