
# Fix parameters for first run 
effect_size <- 0.05
effect_ate <- 0.5 + effect_size
pop <- 100000
sample_size <- 20

# Create a true_population of 50/50 split
true_population <- c(rep(0, pop * (1/2)), rep(1, pop * (1/2)))
true_ATE <- mean(true_population)
  
#Rely on simulated CLT to get the distribution of the ATE
sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)))
p_value = mean(effect_ate >= sharp.null.hypothesis)
par(mfrow=c(1,1))
hist(sharp.null.hypothesis, n = 20, main = paste('sample: ', sample_size, ' effect: ', effect_ate, ' p :', p_value))
abline(v = effect_ate, col = "blue")

#Simulate p-value for each sample size 
par(mfrow=c(2,4))
effect_size <- 0.05
effect_ate <- 0.5 + effect_size
sample_size <- c(10, 20, 50, 100, 200, 500, 1000, 2000)                                   
for (s in sample_size) {
  sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, s)))
  p_value = mean(effect_ate >= sharp.null.hypothesis)
  plot(density(sharp.null.hypothesis),  main = paste('samp: ', s, ' eff: ', effect_ate, ' p :', p_value), cex.main= 0.9)
  abline(v = effect_ate, col = "blue")
  #hist(sharp.null.hypothesis, n = 20)
}

#simulate p_value for each effect size
par(mfrow=c(2,3))
effect_ate <- c(0.55, 0.54, 0.53, 0.52, 0.51)
sample_size <- 500
for (e in effect_ate) {
  sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, sample_size)))
  p_value = mean(e >= sharp.null.hypothesis)
  plot(density(sharp.null.hypothesis),  main = paste('s: ', sample_size, ' e: ', e, 'p :', p_value), cex.main= 1)
  abline(v = e, col = "blue")
  #hist(sharp.null.hypothesis, n = 20)
}

#Compiling a complete table
summary_table <- data.frame()
sample_size <- c(10, 20, 50, 100, 200, 500, 1000, 2000)
effect_ate <- c(0.55, 0.54, 0.53, 0.52, 0.51)
for (s in sample_size) {
  p_values <- c(s)
  for (e in effect_ate) {
    sharp.null.hypothesis <- replicate(10000, mean(sample(true_population, s)))
    p_value <- mean(e >= sharp.null.hypothesis)
    p_values <- c(p_values, p_value)
  }
  summary_table <- rbind(summary_table, p_values)
}

row.names(summary_table) <- summary_table[, 1]
summary_table <- summary_table[, -1]
colnames(summary_table) <- effect_ate
summary_table

#Create a summary table 
setwd('~/Downloads')
d <- read.csv('BeautyAds_July 6, 2017_17.33.csv')
