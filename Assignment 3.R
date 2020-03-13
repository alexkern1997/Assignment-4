# Load the proper libraries and set the right working directory
setwd("D:/Documenten/Artificial Intelligence Master/Semester 1/Experimentation in Psychology and Linguistics/Assignment 4")
library(readr)
library(tidyverse)
library(rcompanion)
library(gridExtra)
# Load in the dataset
results_df <- read_csv("experiment_data.csv")
results_df$quant <- as.factor(results_df$quant)
results_df$pred <- as.factor(results_df$pred)

# Clean the data further
# First, remove outliers in readTimes using filter
results_df <- results_df %>% filter(between(readTimes, 50, 3001))
# Second, also remove readers that deviate from the average reading times of subjects
df <- aggregate(readTimes ~ subj, results_df, mean)
SD <- sd(df$readTimes)
M <- mean(df$readTimes)
HIGH <- M + 3*SD 
df <- df %>% filter(readTimes < HIGH)
results_df <- results_df %>% filter(subj %in%  df$subj)
# Thirdly check if the readTimes is actually normally distributed
results_df <- results_df %>% mutate(sqrtRT = sqrt(readTimes))
results_df <- results_df %>% mutate(logRT = log(readTimes))
p1 <- ggplot(results_df, aes(x = readTimes)) + geom_histogram()
p2 <- ggplot(results_df, aes(x = sqrtRT)) + geom_histogram()
p3 <- ggplot(results_df, aes(x = logRT)) + geom_histogram()
grid.arrange(p1, p2, p3, nrow = 1)

# Run a sanity check
results_df <- results_df %>% mutate(lenWords = nchar(word))
df <- aggregate(logRT ~ lenWords, results_df, mean)
p4 <- ggplot(df, aes(x = lenWords, y = logRT)) + 
  geom_line()
p4
df <- aggregate(logRT ~ wordNo, results_df, mean)
p5 <- ggplot(df, aes(x = wordNo, y = logRT)) + 
  geom_line()
p5

# Only select the experimental words of data
results_df <- results_df %>% filter(itemtype == "experiment")

# The analysis is done using a two-way ANOVA (http://www.sthda.com/english/wiki/two-way-anova-test-in-r)

# First check if the cells are equal in sample size, by making a freq table.
table(results_df$quant, results_df$pred)

# Then visualize the difference in readingTimes for the 4 cells
ggplot(results_df, aes(x = quant, y = logRT, color = pred))+
  geom_boxplot() + 
  scale_fill_brewer(palette = "Blues")

# Two-way interaction plot
interaction.plot(x.factor = results_df$pred, trace.factor = results_df$quant, 
                 response = results_df$logRT, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Predicative", ylab="Log Reaction Time",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

# Do a two anova analysis and check for an interaction effect
res.aov <- aov(logRT ~ quant * pred, data = results_df)
summary(res.aov)

# Creating a barplot to show mean and sd
plot_df <- aggregate(logRT ~ quant * pred, results_df, mean)
df2 <- aggregate(logRT ~ quant * pred, results_df, sd)
plot_df$sd <- df2$logRT

ggplot(plot_df, aes(x = quant, y = logRT, fill = pred)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=logRT-sd, ymax=logRT+sd), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00')) +
  labs(title = "Average log Reading time per Quantifier", x = 'Quantifiers', y = 'Log Transformed Reading Time')
