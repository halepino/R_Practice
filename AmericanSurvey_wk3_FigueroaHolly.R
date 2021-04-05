# Assignment: Week 3 American Community Survey Exercise
# Name: Figueroa, Holly
# Date: 2021-04-01

# Load ggplot Package
library(ggplot2)
theme_set(theme_minimal())

# Set working directory
setwd('C:/DataLore/R_Projects')

# Load Data File
Community_df<- read.csv('wk3_survey_data.csv')
head(Community_df)

# Provide outputs
str(Community_df)

nrow(Community_df)
     
ncol(Community_df)

# Create histogram for HSDegree variable with labels
ggplot(Community_df, aes(HSDegree)) + 
  geom_histogram(bins = 10)+
  ggtitle("HS Degree Holders Accross Communities") +
  xlab("High School Degree Holders (%)") +
  ylab("Frequency of Percentage")


ggplot(Community_df, aes(sample=HSDegree)) + stat_qq() + stat_qq_line()

stat.desc(Community_df$HSDegree, basic = FALSE, norm = TRUE)

