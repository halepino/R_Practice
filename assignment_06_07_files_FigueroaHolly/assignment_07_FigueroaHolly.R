# Assignment: ASSIGNMENT 7
# Name: Figueroa, Holly
# Date: 2021-05-03

## Set the working directory to the root of your DSC 520 directory
setwd("C:/DataLore/R_Projects/hello-world")
library(dplyr)

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("dsc520/data/r4ds/heights.csv")
head(heights_df,10)
colnames(heights_df)

# Fit a linear model
earn_lm <-  lm(earn ~ height + sex + ed + age + race, data=heights_df)

# View the summary of your model
summary(earn_lm)


sample_heights<-sample_n(heights_df, size = 20)
head(sample_heights)

predicted_df <- data.frame(
  earn = predict(earn_lm, sample_heights),
  ed=sample_heights$ed, race=sample_heights$race, height=sample_heights$height,
  age=sample_heights$age, sex=sample_heights$sex
  )
head(predicted_df)

## Compute deviation (i.e. residuals)
mean_earn <- mean(heights_df$earn)
## Corrected Sum of Squares Totals
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - sample_heights$earn)^2)
## Residuals
residuals <- heights_df$earn - sample_heights$earn
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared
r_squared <- ssm/sst

## Number of observations
n <- 20
## Number of regression paramaters
p <- 8
## Corrected Degrees of Freedom for Model
dfm <- p - 1
## Degrees of Freedom for Error
dfe <- n - p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n - 1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm / dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse / dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst / dft
## F Statistic
f_score <- msm / mse

## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- 1 - (1 - r_squared)*(n - 1) / (n - p)
