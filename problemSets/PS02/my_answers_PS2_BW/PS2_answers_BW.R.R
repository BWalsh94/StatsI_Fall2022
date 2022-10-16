foUpper1 <- 14
foUpper2 <- 6
foUpper3 <- 7
foLower1 <- 7
foLower2 <- 7
foLower3 <- 1

Column1 <- sum(foUpper1, foLower1)
Column2 <- sum(foUpper2, foLower2)
Column3 <- sum(foUpper3, foLower3)

Row1 <- sum(foUpper1, foUpper2, foUpper3)
Row2 <- sum(foLower1, foLower2, foLower3)

Grand_Total <- sum(Row1, Row2)

f1e <- (Row1 / Grand_Total) * Column1
f2e <- (Row1 / Grand_Total) * Column2
f3e <- (Row1 / Grand_Total) * Column3
f4e <- (Row2 / Grand_Total) * Column1
f5e <- (Row2 / Grand_Total) * Column2
f6e <- (Row2 / Grand_Total) * Column3

x2_test_statistic <- ((((foUpper1 - f1e)^2)/f1e) + (((foUpper2 - f2e)^2)/f2e) + (((foUpper3 - f3e)^2)/f3e)
                      + (((foLower1 - f4e)^2)/f4e) + (((foLower2 - f5e)^2)/f5e) + (((foLower3 - f6e)^2)/f6e))

x2_test_statistic                      

P_Value <- pchisq(x2_test_statistic, df = 2, lower.tail = F)
P_Value <- as.numeric(format(round(P_Value, 2), nsmall = 2))
P_Value

Stan_Residual1 <- (foUpper1 - f1e) / 
  sqrt(f1e*(1 - (Row1/Grand_Total))*(1- (Column1/Grand_Total)))
Stan_Residual2 <- (foUpper2 - f2e) / 
  sqrt(f2e*(1 - (Row1/Grand_Total))*(1- (Column2/Grand_Total)))
Stan_Residual3 <- (foUpper3 - f3e) / 
  sqrt(f3e*(1 - (Row1/Grand_Total))*(1- (Column3/Grand_Total)))
Stan_Residual4 <- (foLower1 - f4e) / 
  sqrt(f4e*(1 - (Row2/Grand_Total))*(1- (Column1/Grand_Total)))
Stan_Residual5 <- (foLower2 - f5e) / 
  sqrt(f5e*(1 - (Row2/Grand_Total))*(1- (Column2/Grand_Total)))
Stan_Residual6 <- (foLower3 - f6e) / 
  sqrt(f6e*(1 - (Row2/Grand_Total))*(1- (Column3/Grand_Total)))


Indian_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header = T)
Indian_data

Regression_analysis_water_and_reserved <- lm(formula = water~reserved, data=Indian_data)
Regression_analysis_water_and_reserved

summary(Regression_analysis_water_and_reserved)

