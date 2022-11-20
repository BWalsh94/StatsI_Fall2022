Incumbent_data <- read.csv("incumbents_subset.csv")

# Question 1 #

# Q1, Part 1:
difflog_voteshare_regression <- lm(voteshare ~ difflog, data = Incumbent_data)

# Q1, Part 2:
plot(Incumbent_data$difflog, Incumbent_data$voteshare, 
     xlab = 'x = voteshare (in blue)', ylab = 'y = difflog (in orange)',
     main = 'voteshare (y) regressed on difflog (x)', 
     pch = 18, cex = 1, type = "p", 
     col=c("steelblue", "orange"),)
abline(difflog_voteshare_regression, col = 'darkgreen', lwd = 3, lty = 7)

# Q1, Part 3: 
Q1residuals <- resid(difflog_voteshare_regression)

# Q1, Part 4: 
summary(difflog_voteshare_regression)


# Question 2 #

# Q2, Part 1: 
presvote_difflog_regression <- lm(presvote ~ difflog, data = Incumbent_data)
presvote_difflog_regression
summary(presvote_difflog_regression)

# Q2, Part 2:
plot(Incumbent_data$difflog, Incumbent_data$presvote, 
     xlab = 'x = difflog (red)', ylab = 'y = presvote (gold)',
     main = 'presvote (y) regressed on difflog (x)', 
     pch = 18, cex = 1, type = "p",
     col = c("darkred", "gold"),)

abline(presvote_difflog_regression, col = 'aquamarine4', lwd = 3, lty = 7)

# Q2, Part 3:
Q2residuals <- residuals(presvote_difflog_regression)

# Q2, Part 4: 
summary(presvote_difflog_regression)


# Question 3 # 

# Q3, Part 1:
voteshare_presvote_regression <- lm(voteshare ~ presvote, data = Incumbent_data)

# Q3, Part 2:
plot(Incumbent_data$presvote, Incumbent_data$voteshare, 
     xlab = 'x = presvote (green)', ylab = 'y = voteshare (orange)',
     main = 'voteshare (y) regressed on presvote (x)', 
     pch = 18, cex = 1, type = "p",
     col = c("seagreen1", "lightsalmon"),)

abline(voteshare_presvote_regression, col = 'slategray4', lwd = 3, lty = 7)

#Q3, Part 3:
summary(voteshare_presvote_regression)


# Question 4 # 

# Q4, Part 1:
Q1residuals_regressed_on_Q2residuals <- lm(Q1residuals ~ Q2residuals)
summary(Q1residuals_regressed_on_Q2residuals)

# Q4, Part 2:
plot(Q2residuals, Q1residuals,
     xlab = 'Question 2 residuals (blue)', ylab = 'Question 1 residuals (purple)',
     main = 'Question 1 residuals (y) regressed on Question 2 residuals (x)',
     pch = 18, cex = 1, type = 'p',
     col = c("cornflowerblue", "blueviolet"),)
abline(Q1residuals_regressed_on_Q2residuals, col = 'gray20', lwd = 3, lty = 7)

# Question 5 #

# Q5, Part 1:
voteshare_regressed_on_difflog_and_presvote<- lm(voteshare ~ difflog + presvote, data = Incumbent_data)

# Q5, Part 2:
summary(voteshare_regressed_on_difflog_and_presvote)
