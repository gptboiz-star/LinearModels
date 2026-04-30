data <- read.csv("C:\\Users\\txs230053\\Downloads\\ap_hiCleaned.csv")

hist(data$ap_lo)
data$age <- data$age / 365

boxplot(data$height)

continuous_vars <- c("age", "height", "weight", "ap_hi", "ap_lo")

data_scaled <- data

data_scaled[continuous_vars] <- scale(data_scaled[continuous_vars])

fit_scaled <- glm(cardio ~., data = data_scaled, family = binomial)
summary(fit_scaled)

# Predicted probabilities
prob <- predict(fit_scaled, type = "response")

# Predicted classes using 0.5 cutoff
pred <- ifelse(prob >= 0.5, 1, 0)

cost_FP <- 1   # cost of Type I error
cost_FN <- 2   # cost of Type II error

thresholds <- seq(0.01, 0.99, by = 0.01)

#------------------------------------------
thresholds <- seq(0.01, 0.99, by = 0.01)

results <- data.frame(
  threshold = thresholds,
  FPR = NA,
  FNR = NA,
  total_error_rate = NA,
  balanced_error = NA
)

for (i in seq_along(thresholds)) {
  pred <- ifelse(prob >= thresholds[i], 1, 0)
  
  cm <- table(factor(pred, levels = c(0,1)),
              factor(data_scaled$cardio, levels = c(0,1)))
  
  FP <- cm["1","0"]
  FN <- cm["0","1"]
  TN <- cm["0","0"]
  TP <- cm["1","1"]
  
  FPR <- FP / (FP + TN)
  FNR <- FN / (FN + TP)
  
  results$FPR[i] <- FPR
  results$FNR[i] <- FNR
  
  # average of both error rates
  results$total_error_rate[i] <- FPR + FNR
  results$balanced_error[i] <- (FPR + FNR) / 2
}

best <- results[which.min(results$balanced_error), ]
best

optimal_threshold <- best$threshold

pred_optimal <- ifelse(prob >= optimal_threshold, 1, 0)

table(Predicted = pred_optimal, Actual = data_scaled$cardio)
#------------------------------------------


results <- data.frame(
  threshold = thresholds,
  FP = NA,
  FN = NA,
  total_cost = NA
)

for (i in seq_along(thresholds)) {
  pred <- ifelse(prob >= thresholds[i], 1, 0)
  cm <- table(factor(pred, levels = c(0,1)),
              factor(data_scaled$cardio, levels = c(0,1)))
  
  FP <- cm["1","0"]
  FN <- cm["0","1"]
  
  results$FP[i] <- FP
  results$FN[i] <- FN
  results$total_cost[i] <- cost_FP * FP + cost_FN * FN
}

best <- results[which.min(results$total_cost), ]
best

optimal_threshold <- best$threshold
pred_optimal <- ifelse(prob >= optimal_threshold, 1, 0)

table(Predicted = pred_optimal, Actual = data_scaled$cardio)
