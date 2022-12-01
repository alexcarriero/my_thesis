# data generation --------------------------------------------------------------

generate_data <- function(inn){
  
  npred   = inn[[1]]
  ev      = inn[[2]]
  n.level = inn[[3]]
  n       = inn[[4]]
  mu0     = inn[[5]]
  mu1     = inn[[6]]
  sigma0  = inn[[7]]
  sigma1  = inn[[8]]
  
  # positive class
  n1      <- rbinom(1, n, ev)
  class_1 <- mvrnorm(n1, mu1, sigma1)
  
  # negative class
  n0      <- n - n1
  class_0 <- mvrnorm(n0, mu0, sigma0)
  
  outcome <- c(rep(1, n1), rep(0, n0))
  
  # format data frame
  df <- cbind(rbind(class_1, class_0), outcome) %>% 
    as.data.frame() %>% 
    mutate(outcome = as.factor(outcome))
  
  return(df)
}



generate_data_test <- function(inn){
  
  npred   = inn[[1]]
  ev      = inn[[2]]
  n.level = inn[[3]]
  n       = inn[[4]]
  mu0     = inn[[5]]
  mu1     = inn[[6]]
  sigma0  = inn[[7]]
  sigma1  = inn[[8]]
  
  # positive class
  n1      <- rbinom(1, n, ev)
  class_1 <- mvrnorm(n1, mu1, sigma1)
  
  # negative class
  n0      <- n - n1
  class_0 <- mvrnorm(n0, mu0, sigma0)
  
  outcome <- c(rep(1, n1), rep(0, n0))
  
  # format data frame
  df <- cbind(rbind(class_1, class_0), outcome) %>% 
    as.data.frame() %>% 
    mutate(outcome = as.factor(outcome))
  
  return(df)
}

# model implementation ---------------------------------------------------------

lrg <- function(df, test){
  
  mod  <- glm(outcome ~ ., family = "binomial", data = df)
  pred <- predict(mod,  newdata = test, type = "response")
  return(pred)
}



svc <- function(df, test){
  
  mod  <- svm(x = subset(df, select = -outcome), y = df$outcome, probability = T)
  pred <- predict(mod, newdata = subset(test, select = -outcome), probability = T)
  pred <- attr(pred, "probabilities")[,1]
  return(pred)
}



rnf <- function(df, test){
  
  mod  <- randomForest(outcome~., data = df) 
  pred <- predict(mod,  newdata = test, type = "prob")[,2]
  return(pred)
}



xgb <- function(df, test){
  
  train_x <- model.matrix(outcome ~ ., df)[,-1]
  train_y <- as.numeric(df$outcome) - 1
  xgb     <- xgboost(data = train_x,
                     label = train_y, 
                     max.depth = 10,
                     eta = 1,
                     nthread = 4,
                     nrounds = 4,
                     objective = "binary:logistic",
                     verbose = 2)
  pred <- predict(xgb, newdata = model.matrix(outcome~., test)[,-1])
  return(pred)
}



rub <- function(df, test){
  mod  <- rus(outcome ~., size = 10, alg = "svm", data = df)
  pred <- predict(mod, newdata = test)
}



pred_probs <- function(df, test){
  
  a <- tibble(
    class = test$outcome, 
    lrg   = lrg(df, test), 
    svm   = svc(df, test), 
    rnf   = rnf(df, test), 
    xgb   = xgb(df, test), 
    rub   = rub(df, test)
  )
  return(a)
}


# performance metrics ----------------------------------------------------------

brier_score <- function(probs, outcome){
  outcome <- as.numeric(outcome) - 1
  mean((probs - outcome)^2)
}


get_brier_score <- function(tibble){
  vec = apply(tibble[-1], 2, brier_score, outcome = tibble$class)
  return(vec)
}


get_auc <- function(tibble){
  vec = apply(tibble[-1], 2, pROC::auc, response = tibble$class)
  return(vec)
}

get_stats <- function(auc, brier){
  
  out <- 
    rbind(
      auc_mean   = apply(auc, 2, mean),
      auc_sd     = apply(auc, 2, sd),
      brier_mean = apply(brier, 2, mean),
      brier_sd   = apply(brier, 2, sd)
    )
  
  colnames(out) <- c("lrg", "svc", "rnf", "xgb", "rub")
  return(out)
}


