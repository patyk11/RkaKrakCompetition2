DT$score_mean <- apply(DT[,Sc_cols, with=FALSE], 1, mean, na.rm=TRUE)
DT$score_sum <- apply(DT[,Sc_cols, with=FALSE], 1, sum, na.rm=TRUE)
DT[, QuestionsCompleted]

glm( score_mean~Sc01 + Sc02 + Sc03, data = DT, family=binomial(link=probit))

questions_means <- DT[, lapply(.SD, mean, na.rm = TRUE), .SDcols = Sc_cols]
means_cumulative <- apply(questions_means, 1, function(x){cumsum(x)/seq_along(x)})

predict_score <- function(score_actual, questions_answered, questions_means = questions_means) {
  score_actual + sum(questions_means[-1:questions_answered])
}


cut <- 30
columns_cut <- Sc_cols[1:(cut)]

DT_testing <- DT[QuestionsCompleted > cut]
DT_testing$score_sum_cut <- apply(DT_testing[, columns_cut, with=FALSE], 1, sum, na.rm=TRUE)

check_method <- function(DT_testing = DT_testing, stop_moments) {
  
  for(stop in stop_moments) {
    columns_answered <- Sc_cols[1:(stop)]
    
    questions_answered <- apply(DT_testing[,columns_answered, with=FALSE], 1, sum)
    questions_mean <- apply(DT_testing[,columns_answered, with=FALSE], 1, mean)
    
    factor <- questions_mean/means_cumulative[stop]
    
    predictions <- questions_answered + factor * sum(unlist(questions_means)[1:cut][-(1:stop)])
    
    DT_testing$preds <- predictions
    
    DT_testing[, err := preds - score_sum_cut]
    
    print(sprintf('mean error for stop %s : %s', stop, DT_testing[, mean(abs(err))]))
  }
}

check_method_different_stop_times <- function(DT_fun) {
  
  DT_testing <- copy(DT_fun) # copying to avoid issues with data.table references
  
  # means for shrinked data set, second possibility: take values from not shrinked DT and save them
  questions_means_shrinked_dt <- DT_testing[, lapply(.SD, mean, na.rm = TRUE), .SDcols = Sc_cols]
  means_cumulative_shrinked_dt <- apply(questions_means_shrinked_dt, 1, function(x){cumsum(x)/seq_along(x)})
  
  score_answered_questions <- apply(DT_testing[, Sc_cols, with = FALSE], 1, sum, na.rm = TRUE)

  # determining for how many questions participant answered
  questions_answered <-  apply(DT_testing[, Sc_cols, with = FALSE], 1, function(x) sum(!is.na(x)))
  
  # means for questions
  questions_means <- score_answered_questions/questions_answered
  
  # not trivial code :D
  factors <- questions_mean/means_cumulative[questions_answered]
  
  predictions <- score_answered_questions
  
  for (it in 1:length(questions_answered)) {
    predictions[it] <- predictions[it] + factor * sum(unlist(questions_means_shrinked_dt)[-(1:questions_answered[it])])
  }
  
  # adapting to scoring environment - we should have scores for separate questions, not only final score
  
  DT_testing$preds_col <- predictions
  DT_testing[is.na(Sc48), Sc48 := preds_col]
  
  return(DT_fun)
}