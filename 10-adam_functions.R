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
  factors <- questions_means/means_cumulative_shrinked_dt[questions_answered]
  
  predictions <- numeric(length(score_answered_questions)) # vector of zeros
  
  for (it in 1:length(questions_answered)) {
    predictions[it] <- factors[it] * sum(unlist(questions_means_shrinked_dt)[-(1:questions_answered[it])])
  }
  
  # adapting to scoring environment - we should have scores for separate questions, not only final score
  
  DT_testing$preds_col <- predictions
  
  DT_testing[, Sc48 := as.numeric(Sc48)]
  DT_testing[is.na(Sc48), Sc48 := preds_col]
  for(col in Sc_cols[Sc_cols != 'Sc48']) {
    setnames(DT_testing, old = col, new = 'temporary_name')
    DT_testing[is.na(temporary_name), temporary_name := 0]
    setnames(DT_testing, new = col, old = 'temporary_name')
  }
  return(list(DT_testing = DT_testing, questions_answered = questions_answered))
}