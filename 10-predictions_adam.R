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