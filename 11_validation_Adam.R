### Please use pacman for loading packages
### see https://cran.r-project.org/web/packages/pacman/vignettes/Introduction_to_pacman.html
library(pacman)
p_load(dplyr, readxl, data.table)
### VALIDATION ###

## DO NOT CHANGE THIS SECTION OF THE CODE
## It's preparing data for validation
# Note that we have provided an xlsx with a small data set (10 persons), to demonstrate how this will work.
THRESHOLD = 1200

source('01_dataload.R')
validationSet = copy(DT)

# for comparison..
questions_answered_before_shrink <- apply(DT[, Sc_cols, with = FALSE], 1, function(x) sum(!is.na(x)))
validationScores = validationSet[["Score"]]

create_validation_set = function()
{
  valDuration     = validationSet %>% select(matches("Du[0-9][0-9]"))
  valDuration     = apply(valDuration, 1, cumsum) %>% t
  valLastQuestion = apply(valDuration > THRESHOLD, 1, function(x) which(x)[1])
  valLastQuestion[is.na(valLastQuestion)] = ncol(valDuration)
  
  for(i in seq_along(valLastQuestion))
  {
    valLQ = valLastQuestion[i]
    if(valLQ == ncol(valDuration)) next; # skip when someone solved all questions
    valLQ = sprintf("An%02i",valLQ + 1)
    valLQ = which(colnames(validationSet) == valLQ):ncol(validationSet) # tu jest dużo kolumn
    validationSet[i,valLQ] = NA
  }
  validationSet
}

validationSet = create_validation_set()

### VALIDATE MODEL (THIS PART IS WHERE YOU CAN CHANGE!)

# This is Super Naive R Model: If they do not finish within 1200 seconds, they
# will not score more. This ignores all the other possible parameters of a real
# model, such as error analysis, time for an item, probability of future results
# based on earlier scores and times, etc.

# backuping validation set
if(!dir.exists('data'))
  dir.create('data')

# changing scores for answers = NA

for(it in 1:48) {
  if(it < 10)
    Sc_col <- sprintf('Sc0%s', it) else
      Sc_col <- sprintf('Sc%s', it)
    
  Sd_col <- gsub(pattern = 'Sc', replacement = 'An', Sc_col)
  
  validationSet[is.na(get(Sc_col)), Sd_col := NA, with = FALSE]
}

saveRDS(validationSet, 'data/validationSet.RDS')

source('10-adam_functions.R')
output <- check_method_different_stop_times(validationSet)
validationSet <- output$DT_testing
questions_answered <- output$questions_answered
# THIS IS THE PART WHICH CALCULATES IF YOU ARE THE WINNER
# Calculate predicted score:
predictedScores = rowSums(validationSet %>% select(matches("Sc[0-9][0-9]")))

SSE = sum((predictedScores - validationScores)^2)
print(SSE) # THE SMALL THIS NUMBER IS, THE MORE CHANCE OF WINNING.
